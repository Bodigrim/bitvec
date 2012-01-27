{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE BangPatterns               #-}
module Data.Vector.Unboxed.Bit
     ( module Data.Bit
     , module U
     
     , wordSize
     , fromWords
     , toWords
     , readWord
     , readWordM
     , writeWord
     
     , mapInPlaceWithIndex
     , mapInPlace
     , zipInPlace
     , bitwiseZip
     
     , union
     , unionInPlace
     , unions
     
     , intersection
     , intersectionInPlace
     , intersections
     
     , difference
     , differenceInPlace
     
     , symDiff
     , symDiffInPlace
     
     , invert
     , invertInPlace
     
     , select
     , selectBits
     , selectBitsInPlace
     
     , exclude
     , excludeBits
     , excludeBitsInPlace
     
     , countBits
     , countBitsM
     
     , and
     , andM
     , or
     , orM
     
     , any
     , anyBits
     , anyBitsM
     , all
     , allBits
     , allBitsM
     
     , reverse
     , reverseInPlace
     
     , first
     , findIndex
     ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bit
import           Data.Bit.Internal
import           Data.Bits
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as MV
import           Data.Vector.Unboxed         as U hiding (and, or, any, all, reverse, findIndex)
import           Data.Vector.Unboxed.Bit.Instance
import           Data.Word
import           Prelude                     as P hiding (and, or, any, all, reverse)


-- TODO: make names more consistent, especially use of "M" in map/zip/etc functions.
-- Consider splitting part of this module into a "dense IntSet" interface.

-- |Map a function over a bit vector one 'Word' at a time ('wordSize' bits at a time).  The function will be passed the bit index (which will always be 'wordSize'-aligned) and the current value of the corresponding word.  The returned word will be written back to the vector.  If there is a partial word at the end of the vector, it will be zero-padded when passed to the function and truncated when the result is written back to the array.
{-# INLINE mapInPlaceWithIndex #-}
mapInPlaceWithIndex ::
    PrimMonad m =>
        (Int -> Word -> m Word)
     -> U.MVector (PrimState m) Bit -> m ()
mapInPlaceWithIndex f xs = loop 0
    where
        !n = MV.length xs
        loop !i
            | i >= n    = return ()
            | otherwise = do
                x <- readWordM xs i
                writeWord xs i =<< f i x
                loop (i + wordSize)

-- |Same as 'mapInPlaceWithIndex' but without the index.
{-# INLINE mapInPlace #-}
mapInPlace :: PrimMonad m => (Word -> m Word) -> U.MVector (PrimState m) Bit -> m ()
mapInPlace f = mapInPlaceWithIndex (const f)

{-# INLINE zipInPlace #-}
zipInPlace :: PrimMonad m => (Word -> Word -> Word) -> U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
zipInPlace f xs ys =
    mapInPlaceWithIndex g (MV.basicUnsafeSlice 0 (min n m) xs)
    where 
        n = MV.length xs
        m =  V.length ys
        g i x = return (f x (readWord ys i))

-- |For a bitwise operation @op@ (whose 1-bit equivalent is @op'@),
-- @bitwiseZip op xs ys == V.fromList (zipWith op' (V.toList xs) (V.toList ys))@, and the former is computed much more quickly.
{-# INLINE bitwiseZip #-}
bitwiseZip :: (Word -> Word -> Word) -> U.Vector Bit -> U.Vector Bit -> U.Vector Bit
bitwiseZip op xs ys
    | V.length xs < V.length ys =
        bitwiseZip (flip op) ys xs
    | otherwise =  runST $ do
        ys <- V.thaw ys
        let f i y = return (readWord xs i `op` y)
        mapInPlaceWithIndex f ys
        V.unsafeFreeze ys

-- |(internal) N-ary 'bitwiseZip' with a unit value and specified output length.  The first input is assumed to be a unit of the operation (on both sides).
{-# INLINE zipMany #-}
zipMany :: Word -> (Word -> Word -> Word) -> Int -> [U.Vector Bit] -> U.Vector Bit
zipMany z op n xss = runST $ do
    ys <- MV.new n
    mapInPlace (return . const z) ys
    P.mapM_ (zipInPlace op ys) xss
    V.unsafeFreeze ys

union = bitwiseZip (.|.)
unionInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
unionInPlace = zipInPlace (.|.)
unions = zipMany 0 (.|.)

intersection = bitwiseZip (.&.)
intersectionInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
intersectionInPlace = zipInPlace (.&.)
intersections = zipMany (complement 0) (.&.)

diff :: Word -> Word -> Word
diff w1 w2 = w1 .&. complement w2

difference = bitwiseZip diff
differenceInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
differenceInPlace = zipInPlace diff

symDiff      = bitwiseZip xor
symDiffInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
symDiffInPlace = zipInPlace xor

-- |Flip every bit in the given vector
invert :: U.Vector Bit -> U.Vector Bit
invert xs = runST $ do
    ys <- MV.new (V.length xs)
    let f i _ = return (complement (readWord xs i))
    mapInPlaceWithIndex f ys
    V.unsafeFreeze ys

-- |Flip every bit in the given vector
invertInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> m ()
invertInPlace = mapInPlace (return . complement)

-- | Given a vector of bits and a vector of things, extract those things for which the corresponding bit is set.
-- 
-- For example, @select (V.map (fromBool . p) x) x == V.filter p x@.
select :: (V.Vector v1 Bit, V.Vector v2 t) => v1 Bit -> v2 t -> v2 t
select is xs = V.unfoldr next 0
    where
        n = min (V.length is) (V.length xs)
        
        next j
            | j >= n             = Nothing
            | toBool (is V.! j)  = Just (xs V.! j, j + 1)
            | otherwise          = next           (j + 1)

-- | Given a vector of bits and a vector of things, extract those things for which the corresponding bit is unset.
-- 
-- For example, @exclude (V.map (fromBool . p) x) x == V.filter (not . p) x@.
exclude :: (V.Vector v1 Bit, V.Vector v2 t) => v1 Bit -> v2 t -> v2 t
exclude is xs = V.unfoldr next 0
    where
        n = min (V.length is) (V.length xs)
        
        next j
            | j >= n             = Nothing
            | toBool (is V.! j)  = next           (j + 1)
            | otherwise          = Just (xs V.! j, j + 1)

selectBitsInPlace :: PrimMonad m => U.Vector Bit -> U.MVector (PrimState m) Bit -> m Int
selectBitsInPlace is xs = loop 0 0
    where
        !n = min (V.length is) (MV.length xs)
        loop !i !ct
            | i >= n    = return ct
            | otherwise = do
                x <- readWordM xs i
                let !(nSet, x') = selectWord (masked (n - i) (readWord is i)) x
                writeWord xs ct x'
                loop (i + wordSize) (ct + nSet)

excludeBitsInPlace :: PrimMonad m => U.Vector Bit -> U.MVector (PrimState m) Bit -> m Int
excludeBitsInPlace is xs = loop 0 0
    where
        !n = min (V.length is) (MV.length xs)
        loop !i !ct
            | i >= n    = return ct
            | otherwise = do
                x <- readWordM xs i
                let !(nSet, x') = selectWord (masked (n - i) (complement (readWord is i))) x
                writeWord xs ct x'
                loop (i + wordSize) (ct + nSet)

selectBits :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
selectBits is xs = runST $ do
    xs <- U.thaw xs
    n <- selectBitsInPlace is xs
    U.unsafeFreeze (MV.take n xs)

excludeBits :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
excludeBits is xs = runST $ do
    xs <- U.thaw xs
    n <- excludeBitsInPlace is xs
    U.unsafeFreeze (MV.take n xs)

-- |return the number of ones in a bit vector
countBits :: U.Vector Bit -> Int
countBits v = loop 0 0
    where
        !n = alignUp (V.length v)
        loop !s !i
            | i >= n    = s
            | otherwise = loop (s + popCount (readWord v i)) (i + wordSize)

-- |return the number of ones in a bit vector
countBitsM :: PrimMonad m => U.MVector (PrimState m) Bit -> m Int
countBitsM v = loop 0 0
    where
        !n = alignUp (MV.length v)
        loop !s !i
            | i >= n    = return s
            | otherwise = do
                x <- readWordM v i
                loop (s + popCount x) (i + wordSize)

-- | 'True' if all bits in the vector are set
and :: U.Vector Bit -> Bool
and v = loop 0
    where
        !n = V.length v
        loop !i
            | i >= n    = True
            | otherwise = (readWord v i == mask (n-i))
                        && loop (i + wordSize)

-- | Returns 'True' if all bits in the vector are set
andM :: PrimMonad m => U.MVector (PrimState m) Bit -> m Bool
andM v = loop 0
    where
        !n = MV.length v
        loop !i
            | i >= n    = return True
            | otherwise = do
                y <- readWordM v i
                if y == mask (n - i)
                    then loop (i + wordSize)
                    else return False

-- | 'True' if any bit in the vector is set
or :: U.Vector Bit -> Bool
or v = loop 0
    where
        !n = V.length v
        loop !i
            | i >= n    = False
            | otherwise = (readWord v i /= 0)
                        || loop (i + wordSize)

-- | Returns 'True' if any bit in the vector is set
orM :: PrimMonad m => U.MVector (PrimState m) Bit -> m Bool
orM v = loop 0
    where
        !n = MV.length v
        loop !i
            | i >= n    = return False
            | otherwise = do
                y <- readWordM v i
                if y /= 0
                    then return True
                    else loop (i + wordSize)

all p = case (p 0, p 1) of
    (False, False) -> U.null
    (False,  True) -> allBits 1
    (True,  False) -> allBits 0
    (True,   True) -> flip seq True

any p = case (p 0, p 1) of
    (False, False) -> flip seq False
    (False,  True) -> anyBits 1
    (True,  False) -> anyBits 0
    (True,   True) -> not . U.null

allBits, anyBits :: Bit -> U.Vector Bit -> Bool
allBits 0 = not . or
allBits 1 = and

anyBits 0 = not . and
anyBits 1 = or

allBitsM, anyBitsM :: PrimMonad m => Bit -> U.MVector (PrimState m) Bit -> m Bool
allBitsM 0 = liftM not . orM
allBitsM 1 = andM

anyBitsM 0 = liftM not . andM
anyBitsM 1 = orM

reverse :: U.Vector Bit -> U.Vector Bit
reverse xs = runST $ do
    let !n = V.length xs
        f i _ = return (reversePartialWord (n - i) (readWord xs (max 0 (n - i - wordSize))))
    ys <- MV.new n
    mapInPlaceWithIndex f ys
    V.unsafeFreeze ys

reverseInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> m ()
reverseInPlace xs = loop 0 (MV.length xs)
    where
        loop !i !j
            | i' <= j'  = do
                x <- readWordM xs i
                y <- readWordM xs j'
                
                writeWord xs i  (reverseWord y)
                writeWord xs j' (reverseWord x)
                
                loop i' j'
            | i' < j    = do
                let w = (j - i) `shiftR` 1
                    k  = j - w
                x <- readWordM xs i
                y <- readWordM xs k
                
                writeWord xs i (meld w (reversePartialWord w y) x)
                writeWord xs k (meld w (reversePartialWord w x) y)
                
                loop i' j'
            | i  < j    = do
                let w = j - i
                x <- readWordM xs i
                writeWord xs i (meld w (reversePartialWord w x) x)
            | otherwise = return ()
            where    
                !i' = i + wordSize
                !j' = j - wordSize

-- |Return the address of the first bit in the vector with the specified value, if any
first :: Bit -> U.Vector Bit -> Maybe Int
first b xs = mfilter (< n) (loop 0)
    where
        !n = V.length xs
        !ff | toBool b  = ffs
            | otherwise = ffs . complement
        
        loop !i
            | i >= n    = Nothing
            | otherwise = fmap (i +) (ff (readWord xs i)) `mplus` loop (i + wordSize)

findIndex p xs = case (p 0, p 1) of
    (False, False) -> Nothing
    (False,  True) -> first 1 xs
    (True,  False) -> first 0 xs
    (True,   True) -> if V.null xs then Nothing else Just 0
