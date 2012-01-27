{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE BangPatterns               #-}
module Data.Vector.Unboxed.Mutable.Bit
     ( module Data.Bit
     , module U
     
     , wordSize
     , fromWords
     , toWords
     , readWordM
     , writeWord
     
     , mapInPlaceWithIndex
     , mapInPlace
     , zipInPlace
     
     , unionInPlace
     , intersectionInPlace
     , differenceInPlace
     , symDiffInPlace
     , invertInPlace
     , selectBitsInPlace
     , excludeBitsInPlace
     
     , countBitsM
     
     , andM
     , orM
     
     , any
     , anyBitsM
     , all
     , allBitsM
     
     , reverseInPlace
     ) where

import           Control.Monad
import           Control.Monad.Primitive
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

unionInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
unionInPlace = zipInPlace (.|.)

intersectionInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
intersectionInPlace = zipInPlace (.&.)

differenceInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
differenceInPlace = zipInPlace diff

symDiffInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
symDiffInPlace = zipInPlace xor

-- |Flip every bit in the given vector
invertInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> m ()
invertInPlace = mapInPlace (return . complement)

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
