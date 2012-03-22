{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#else
#define safe
#endif
module Data.Vector.Unboxed.Bit
     ( module Data.Bit
     , module U
     
     , wordSize
     , wordLength
     , fromWords
     , toWords
     , indexWord
     
     , pad
     , padWith
     
     , zipWords
     
     , union
     , unions
     
     , intersection
     , intersections
     , difference
     , symDiff
     
     , invert
     
     , select
     , selectBits
     
     , exclude
     , excludeBits
     
     , countBits
     , listBits
     
     , and
     , or
     
     , any
     , anyBits
     , all
     , allBits
     
     , reverse
     
     , first
     , findIndex
     ) where

import safe           Control.Monad
import                Control.Monad.ST
import safe           Data.Bit
import safe           Data.Bit.Internal
import safe           Data.Bits
import safe qualified Data.List                          as L
import safe qualified Data.Vector.Generic.Safe           as V
import safe qualified Data.Vector.Generic.Mutable.Safe   as MV
import safe           Data.Vector.Unboxed.Safe           as U
    hiding (and, or, any, all, reverse, findIndex)
import      qualified Data.Vector.Unboxed                as Unsafe
import safe qualified Data.Vector.Unboxed.Mutable.Bit    as B
import                Data.Vector.Unboxed.Bit.Internal
import safe           Data.Word
import safe           Prelude                            as P
    hiding (and, or, any, all, reverse)

wordLength :: U.Vector Bit -> Int
wordLength = nWords . U.length

-- |Given a number of bits and a vector of words, concatenate them to a vector of bits (interpreting the words in little-endian order, as described at 'indexWord').  If there are not enough words for the number of bits requested, the vector will be zero-padded.
fromWords :: Int -> U.Vector Word -> U.Vector Bit
fromWords n ws
    | n <= m    = BitVec 0 n (V.take (nWords n) ws)
    | otherwise = pad n (BitVec 0 m ws)
    where 
         m = nBits (V.length ws)

-- |Given a vector of bits, extract an unboxed vector of words.  If the bits don't completely fill the words, the last word will be zero-padded.
toWords :: U.Vector Bit -> U.Vector Word
toWords v@(BitVec s n ws)
    | aligned s && (aligned n || isMasked (modWordSize n) (ws V.! divWordSize n))
         = V.slice (divWordSize s) (nWords n) ws
    | otherwise = runST (Unsafe.unsafeThaw v >>= cloneWords >>= Unsafe.unsafeFreeze)

-- | @zipWords f xs ys@ = @fromWords (min (length xs) (length ys)) (zipWith f (toWords xs) (toWords ys))@
{-# INLINE zipWords #-}
zipWords :: (Word -> Word -> Word) -> U.Vector Bit -> U.Vector Bit -> U.Vector Bit
zipWords op xs ys
    | V.length xs > V.length ys =
        zipWords (flip op) ys xs
    | otherwise =  runST $ do
        -- TODO: eliminate this extra traversal
        xs <- V.thaw xs
        B.zipInPlace op xs ys
        Unsafe.unsafeFreeze xs

-- |(internal) N-ary 'zipWords' with specified output length.  Makes all kinds of assumptions; mainly only valid for union and intersection.
{-# INLINE zipMany #-}
zipMany :: Word -> (Word -> Word -> Word) -> Int -> [U.Vector Bit] -> U.Vector Bit
zipMany z op n xss = runST $ do
    ys <- MV.new n
    B.mapInPlace (const z) ys
    P.mapM_ (B.zipInPlace op ys) xss
    Unsafe.unsafeFreeze ys

union        = zipWords (.|.)
intersection = zipWords (.&.)
difference   = zipWords diff
symDiff      = zipWords xor

unions :: Int -> [U.Vector Bit] -> U.Vector Bit
unions = zipMany 0 (.|.)

intersections :: Int -> [U.Vector Bit] -> U.Vector Bit
intersections = zipMany (complement 0) (.&.)

-- |Flip every bit in the given vector
invert :: U.Vector Bit -> U.Vector Bit
invert xs = runST $ do
    ys <- MV.new (V.length xs)
    let f i _ = complement (indexWord xs i)
    B.mapInPlaceWithIndex f ys
    Unsafe.unsafeFreeze ys

-- | Given a vector of bits and a vector of things, extract those things for which the corresponding bit is set.
-- 
-- For example, @select (V.map (fromBool . p) x) x == V.filter p x@.
select :: (V.Vector v1 Bit, V.Vector v2 t) => v1 Bit -> v2 t -> [t]
select is xs = L.unfoldr next 0
    where
        n = min (V.length is) (V.length xs)
        
        next j
            | j >= n             = Nothing
            | toBool (is V.! j)  = Just (xs V.! j, j + 1)
            | otherwise          = next           (j + 1)

-- | Given a vector of bits and a vector of things, extract those things for which the corresponding bit is unset.
-- 
-- For example, @exclude (V.map (fromBool . p) x) x == V.filter (not . p) x@.
exclude :: (V.Vector v1 Bit, V.Vector v2 t) => v1 Bit -> v2 t -> [t]
exclude is xs = L.unfoldr next 0
    where
        n = min (V.length is) (V.length xs)
        
        next j
            | j >= n             = Nothing
            | toBool (is V.! j)  = next           (j + 1)
            | otherwise          = Just (xs V.! j, j + 1)

selectBits :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
selectBits is xs = runST $ do
    xs <- U.thaw xs
    n <- B.selectBitsInPlace is xs
    Unsafe.unsafeFreeze (MV.take n xs)

excludeBits :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
excludeBits is xs = runST $ do
    xs <- U.thaw xs
    n <- B.excludeBitsInPlace is xs
    Unsafe.unsafeFreeze (MV.take n xs)

-- |return the number of ones in a bit vector
countBits :: U.Vector Bit -> Int
countBits v = loop 0 0
    where
        !n = alignUp (V.length v)
        loop !s !i
            | i >= n    = s
            | otherwise = loop (s + popCount (indexWord v i)) (i + wordSize)

listBits :: U.Vector Bit -> [Int]
listBits v = loop id 0
    where
        !n = V.length v
        loop bs !i
            | i >= n    = bs []
            | otherwise = 
                loop (bs . bitsInWord i (indexWord v i)) (i + wordSize)

-- | 'True' if all bits in the vector are set
and :: U.Vector Bit -> Bool
and v = loop 0
    where
        !n = V.length v
        loop !i
            | i >= n    = True
            | otherwise = (indexWord v i == mask (n-i))
                        && loop (i + wordSize)

-- | 'True' if any bit in the vector is set
or :: U.Vector Bit -> Bool
or v = loop 0
    where
        !n = V.length v
        loop !i
            | i >= n    = False
            | otherwise = (indexWord v i /= 0)
                        || loop (i + wordSize)

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

reverse :: U.Vector Bit -> U.Vector Bit
reverse xs = runST $ do
    let !n = V.length xs
        f i _ = reversePartialWord (n - i) (indexWord xs (max 0 (n - i - wordSize)))
    ys <- MV.new n
    B.mapInPlaceWithIndex f ys
    Unsafe.unsafeFreeze ys

-- |Return the address of the first bit in the vector with the specified value, if any
first :: Bit -> U.Vector Bit -> Maybe Int
first b xs = mfilter (< n) (loop 0)
    where
        !n = V.length xs
        !ff | toBool b  = ffs
            | otherwise = ffs . complement
        
        loop !i
            | i >= n    = Nothing
            | otherwise = fmap (i +) (ff (indexWord xs i)) `mplus` loop (i + wordSize)

findIndex p xs = case (p 0, p 1) of
    (False, False) -> Nothing
    (False,  True) -> first 1 xs
    (True,  False) -> first 0 xs
    (True,   True) -> if V.null xs then Nothing else Just 0
