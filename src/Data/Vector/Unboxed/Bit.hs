{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE BangPatterns               #-}

module Data.Vector.Unboxed.Bit
     ( wordSize
     , wordLength
     , fromWords
     , toWords
     , indexWord

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

     , reverse

     , first
     ) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bit.Internal
import           Data.Bits
import qualified Data.List                          as L
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Vector.Generic                as V
import qualified Data.Vector.Generic.Mutable        as MV
import           Data.Vector.Unboxed                as U
    hiding (and, or, any, all, reverse, findIndex)
import qualified Data.Vector.Unboxed                as Unsafe
import qualified Data.Vector.Unboxed.Mutable.Bit    as B
import           Data.Vector.Unboxed.Bit.Internal
import           Data.Word
import           Prelude                            as P
    hiding (and, or, any, all, reverse)

wordLength :: U.Vector Bit -> Int
wordLength = nWords . U.length

-- |Given a vector of words, concatenate them to a vector of bits (interpreting the words in little-endian order, as described at 'indexWord').
fromWords :: U.Vector Word -> U.Vector Bit
fromWords ws = BitVec 0 (nBits (V.length ws)) ws

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
        xs1 <- V.thaw xs
        B.zipInPlace op xs1 ys
        Unsafe.unsafeFreeze xs1

-- |(internal) N-ary 'zipWords' with specified output length.  Makes all kinds of assumptions; mainly only valid for union and intersection.
{-# INLINE zipMany #-}
zipMany :: (Word -> Word -> Word) -> NE.NonEmpty (U.Vector Bit) -> U.Vector Bit
zipMany _ (v NE.:| []) = v
zipMany op (v NE.:| vs) = runST $ do
    let n = L.foldl' P.min (U.length v) (P.map U.length vs)
    w <- V.thaw (V.slice 0 n v)
    P.mapM_ (B.zipInPlace op w) vs
    Unsafe.unsafeFreeze w

union :: Vector Bit -> Vector Bit -> Vector Bit
union = zipWords (.|.)

intersection :: Vector Bit -> Vector Bit -> Vector Bit
intersection = zipWords (.&.)

difference :: Vector Bit -> Vector Bit -> Vector Bit
difference = zipWords diff

symDiff :: Vector Bit -> Vector Bit -> Vector Bit
symDiff = zipWords xor

unions :: NE.NonEmpty (U.Vector Bit) -> U.Vector Bit
unions = zipMany (.|.)

intersections :: NE.NonEmpty (U.Vector Bit) -> U.Vector Bit
intersections = zipMany (.&.)

-- |Flip every bit in the given vector
invert :: U.Vector Bit -> U.Vector Bit
invert xs = runST $ do
    ys <- MV.new (V.length xs)
    let f i _ = complement (indexWord xs i)
    B.mapInPlaceWithIndex f ys
    Unsafe.unsafeFreeze ys

-- | Given a vector of bits and a vector of things, extract those things for which the corresponding bit is set.
--
-- For example, @select (V.map (Bit . p) x) x == V.filter p x@.
select :: (V.Vector v1 Bit, V.Vector v2 t) => v1 Bit -> v2 t -> [t]
select is xs = L.unfoldr next 0
    where
        n = min (V.length is) (V.length xs)

        next j
            | j >= n           = Nothing
            | unBit (is V.! j) = Just (xs V.! j, j + 1)
            | otherwise        = next           (j + 1)

-- | Given a vector of bits and a vector of things, extract those things for which the corresponding bit is unset.
--
-- For example, @exclude (V.map (Bit . p) x) x == V.filter (not . p) x@.
exclude :: (V.Vector v1 Bit, V.Vector v2 t) => v1 Bit -> v2 t -> [t]
exclude is xs = L.unfoldr next 0
    where
        n = min (V.length is) (V.length xs)

        next j
            | j >= n           = Nothing
            | unBit (is V.! j) = next           (j + 1)
            | otherwise        = Just (xs V.! j, j + 1)

selectBits :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
selectBits is xs = runST $ do
    xs1 <- U.thaw xs
    n <- B.selectBitsInPlace is xs1
    Unsafe.unsafeFreeze (MV.take n xs1)

excludeBits :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
excludeBits is xs = runST $ do
    xs1 <- U.thaw xs
    n <- B.excludeBitsInPlace is xs1
    Unsafe.unsafeFreeze (MV.take n xs1)

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
        !ff | unBit b   = ffs
            | otherwise = ffs . complement

        loop !i
            | i >= n    = Nothing
            | otherwise = fmap (i +) (ff (indexWord xs i)) `mplus` loop (i + wordSize)
