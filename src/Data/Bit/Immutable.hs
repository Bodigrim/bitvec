{-# LANGUAGE CPP              #-}

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

#ifndef BITVEC_THREADSAFE
module Data.Bit.Immutable
#else
module Data.Bit.ImmutableTS
#endif
     ( castFromWords
     , castToWords
     , cloneToWords

     , zipBits

     , selectBits
     , excludeBits
     , bitIndex
     ) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
#ifndef BITVEC_THREADSAFE
import           Data.Bit.Internal
import qualified Data.Bit.Mutable                   as B
#else
import           Data.Bit.InternalTS
import qualified Data.Bit.MutableTS                   as B
#endif
import           Data.Bit.Utils
import qualified Data.Vector.Generic.Mutable       as MV
import qualified Data.Vector.Generic               as V
import qualified Data.Vector.Primitive             as P
import           Data.Vector.Unboxed                as U
    hiding (and, or, any, all, reverse, findIndex)
import qualified Data.Vector.Unboxed                as Unsafe
import           Data.Word
import           Prelude                           as P
    hiding (and, or, any, all, reverse)
import Unsafe.Coerce

-- | Cast a vector of words to a vector of bits.
-- Cf. 'Data.Bit.castFromWordsM'.
--
-- >>> castFromWords (Data.Vector.Unboxed.singleton 123)
-- [1,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
castFromWords
    :: U.Vector Word
    -> U.Vector Bit
castFromWords ws = BitVec (mulWordSize off) (mulWordSize len) arr
    where
        P.Vector off len arr = unsafeCoerce ws

-- | Try to cast a vector of bits to a vector of words.
-- It succeeds if a vector of bits is aligned.
-- Use 'cloneToWords' otherwise.
-- Cf. 'Data.Bit.castToWordsM'.
--
-- prop> castToWords (castFromWords v) == Just v
castToWords
    :: U.Vector Bit
    -> Maybe (U.Vector Word)
castToWords (BitVec s n ws)
    | aligned s
    , aligned n
    = Just $ unsafeCoerce $ P.Vector (divWordSize s) (divWordSize n) ws
    | otherwise
    = Nothing

-- | Clone a vector of bits to a new unboxed vector of words.
-- If the bits don't completely fill the words, the last word will be zero-padded.
-- Cf. 'Data.Bit.cloneToWordsM'.
--
-- >>> cloneToWords (read "[1,1,0,1,1,1,1,0]")
-- [123]
cloneToWords
    :: U.Vector Bit
    -> U.Vector Word
cloneToWords v@(BitVec _ n _) = runST $ do
    ws <- MV.new (nWords n)
    let loop !i !j
            | i >= n    = return ()
            | otherwise = do
                MV.write ws j (indexWord v i)
                loop (i + wordSize) (j + 1)
    loop 0 0
    V.unsafeFreeze ws
{-# INLINE cloneToWords #-}

-- | Zip two vectors with the given function.
-- Similar to 'Data.Vector.Unboxed.zipWith', but much faster.
--
-- >>> import Data.Bits
-- >>> zipBits (.&.) (read "[1,1,0]") (read "[0,1,1]") -- intersection
-- [0,1,0]
-- >>> zipBits (.|.) (read "[1,1,0]") (read "[0,1,1]") -- union
-- [1,1,1]
-- >>> zipBits (\x y -> x .&. complement y) (read "[1,1,0]") (read "[0,1,1]") -- difference
-- [1,0,0]
-- >>> zipBits xor (read "[1,1,0]") (read "[0,1,1]") -- symmetric difference
-- [1,0,1]
zipBits
    :: (forall a. Bits a => a -> a -> a)
    -> U.Vector Bit
    -> U.Vector Bit
    -> U.Vector Bit
zipBits f xs ys
    | U.length xs >= U.length ys = zs
    | otherwise = U.slice 0 (U.length xs) zs
    where
        zs = U.modify (B.zipInPlace f xs) ys

-- | For each set bit of the first argument, deposit
-- the corresponding bit of the second argument
-- to the result. Similar to the parallel deposit instruction (PDEP).
--
-- >>> selectBits (read "[0,1,0,1,1]") (read "[1,1,0,0,1]")
-- [1,0,1]
--
-- Here is a reference (but slow) implementation:
--
-- > import qualified Data.Vector.Unboxed as U
-- > selectBits mask ws == U.map snd (U.filter (unBit . fst) (U.zip mask ws))
selectBits :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
selectBits is xs = runST $ do
    xs1 <- U.thaw xs
    n <- B.selectBitsInPlace is xs1
    Unsafe.unsafeFreeze (MV.take n xs1)

-- | For each unset bit of the first argument, deposit
-- the corresponding bit of the second argument
-- to the result.
--
-- >>> excludeBits (read "[0,1,0,1,1]") (read "[1,1,0,0,1]")
-- [1,0]
--
-- Here is a reference (but slow) implementation:
--
-- > import qualified Data.Vector.Unboxed as U
-- > excludeBits mask ws == U.map snd (U.filter (not . unBit . fst) (U.zip mask ws))
excludeBits :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
excludeBits is xs = runST $ do
    xs1 <- U.thaw xs
    n <- B.excludeBitsInPlace is xs1
    Unsafe.unsafeFreeze (MV.take n xs1)

-- | Return the index of the first bit in the vector
-- with the specified value, if any.
-- Similar to 'Data.Vector.Unboxed.elemIndex', but much faster.
--
-- >>> bitIndex (Bit True) (read "[0,0,1,0,1]")
-- Just 2
-- >>> bitIndex (Bit True) (read "[0,0,0,0,0]")
-- Nothing
--
-- prop> bitIndex bit == nthBitIndex bit 1
--
-- One can also use it to reduce a vector with disjunction or conjunction:
--
-- >>> import Data.Maybe
-- >>> isAnyBitSet   = isJust    . bitIndex (Bit True)
-- >>> areAllBitsSet = isNothing . bitIndex (Bit False)
bitIndex :: Bit -> U.Vector Bit -> Maybe Int
bitIndex b xs = mfilter (< n) (loop 0)
    where
        !n = V.length xs
        !ff | unBit b   = ffs
            | otherwise = ffs . complement

        loop !i
            | i >= n    = Nothing
            | otherwise = fmap (i +) (ff (indexWord xs i)) `mplus` loop (i + wordSize)
