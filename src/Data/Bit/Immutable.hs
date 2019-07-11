{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Data.Bit.Immutable
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
import           Data.Bit.Internal
import qualified Data.Bit.Mutable                   as B
import           Data.Bit.Utils
import qualified Data.Vector.Generic.Mutable       as MV
import qualified Data.Vector.Generic               as V
import           Data.Vector.Unboxed                as U
    hiding (and, or, any, all, reverse, findIndex)
import qualified Data.Vector.Unboxed                as Unsafe
import           Data.Word
import           Prelude                           as P
    hiding (and, or, any, all, reverse)

-- | Cast a vector of words to a vector of bits in-place.
castFromWords
    :: U.Vector Word
    -> U.Vector Bit
castFromWords ws = BitVec 0 (nBits (V.length ws)) ws

-- | Try to cast a vector of bits to a vector of words in-place.
-- It succeeds if a vector of bits is aligned.
-- Use 'cloneToWords' otherwise.
castToWords
    :: U.Vector Bit
    -> Maybe (U.Vector Word)
castToWords v@(BitVec s n ws)
    | aligned s
    , aligned n
    = Just $ V.slice (divWordSize s) (nWords n) ws
    | otherwise
    = Nothing

-- | Clone a vector of bits to a new unboxed vector of words.
-- If the bits don't completely fill the words, the last word will be zero-padded.
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

-- |Return the address of the first bit in the vector with the specified value, if any
bitIndex :: Bit -> U.Vector Bit -> Maybe Int
bitIndex b xs = mfilter (< n) (loop 0)
    where
        !n = V.length xs
        !ff | unBit b   = ffs
            | otherwise = ffs . complement

        loop !i
            | i >= n    = Nothing
            | otherwise = fmap (i +) (ff (indexWord xs i)) `mplus` loop (i + wordSize)
