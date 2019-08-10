{-# LANGUAGE CPP              #-}

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

#ifndef BITVEC_THREADSAFE
module Data.Bit.Mutable
#else
module Data.Bit.MutableTS
#endif
     ( castFromWordsM
     , castToWordsM
     , cloneToWordsM

     , zipInPlace

     , invertInPlace
     , selectBitsInPlace
     , excludeBitsInPlace

     , reverseInPlace
     ) where

import           Control.Monad
import           Control.Monad.Primitive
#ifndef BITVEC_THREADSAFE
import           Data.Bit.Internal
#else
import           Data.Bit.InternalTS
#endif
import           Data.Bit.Utils
import           Data.Bits
import qualified Data.Vector.Generic.Mutable       as MV
import qualified Data.Vector.Generic               as V
import qualified Data.Vector.Primitive             as P
import qualified Data.Vector.Unboxed               as U (Vector)
import           Data.Vector.Unboxed.Mutable       as U
import           Data.Word
import           Prelude                           as P
    hiding (and, or, any, all, reverse)

-- | Cast a vector of words to a vector of bits.
-- Cf. 'Data.Bit.castFromWords'.
castFromWordsM
    :: U.MVector s Word
    -> U.MVector s Bit
castFromWordsM (MV_Word (P.MVector off len ws)) =
    BitMVec (mulWordSize off) (mulWordSize len) ws

-- | Try to cast a vector of bits to a vector of words.
-- It succeeds if a vector of bits is aligned.
-- Use 'cloneToWordsM' otherwise.
-- Cf. 'Data.Bit.castToWords'.
castToWordsM
    :: U.MVector s Bit
    -> Maybe (U.MVector s Word)
castToWordsM (BitMVec s n ws)
    | aligned s
    , aligned n
    = Just $ MV_Word $ P.MVector (divWordSize s) (divWordSize n) ws
    | otherwise
    = Nothing

-- | Clone a vector of bits to a new unboxed vector of words.
-- If the bits don't completely fill the words, the last word will be zero-padded.
-- Cf. 'Data.Bit.cloneToWords'.
cloneToWordsM
    :: PrimMonad m
    => U.MVector (PrimState m) Bit
    -> m (U.MVector (PrimState m) Word)
cloneToWordsM v = do
    let lenBits  = MV.length v
        lenWords = nWords lenBits
    w@(BitMVec _ _ arr) <- MV.unsafeNew (mulWordSize lenWords)
    MV.unsafeCopy (U.slice 0 lenBits w) v
    MV.set (U.slice lenBits (mulWordSize lenWords - lenBits) w) (Bit False)
    pure $ MV_Word $ P.MVector 0 lenWords arr
{-# INLINE cloneToWordsM #-}

-- | Zip two vectors with the given function.
-- rewriting contents of the second argument.
-- Cf. 'Data.Bit.zipBits'.
--
-- >>> import Data.Bits
-- >>> modify (zipInPlace (.&.) (read "[1,1,0]")) (read "[0,1,1]")
-- [0,1,0]
--
-- __Warning__: if the immutable vector is shorter than the mutable one,
-- it is a caller's responsibility to trim the result:
--
-- >>> import Data.Bits
-- >>> modify (zipInPlace (.&.) (read "[1,1,0]")) (read "[0,1,1,1,1,1]")
-- [0,1,0,1,1,1] -- note trailing garbage
zipInPlace
    :: PrimMonad m
    => (forall a. Bits a => a -> a -> a)
    -> U.Vector Bit
    -> U.MVector (PrimState m) Bit
    -> m ()
zipInPlace f xs ys = loop 0
    where
        !n = min (V.length xs) (MV.length ys)
        loop !i
            | i >= n = pure ()
            | otherwise = do
                let x = indexWord xs i
                y <- readWord ys i
                writeWord ys i (f x y)
                loop (i + wordSize)
{-# INLINE zipInPlace #-}

-- | Invert (flip) all bits in-place.
--
-- Combine with 'Data.Vector.Unboxed.modify'
-- or simply resort to 'Data.Vector.Unboxed.map' 'Data.Bits.complement'
-- to operate on immutable vectors.
--
-- >>> Data.Vector.Unboxed.modify invertInPlace (read "[0,1,0,1,0]")
-- [1,0,1,0,1]
invertInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> m ()
invertInPlace xs = loop 0
    where
        !n = MV.length xs
        loop !i
            | i >= n = pure ()
            | otherwise = do
                x <- readWord xs i
                writeWord xs i (complement x)
                loop (i + wordSize)
{-# INLINE invertInPlace #-}

-- | Same as 'Data.Bit.selectBits', but deposit
-- selected bits in-place. Returns a number of selected bits.
-- It is caller's resposibility to trim the result to this number.
selectBitsInPlace
    :: PrimMonad m
    => U.Vector Bit
    -> U.MVector (PrimState m) Bit
    -> m Int
selectBitsInPlace is xs = loop 0 0
    where
        !n = min (V.length is) (MV.length xs)
        loop !i !ct
            | i >= n    = return ct
            | otherwise = do
                x <- readWord xs i
                let !(nSet, x') = selectWord (masked (n - i) (indexWord is i)) x
                writeWord xs ct x'
                loop (i + wordSize) (ct + nSet)

-- | Same as 'Data.Bit.excludeBits', but deposit
-- excluded bits in-place. Returns a number of excluded bits.
-- It is caller's resposibility to trim the result to this number.
excludeBitsInPlace :: PrimMonad m => U.Vector Bit -> U.MVector (PrimState m) Bit -> m Int
excludeBitsInPlace is xs = loop 0 0
    where
        !n = min (V.length is) (MV.length xs)
        loop !i !ct
            | i >= n    = return ct
            | otherwise = do
                x <- readWord xs i
                let !(nSet, x') = selectWord (masked (n - i) (complement (indexWord is i))) x
                writeWord xs ct x'
                loop (i + wordSize) (ct + nSet)

-- | Reverse the order of bits in-place.
--
-- Combine with 'Data.Vector.Unboxed.modify'
-- or simply resort to 'Data.Vector.Unboxed.reverse'
-- to operate on immutable vectors.
--
-- >>> Data.Vector.Unboxed.modify reverseInPlace (read "[1,1,0,1,0]")
-- [0,1,0,1,1]
reverseInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> m ()
reverseInPlace xs
    | len == 0  = pure ()
    | otherwise = loop 0
    where
        len = MV.length xs

        loop !i
            | i' <= j'  = do
                x <- readWord xs i
                y <- readWord xs j'

                writeWord xs i  (reverseWord y)
                writeWord xs j' (reverseWord x)

                loop i'
            | i' < j    = do
                let w = (j - i) `shiftR` 1
                    k =  j - w
                x <- readWord xs i
                y <- readWord xs k

                writeWord xs i (meld w (reversePartialWord w y) x)
                writeWord xs k (meld w (reversePartialWord w x) y)

                loop i'
            | otherwise = do
                let w = j - i
                x <- readWord xs i
                writeWord xs i (meld w (reversePartialWord w x) x)
            where
                !j  = len - i
                !i' = i + wordSize
                !j' = j - wordSize
