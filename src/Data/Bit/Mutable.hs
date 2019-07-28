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
cloneToWordsM v@(BitMVec _ n _) = do
    ws <- MV.new (nWords n)
    let loop !i !j
            | i >= n    = return ()
            | otherwise = do
                readWord v i >>= MV.write ws j
                loop (i + wordSize) (j + 1)
    loop 0 0
    return ws
{-# INLINE cloneToWordsM #-}

-- |Map a function over a bit vector one 'Word' at a time ('wordSize' bits at a time).  The function will be passed the bit index (which will always be 'wordSize'-aligned) and the current value of the corresponding word.  The returned word will be written back to the vector.  If there is a partial word at the end of the vector, it will be zero-padded when passed to the function and truncated when the result is written back to the array.
{-# INLINE mapMInPlaceWithIndex #-}
mapMInPlaceWithIndex ::
    PrimMonad m =>
        (Int -> Word -> m Word)
     -> U.MVector (PrimState m) Bit -> m ()
mapMInPlaceWithIndex f xs = loop 0
    where
        !n = MV.length xs
        loop !i
            | i >= n    = return ()
            | otherwise = do
                readWord xs i >>= f i >>= writeWord xs i
                loop (i + wordSize)

{-# INLINE mapInPlaceWithIndex #-}
mapInPlaceWithIndex ::
    PrimMonad m =>
        (Int -> Word -> Word)
     -> U.MVector (PrimState m) Bit -> m ()
mapInPlaceWithIndex f = mapMInPlaceWithIndex g
    where
        {-# INLINE g #-}
        g i x = return $! f i x

{-# INLINE mapInPlace #-}
mapInPlace :: PrimMonad m => (Word -> Word) -> U.MVector (PrimState m) Bit -> m ()
mapInPlace f = mapMInPlaceWithIndex (\_ x -> return (f x))

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
zipInPlace f ys xs =
    mapInPlaceWithIndex g (MV.basicUnsafeSlice 0 n xs)
    where
        !n = min (MV.length xs) (V.length ys)
        {-# INLINE g #-}
        g !i !x =
            let !w = indexWord ys i
             in f w x
{-# INLINE zipInPlace #-}

-- | Invert (flip) all bits in-place.
--
-- Combine with 'Data.Vector.Unboxed.modify'
-- to operate on immutable vectors.
--
-- >>> Data.Vector.Unboxed.modify invertInPlace (read "[0,1,0,1,0]")
-- [1,0,1,0,1]
invertInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> m ()
invertInPlace = mapInPlace complement

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
-- to operate on immutable vectors.
--
-- >>> Data.Vector.Unboxed.modify reverseInPlace (read "[1,1,0,1,0]")
-- [0,1,0,1,1]
reverseInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> m ()
reverseInPlace xs = loop 0 (MV.length xs)
    where
        loop !i !j
            | i' <= j'  = do
                x <- readWord xs i
                y <- readWord xs j'

                writeWord xs i  (reverseWord y)
                writeWord xs j' (reverseWord x)

                loop i' j'
            | i' < j    = do
                let w = (j - i) `shiftR` 1
                    k  = j - w
                x <- readWord xs i
                y <- readWord xs k

                writeWord xs i (meld w (reversePartialWord w y) x)
                writeWord xs k (meld w (reversePartialWord w x) y)

                loop i' j'
            | i  < j    = do
                let w = j - i
                x <- readWord xs i
                writeWord xs i (meld w (reversePartialWord w x) x)
            | otherwise = return ()
            where
                !i' = i + wordSize
                !j' = j - wordSize
