{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Data.Bit.Mutable
     ( castFromWords
     , castToWords
     , cloneToWords

     , zipInPlace

     , invertInPlace
     , selectBitsInPlace
     , excludeBitsInPlace

     , reverseInPlace
     ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bit.Internal
import           Data.Bit.Utils
import           Data.Bits
import qualified Data.Vector.Generic.Mutable       as MV
import qualified Data.Vector.Generic               as V
import qualified Data.Vector.Unboxed               as U (Vector)
import           Data.Vector.Unboxed.Mutable       as U
import           Data.Word
import           Prelude                           as P
    hiding (and, or, any, all, reverse)

-- | Cast a vector of words to a vector of bits in-place.
castFromWords
    :: U.MVector s Word
    -> U.MVector s Bit
castFromWords ws = BitMVec 0 (nBits (MV.length ws)) ws

-- | Try to cast a vector of bits to a vector of words in-place.
-- It succeeds if a vector of bits is aligned.
-- Use 'cloneToWords' otherwise.
castToWords
    :: U.MVector s Bit
    -> Maybe (U.MVector s Word)
castToWords v@(BitMVec s n ws)
    | aligned s
    , aligned n
    = Just $ MV.slice (divWordSize s) (nWords n) ws
    | otherwise
    = Nothing

-- | Clone a vector of bits to a new unboxed vector of words.  If the bits don't completely fill the words, the last word will be zero-padded.
cloneToWords :: PrimMonad m => U.MVector (PrimState m) Bit -> m (U.MVector (PrimState m) Word)
cloneToWords v@(BitMVec _ n _) = do
    ws <- MV.new (nWords n)
    let loop !i !j
            | i >= n    = return ()
            | otherwise = do
                readWord v i >>= MV.write ws j
                loop (i + wordSize) (j + 1)
    loop 0 0
    return ws
{-# INLINE cloneToWords #-}

-- |Map a function over a bit vector one 'Word' at a time ('wordSize' bits at a time).  The function will be passed the bit index (which will always be 'wordSize'-aligned) and the current value of the corresponding word.  The returned word will be written back to the vector.  If there is a partial word at the end of the vector, it will be zero-padded when passed to the function and truncated when the result is written back to the array.
{-# INLINE mapMInPlaceWithIndex #-}
mapMInPlaceWithIndex ::
    PrimMonad m =>
        (Int -> Word -> m Word)
     -> U.MVector (PrimState m) Bit -> m ()
mapMInPlaceWithIndex f xs@(BitMVec 0 _ v) = loop 0 0
    where
        !n_ = alignDown (MV.length xs)
        loop !i !j
            | i >= n_   = when (n_ /= MV.length xs) $ do
                readWord xs i >>= f i >>= writeWord xs i

            | otherwise = do
                MV.read v j >>= f i >>= MV.write v j
                loop (i + wordSize) (j + 1)
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

{-# INLINE zipInPlace #-}
zipInPlace :: PrimMonad m => (forall a. Bits a => a -> a -> a) -> U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
zipInPlace f xs ys@(BitVec 0 n2 v) =
    mapInPlaceWithIndex g (MV.basicUnsafeSlice 0 n xs)
    where
        -- WARNING: relies on guarantee by mapMInPlaceWithIndex that index will always be aligned!
        !n = min (MV.length xs) (V.length ys)
        {-# INLINE g #-}
        g !i !x =
            let !w = masked (n2 - i) (v V.! divWordSize i)
             in f x w
zipInPlace f xs ys =
    mapInPlaceWithIndex g (MV.basicUnsafeSlice 0 n xs)
    where
        !n = min (MV.length xs) (V.length ys)
        {-# INLINE g #-}
        g !i !x =
            let !w = indexWord ys i
             in f x w

-- |Flip every bit in the given vector
invertInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> m ()
invertInPlace = mapInPlace complement

selectBitsInPlace :: PrimMonad m => U.Vector Bit -> U.MVector (PrimState m) Bit -> m Int
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
