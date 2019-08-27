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

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
#ifndef BITVEC_THREADSAFE
import Data.Bit.Internal
#else
import Data.Bit.InternalTS
#endif
import Data.Bit.Utils
import Data.Bits
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

-- | Cast a vector of words to a vector of bits.
-- Cf. 'Data.Bit.castFromWords'.
castFromWordsM :: MVector s Word -> MVector s Bit
castFromWordsM (MU.MV_Word (P.MVector off len ws)) =
  BitMVec (mulWordSize off) (mulWordSize len) ws

-- | Try to cast a vector of bits to a vector of words.
-- It succeeds if a vector of bits is aligned.
-- Use 'cloneToWordsM' otherwise.
-- Cf. 'Data.Bit.castToWords'.
castToWordsM :: MVector s Bit -> Maybe (MVector s Word)
castToWordsM (BitMVec s n ws)
  | aligned s, aligned n = Just $ MU.MV_Word $ P.MVector (divWordSize s)
                                                         (divWordSize n)
                                                         ws
  | otherwise = Nothing

-- | Clone a vector of bits to a new unboxed vector of words.
-- If the bits don't completely fill the words, the last word will be zero-padded.
-- Cf. 'Data.Bit.cloneToWords'.
cloneToWordsM
  :: PrimMonad m => MVector (PrimState m) Bit -> m (MVector (PrimState m) Word)
cloneToWordsM v = do
  let lenBits  = MU.length v
      lenWords = nWords lenBits
  w@(BitMVec _ _ arr) <- MU.unsafeNew (mulWordSize lenWords)
  MU.unsafeCopy (MU.slice 0 lenBits w) v
  MU.set (MU.slice lenBits (mulWordSize lenWords - lenBits) w) (Bit False)
  pure $ MU.MV_Word $ P.MVector 0 lenWords arr
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
  => (forall a . Bits a => a -> a -> a)
  -> Vector Bit
  -> MVector (PrimState m) Bit
  -> m ()
zipInPlace f xs ys = do
  let n = min (U.length xs) (MU.length ys)
  forM_ [0, wordSize .. n - 1] $ \i -> do
    let x = indexWord xs i
    y <- readWord ys i
    writeWord ys i (f x y)
{-# INLINE zipInPlace #-}

-- | Invert (flip) all bits in-place.
--
-- >>> Data.Vector.Unboxed.modify invertInPlace (read "[0,1,0,1,0]")
-- [1,0,1,0,1]
invertInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> m ()
invertInPlace xs = do
  let n = MU.length xs
  forM_ [0, wordSize .. n - 1] $ \i -> do
    x <- readWord xs i
    writeWord xs i (complement x)
#if __GLASGOW_HASKELL__ >= 800
{-# SPECIALIZE invertInPlace :: U.MVector s Bit -> ST s () #-}
#endif

-- | Same as 'Data.Bit.selectBits', but deposit
-- selected bits in-place. Returns a number of selected bits.
-- It is caller's resposibility to trim the result to this number.
selectBitsInPlace
  :: PrimMonad m => U.Vector Bit -> U.MVector (PrimState m) Bit -> m Int
selectBitsInPlace is xs = loop 0 0
 where
  !n = min (U.length is) (MU.length xs)
  loop !i !ct
    | i >= n = return ct
    | otherwise = do
      x <- readWord xs i
      let !(nSet, x') = selectWord (masked (n - i) (indexWord is i)) x
      writeWord xs ct x'
      loop (i + wordSize) (ct + nSet)

-- | Same as 'Data.Bit.excludeBits', but deposit
-- excluded bits in-place. Returns a number of excluded bits.
-- It is caller's resposibility to trim the result to this number.
excludeBitsInPlace
  :: PrimMonad m => U.Vector Bit -> U.MVector (PrimState m) Bit -> m Int
excludeBitsInPlace is xs = loop 0 0
 where
  !n = min (U.length is) (MU.length xs)
  loop !i !ct
    | i >= n = return ct
    | otherwise = do
      x <- readWord xs i
      let !(nSet, x') =
            selectWord (masked (n - i) (complement (indexWord is i))) x
      writeWord xs ct x'
      loop (i + wordSize) (ct + nSet)

-- | Reverse the order of bits in-place.
--
-- >>> Data.Vector.Unboxed.modify reverseInPlace (read "[1,1,0,1,0]")
-- [0,1,0,1,1]
reverseInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> m ()
reverseInPlace xs | len == 0  = pure ()
                  | otherwise = loop 0
 where
  len = MU.length xs

  loop !i
    | i' <= j' = do
      x <- readWord xs i
      y <- readWord xs j'

      writeWord xs i  (reverseWord y)
      writeWord xs j' (reverseWord x)

      loop i'
    | i' < j = do
      let w = (j - i) `shiftR` 1
          k = j - w
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
#if __GLASGOW_HASKELL__ >= 800
{-# SPECIALIZE reverseInPlace :: U.MVector s Bit -> ST s () #-}
#endif
