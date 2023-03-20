{-# LANGUAGE CPP                        #-}

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE ViewPatterns               #-}

#ifndef BITVEC_THREADSAFE
module Data.Bit.Internal
#else
module Data.Bit.InternalTS
#endif
  ( Bit(..)
  , U.Vector(BitVec)
  , U.MVector(BitMVec)
  , indexWord
  , readWord
  , writeWord
  , unsafeFlipBit
  , flipBit
  , modifyByteArray
  ) where

#if MIN_VERSION_vector(0,13,0)
import Data.Vector.Internal.Check (checkIndex, Checks(..))
#else
#include "vector.h"
#endif

import Control.DeepSeq
import Control.Exception
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Bit.Utils
import Data.Primitive.ByteArray
import Data.Ratio
import Data.Typeable
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed as U
import GHC.Generics

#ifdef BITVEC_THREADSAFE
import GHC.Exts
#endif

#ifndef BITVEC_THREADSAFE
-- | A newtype wrapper with a custom instance
-- for "Data.Vector.Unboxed", which packs booleans
-- as efficient as possible (8 values per byte).
-- Unboxed vectors of `Bit` use 8x less memory
-- than unboxed vectors of 'Bool' (which store one value per byte),
-- but random writes are slightly slower.
--
-- @since 0.1
newtype Bit = Bit {
  unBit :: Bool -- ^ @since 0.2.0.0
  } deriving
  (Bounded, Enum, Eq, Ord
  , FiniteBits -- ^ @since 0.2.0.0
  , Bits, Typeable
  , Generic    -- ^ @since 1.0.1.0
  , NFData     -- ^ @since 1.0.1.0
  )
#else
-- | A newtype wrapper with a custom instance
-- for "Data.Vector.Unboxed", which packs booleans
-- as efficient as possible (8 values per byte).
-- Unboxed vectors of `Bit` use 8x less memory
-- than unboxed vectors of 'Bool' (which store one value per byte),
-- but random writes are slightly slower.
--
-- @since 1.0.0.0
newtype Bit = Bit {
  unBit :: Bool -- ^ @since 0.2.0.0
  } deriving
  (Bounded, Enum, Eq, Ord
  , FiniteBits -- ^ @since 0.2.0.0
  , Bits, Typeable
  , Generic    -- ^ @since 1.0.1.0
  , NFData     -- ^ @since 1.0.1.0
  )
#endif

-- | There is only one lawful 'Num' instance possible
-- with '+' = 'xor' and
-- 'fromInteger' = 'Bit' . 'odd'.
--
-- @since 1.0.1.0
instance Num Bit where
  Bit a * Bit b = Bit (a && b)
  Bit a + Bit b = Bit (a /= b)
  Bit a - Bit b = Bit (a /= b)
  negate = id
  abs    = id
  signum = id
  fromInteger = Bit . odd

-- | @since 1.0.1.0
instance Real Bit where
  toRational = fromIntegral

-- | @since 1.0.1.0
instance Integral Bit where
  quotRem _ (Bit False) = throw DivideByZero
  quotRem x (Bit True)  = (x, Bit False)
  toInteger (Bit False) = 0
  toInteger (Bit True)  = 1

-- | @since 1.0.1.0
instance Fractional Bit where
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)
  (/) = quot

instance Show Bit where
  showsPrec _ (Bit False) = showString "0"
  showsPrec _ (Bit True ) = showString "1"

instance Read Bit where
  readsPrec p (' ' : rest) = readsPrec p rest
  readsPrec _ ('0' : rest) = [(Bit False, rest)]
  readsPrec _ ('1' : rest) = [(Bit True, rest)]
  readsPrec _ _            = []

instance U.Unbox Bit

-- Ints are offset and length in bits
data instance U.MVector s Bit = BitMVec !Int !Int !(MutableByteArray s)
data instance U.Vector    Bit = BitVec  !Int !Int !ByteArray

readBit :: Int -> Word -> Bit
readBit i w = Bit (w .&. (1 `unsafeShiftL` i) /= 0)
{-# INLINE readBit #-}

extendToWord :: Bit -> Word
extendToWord (Bit False) = 0
extendToWord (Bit True ) = complement 0

-- | Read a word at the given bit offset in little-endian order (i.e., the LSB will correspond to the bit at the given address, the 2's bit will correspond to the address + 1, etc.).  If the offset is such that the word extends past the end of the vector, the result is padded with memory garbage.
indexWord :: U.Vector Bit -> Int -> Word
indexWord (BitVec _ 0 _) _ = 0
indexWord (BitVec off len' arr) !i' = word
 where
  len    = off + len'
  i      = off + i'
  nMod   = modWordSize i
  loIx   = divWordSize i
  loWord = indexByteArray arr loIx
  hiWord = indexByteArray arr (loIx + 1)

  word
    | nMod == 0
    = loWord
    | loIx == divWordSize (len - 1)
    = loWord `unsafeShiftR` nMod
    | otherwise
    = (loWord `unsafeShiftR` nMod) .|. (hiWord `unsafeShiftL` (wordSize - nMod))
{-# INLINE indexWord #-}

-- | Read a word at the given bit offset in little-endian order (i.e., the LSB will correspond to the bit at the given address, the 2's bit will correspond to the address + 1, etc.).  If the offset is such that the word extends past the end of the vector, the result is padded with memory garbage.
readWord :: PrimMonad m => U.MVector (PrimState m) Bit -> Int -> m Word
readWord (BitMVec _ 0 _) _ = pure 0
readWord (BitMVec off len' arr) !i' = do
  let len  = off + len'
      i    = off + i'
      nMod = modWordSize i
      loIx = divWordSize i
  loWord <- readByteArray arr loIx

  if nMod == 0
    then pure loWord
    else if loIx == divWordSize (len - 1)
      then pure (loWord `unsafeShiftR` nMod)
      else do
        hiWord <- readByteArray arr (loIx + 1)
        pure
          $   (loWord `unsafeShiftR` nMod)
          .|. (hiWord `unsafeShiftL` (wordSize - nMod))
{-# SPECIALIZE readWord :: U.MVector s Bit -> Int -> ST s Word #-}
{-# INLINE readWord #-}

modifyByteArray
  :: PrimMonad m
  => MutableByteArray (PrimState m)
  -> Int
  -> Word
  -> Word
  -> m ()
#ifndef BITVEC_THREADSAFE
modifyByteArray arr ix msk new = do
  old <- readByteArray arr ix
  writeByteArray arr ix (old .&. msk .|. new)
{-# INLINE modifyByteArray #-}
#else
modifyByteArray (MutableByteArray mba) (I# ix) (W# msk) (W# new) = do
  primitive $ \state ->
    let !(# state',  _ #) = fetchAndIntArray# mba ix (word2Int# msk) state  in
    let !(# state'', _ #) = fetchOrIntArray#  mba ix (word2Int# new) state' in
    (# state'', () #)

-- https://gitlab.haskell.org/ghc/ghc/issues/17334
#if __GLASGOW_HASKELL__ == 808 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 1
{-# NOINLINE modifyByteArray #-}
#else
{-# INLINE modifyByteArray #-}
#endif

#endif

-- | Write a word at the given bit offset in little-endian order (i.e., the LSB will correspond to the bit at the given address, the 2's bit will correspond to the address + 1, etc.).  If the offset is such that the word extends past the end of the vector, the word is truncated and as many low-order bits as possible are written.
writeWord :: PrimMonad m => U.MVector (PrimState m) Bit -> Int -> Word -> m ()
writeWord (BitMVec _ 0 _) _ _ = pure ()
writeWord (BitMVec off len' arr) !i' !x
  | iMod == 0
  = if len >= i + wordSize
    then writeByteArray arr iDiv x
    else modifyByteArray arr iDiv (hiMask lenMod) (x .&. loMask lenMod)
  | iDiv == divWordSize (len - 1)
  = if lenMod == 0
    then modifyByteArray arr iDiv (loMask iMod) (x `unsafeShiftL` iMod)
    else modifyByteArray arr iDiv (loMask iMod .|. hiMask lenMod) ((x `unsafeShiftL` iMod) .&. loMask lenMod)
  | iDiv + 1 == divWordSize (len - 1)
  = do
    modifyByteArray arr iDiv (loMask iMod) (x `unsafeShiftL` iMod)
    if lenMod == 0
    then modifyByteArray arr (iDiv + 1) (hiMask iMod) (x `unsafeShiftR` (wordSize - iMod))
    else modifyByteArray arr (iDiv + 1) (hiMask iMod .|. hiMask lenMod) (x `unsafeShiftR` (wordSize - iMod) .&. loMask lenMod)
  | otherwise
  = do
    modifyByteArray arr iDiv (loMask iMod) (x `unsafeShiftL` iMod)
    modifyByteArray arr (iDiv + 1) (hiMask iMod) (x `unsafeShiftR` (wordSize - iMod))
  where
    len    = off + len'
    lenMod = modWordSize len
    i      = off + i'
    iMod   = modWordSize i
    iDiv   = divWordSize i

{-# SPECIALIZE writeWord :: U.MVector s Bit -> Int -> Word -> ST s () #-}
{-# INLINE writeWord #-}

instance MV.MVector U.MVector Bit where
  {-# INLINE basicInitialize #-}
  basicInitialize vec = MV.basicSet vec (Bit False)

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n
    | n < 0 = error $ "Data.Bit.basicUnsafeNew: negative length: " ++ show n
    | otherwise = do
      arr <- newByteArray (wordsToBytes $ nWords n)
      pure $ BitMVec 0 n arr

  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n x
    | n < 0 =  error $  "Data.Bit.basicUnsafeReplicate: negative length: " ++ show n
    | otherwise = do
      arr <- newByteArray (wordsToBytes $ nWords n)
      setByteArray arr 0 (nWords n) (extendToWord x :: Word)
      pure $ BitMVec 0 n arr

  {-# INLINE basicOverlaps #-}
  basicOverlaps (BitMVec i' m' arr1) (BitMVec j' n' arr2) =
    sameMutableByteArray arr1 arr2
      && (between i j (j + n) || between j i (i + m))
   where
    i = divWordSize i'
    m = nWords (i' + m') - i
    j = divWordSize j'
    n = nWords (j' + n') - j
    between x y z = x >= y && x < z

  {-# INLINE basicLength #-}
  basicLength (BitMVec _ n _) = n

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (BitMVec off _ arr) !i' = do
    let i = off + i'
    word <- readByteArray arr (divWordSize i)
    pure $ readBit (modWordSize i) word

  {-# INLINE basicUnsafeWrite #-}
#ifndef BITVEC_THREADSAFE
  basicUnsafeWrite (BitMVec off _ arr) !i' !x = do
    let i  = off + i'
        j  = divWordSize i
        k  = modWordSize i
        kk = 1 `unsafeShiftL` k :: Word
    word <- readByteArray arr j
    writeByteArray arr j (if unBit x then word .|. kk else word .&. complement kk)
#else
  basicUnsafeWrite (BitMVec off _ (MutableByteArray mba)) !i' (Bit b) = do
    let i       = off + i'
        !(I# j) = divWordSize i
        !(I# k) = 1 `unsafeShiftL` modWordSize i
    primitive $ \state ->
      let !(# state', _ #) =
              (if b
                then fetchOrIntArray# mba j k state
                else fetchAndIntArray# mba j (notI# k) state
              )
      in  (# state', () #)
#endif

  {-# INLINE basicSet #-}
  basicSet (BitMVec off len arr) (extendToWord -> x) | offBits == 0 =
    case modWordSize len of
      0    -> setByteArray arr offWords lWords (x :: Word)
      nMod -> do
        setByteArray arr offWords (lWords - 1) (x :: Word)
        modifyByteArray arr (offWords + lWords - 1) (hiMask nMod) (x .&. loMask nMod)
   where
    offBits  = modWordSize off
    offWords = divWordSize off
    lWords   = nWords (offBits + len)
  basicSet (BitMVec off len arr) (extendToWord -> x) =
    case modWordSize (off + len) of
      0 -> do
        modifyByteArray arr offWords (loMask offBits) (x .&. hiMask offBits)
        setByteArray arr (offWords + 1) (lWords - 1) (x :: Word)
      nMod -> if lWords == 1
        then do
          let lohiMask = loMask offBits .|. hiMask nMod
          modifyByteArray arr offWords lohiMask (x .&. complement lohiMask)
        else do
          modifyByteArray arr offWords (loMask offBits) (x .&. hiMask offBits)
          setByteArray arr (offWords + 1) (lWords - 2) (x :: Word)
          modifyByteArray arr (offWords + lWords - 1) (hiMask nMod) (x .&. loMask nMod)
   where
    offBits  = modWordSize off
    offWords = divWordSize off
    lWords   = nWords (offBits + len)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (BitMVec offDst lenDst dst) (BitMVec offSrc _ src)
    | offDstBits == 0, offSrcBits == 0 = case modWordSize lenDst of
      0 -> copyMutableByteArray dst
                                (wordsToBytes offDstWords)
                                src
                                (wordsToBytes offSrcWords)
                                (wordsToBytes lDstWords)
      nMod -> do
        copyMutableByteArray dst
                             (wordsToBytes offDstWords)
                             src
                             (wordsToBytes offSrcWords)
                             (wordsToBytes $ lDstWords - 1)

        lastWordSrc <- readByteArray src (offSrcWords + lDstWords - 1)
        modifyByteArray dst (offDstWords + lDstWords - 1) (hiMask nMod) (lastWordSrc .&. loMask nMod)
   where
    offDstBits  = modWordSize offDst
    offDstWords = divWordSize offDst
    lDstWords   = nWords (offDstBits + lenDst)
    offSrcBits  = modWordSize offSrc
    offSrcWords = divWordSize offSrc
  basicUnsafeCopy (BitMVec offDst lenDst dst) (BitMVec offSrc _ src)
    | offDstBits == offSrcBits = case modWordSize (offSrc + lenDst) of
      0 -> do
        firstWordSrc <- readByteArray src offSrcWords
        modifyByteArray dst offDstWords (loMask offSrcBits) (firstWordSrc .&. hiMask offSrcBits)
        copyMutableByteArray dst
                             (wordsToBytes $ offDstWords + 1)
                             src
                             (wordsToBytes $ offSrcWords + 1)
                             (wordsToBytes $ lDstWords - 1)
      nMod -> if lDstWords == 1
        then do
          let lohiMask = loMask offSrcBits .|. hiMask nMod
          theOnlyWordSrc <- readByteArray src offSrcWords
          modifyByteArray dst offDstWords lohiMask (theOnlyWordSrc .&. complement lohiMask)
        else do
          firstWordSrc <- readByteArray src offSrcWords
          modifyByteArray dst offDstWords (loMask offSrcBits) (firstWordSrc .&. hiMask offSrcBits)
          copyMutableByteArray dst
                               (wordsToBytes $ offDstWords + 1)
                               src
                               (wordsToBytes $ offSrcWords + 1)
                               (wordsToBytes $ lDstWords - 2)
          lastWordSrc <- readByteArray src (offSrcWords + lDstWords - 1)
          modifyByteArray dst (offDstWords + lDstWords - 1) (hiMask nMod) (lastWordSrc .&. loMask nMod)
   where
    offDstBits  = modWordSize offDst
    offDstWords = divWordSize offDst
    lDstWords   = nWords (offDstBits + lenDst)
    offSrcBits  = modWordSize offSrc
    offSrcWords = divWordSize offSrc

  basicUnsafeCopy dst@(BitMVec _ len _) src = do_copy 0
   where
    n = alignUp len

    do_copy i
      | i < n = do
        x <- readWord src i
        writeWord dst i x
        do_copy (i + wordSize)
      | otherwise = pure ()

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove !dst src@(BitMVec srcShift srcLen _)
    | MV.basicOverlaps dst src = do
          -- Align shifts of src and srcCopy to speed up basicUnsafeCopy srcCopy src
      srcCopy <- MV.drop (modWordSize srcShift)
        <$> MV.basicUnsafeNew (modWordSize srcShift + srcLen)
      MV.basicUnsafeCopy srcCopy src
      MV.basicUnsafeCopy dst srcCopy
    | otherwise = MV.basicUnsafeCopy dst src

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice offset n (BitMVec off _ arr) = BitMVec (off + offset) n arr

  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow (BitMVec off len src) byBits
    | byWords == 0 = pure $ BitMVec off (len + byBits) src
    | otherwise = do
      dst <- newByteArray (wordsToBytes newWords)
      copyMutableByteArray dst 0 src 0 (wordsToBytes oldWords)
      pure $ BitMVec off (len + byBits) dst
   where
    oldWords = nWords (off + len)
    newWords = nWords (off + len + byBits)
    byWords  = newWords - oldWords

#ifndef BITVEC_THREADSAFE

-- | Flip the bit at the given position.
-- No bound checks are performed.
-- Equivalent to 'flip' 'Data.Vector.Unboxed.Mutable.unsafeModify' 'Data.Bits.complement',
-- but up to 2x faster.
--
-- In general there is no reason to 'Data.Vector.Unboxed.Mutable.unsafeModify' bit vectors:
-- either you modify it with 'id' (which is 'id' altogether)
-- or with 'Data.Bits.complement' (which is 'unsafeFlipBit').
--
-- >>> :set -XOverloadedLists
-- >>> Data.Vector.Unboxed.modify (`unsafeFlipBit` 2) [1,1,1,1]
-- [1,1,0,1]
--
-- @since 1.0.0.0
unsafeFlipBit :: PrimMonad m => U.MVector (PrimState m) Bit -> Int -> m ()
unsafeFlipBit v i =
#if MIN_VERSION_vector(0,13,0)
  checkIndex Unsafe
#else
  UNSAFE_CHECK(checkIndex) "flipBit"
#endif
    i (MV.length v) $ basicFlipBit v i
{-# INLINE unsafeFlipBit #-}

basicFlipBit :: PrimMonad m => U.MVector (PrimState m) Bit -> Int -> m ()
basicFlipBit (BitMVec off _ arr) !i' = do
  let i  = off + i'
      j  = divWordSize i
      k  = modWordSize i
      kk = 1 `unsafeShiftL` k :: Word
  word <- readByteArray arr j
  writeByteArray arr j (word `xor` kk)
{-# INLINE basicFlipBit #-}

-- | Flip the bit at the given position.
-- Equivalent to 'flip' 'Data.Vector.Unboxed.Mutable.modify' 'Data.Bits.complement',
-- but up to 2x faster.
--
-- In general there is no reason to 'Data.Vector.Unboxed.Mutable.modify' bit vectors:
-- either you modify it with 'id' (which is 'id' altogether)
-- or with 'Data.Bits.complement' (which is 'flipBit').
--
-- >>> :set -XOverloadedLists
-- >>> Data.Vector.Unboxed.modify (`flipBit` 2) [1,1,1,1]
-- [1,1,0,1]
--
-- @since 1.0.0.0
flipBit :: PrimMonad m => U.MVector (PrimState m) Bit -> Int -> m ()
flipBit v i =
#if MIN_VERSION_vector(0,13,0)
  checkIndex Bounds
#else
  BOUNDS_CHECK(checkIndex) "flipBit"
#endif
    i (MV.length v) $
      unsafeFlipBit v i
{-# INLINE flipBit #-}

#else

-- | Flip the bit at the given position.
-- No bound checks are performed.
-- Equivalent to 'flip' 'Data.Vector.Unboxed.Mutable.unsafeModify' 'Data.Bits.complement',
-- but up to 33% faster and atomic.
--
-- In general there is no reason to 'Data.Vector.Unboxed.Mutable.unsafeModify' bit vectors:
-- either you modify it with 'id' (which is 'id' altogether)
-- or with 'Data.Bits.complement' (which is 'unsafeFlipBit').
--
-- >>> Data.Vector.Unboxed.modify (\v -> unsafeFlipBit v 1) (read "[1,1,1]")
-- [1,0,1]
unsafeFlipBit :: PrimMonad m => U.MVector (PrimState m) Bit -> Int -> m ()
unsafeFlipBit v i =
#if MIN_VERSION_vector(0,13,0)
  checkIndex Unsafe
#else
  UNSAFE_CHECK(checkIndex) "flipBit"
#endif
    i (MV.length v) $ basicFlipBit v i
{-# INLINE unsafeFlipBit #-}

basicFlipBit :: PrimMonad m => U.MVector (PrimState m) Bit -> Int -> m ()
basicFlipBit (BitMVec off _ (MutableByteArray mba)) !i' = do
  let i       = off + i'
      !(I# j) = divWordSize i
      !(I# k) = 1 `unsafeShiftL` modWordSize i
  primitive $ \state ->
    let !(# state', _ #) = fetchXorIntArray# mba j k state in (# state', () #)
{-# INLINE basicFlipBit #-}

-- | Flip the bit at the given position.
-- Equivalent to 'flip' 'Data.Vector.Unboxed.Mutable.modify' 'Data.Bits.complement',
-- but up to 33% faster and atomic.
--
-- In general there is no reason to 'Data.Vector.Unboxed.Mutable.modify' bit vectors:
-- either you modify it with 'id' (which is 'id' altogether)
-- or with 'Data.Bits.complement' (which is 'flipBit').
--
-- >>> Data.Vector.Unboxed.modify (\v -> flipBit v 1) (read "[1,1,1]")
-- [1,0,1]
flipBit :: PrimMonad m => U.MVector (PrimState m) Bit -> Int -> m ()
flipBit v i =
#if MIN_VERSION_vector(0,13,0)
  checkIndex Bounds
#else
  BOUNDS_CHECK(checkIndex) "flipBit"
#endif
    i (MV.length v) $ basicFlipBit v i
{-# INLINE flipBit #-}

#endif

instance V.Vector U.Vector Bit where
  basicUnsafeFreeze (BitMVec s n v) = BitVec s n <$> unsafeFreezeByteArray v
  basicUnsafeThaw (BitVec s n v) = BitMVec s n <$> unsafeThawByteArray v
  basicLength (BitVec _ n _) = n

  basicUnsafeIndexM (BitVec off _ arr) !i' = do
    let i = off + i'
    pure $! readBit (modWordSize i) (indexByteArray arr (divWordSize i))

  basicUnsafeCopy dst src = do
    src1 <- V.basicUnsafeThaw src
    MV.basicUnsafeCopy dst src1

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice offset n (BitVec off _ arr) = BitVec (off + offset) n arr
