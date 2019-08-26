{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}

module Data.Bit.Utils
  ( modWordSize
  , divWordSize
  , mulWordSize
  , wordSize
  , wordsToBytes
  , nWords
  , aligned
  , alignUp
  , selectWord
  , reverseWord
  , reversePartialWord
  , masked
  , meld
  , ffs
  , loMask
  , hiMask
  , sparseBits
  ) where

#include "MachDeps.h"

import Data.Bits

-- |The number of bits in a 'Word'.  A handy constant to have around when defining 'Word'-based bulk operations on bit vectors.
wordSize :: Int
wordSize = finiteBitSize (0 :: Word)

lgWordSize, wordSizeMask, wordSizeMaskC :: Int
lgWordSize = case wordSize of
  32 -> 5
  64 -> 6
  _  -> error "wordsToBytes: unknown architecture"

wordSizeMask = wordSize - 1
wordSizeMaskC = complement wordSizeMask

divWordSize :: Bits a => a -> a
divWordSize x = unsafeShiftR x lgWordSize
{-# INLINE divWordSize #-}

modWordSize :: Int -> Int
modWordSize x = x .&. (wordSize - 1)
{-# INLINE modWordSize #-}

mulWordSize :: Bits a => a -> a
mulWordSize x = unsafeShiftL x lgWordSize

-- number of words needed to store n bits
nWords :: Int -> Int
nWords ns = divWordSize (ns + wordSize - 1)

wordsToBytes :: Int -> Int
wordsToBytes ns = case wordSize of
  32 -> ns `unsafeShiftL` 2
  64 -> ns `unsafeShiftL` 3
  _  -> error "wordsToBytes: unknown architecture"

aligned :: Int -> Bool
aligned x = x .&. wordSizeMask == 0

-- round a number of bits up to the nearest multiple of word size
alignUp :: Int -> Int
alignUp x | x == x'   = x'
          | otherwise = x' + wordSize
  where x' = alignDown x

-- round a number of bits down to the nearest multiple of word size
alignDown :: Int -> Int
alignDown x = x .&. wordSizeMaskC

-- create a mask consisting of the lower n bits
mask :: Int -> Word
mask b = m
 where
  m | b >= finiteBitSize m = complement 0
    | b < 0                = 0
    | otherwise            = bit b - 1

masked :: Int -> Word -> Word
masked b x = x .&. mask b

-- meld 2 words by taking the low 'b' bits from 'lo' and the rest from 'hi'
meld :: Int -> Word -> Word -> Word
meld b lo hi = (lo .&. m) .|. (hi .&. complement m) where m = mask b
{-# INLINE meld #-}

#if WORD_SIZE_IN_BITS == 64
reverseWord :: Word -> Word
reverseWord x0 = x6
 where
  x1 = ((x0 .&. 0x5555555555555555) `shiftL`  1) .|. ((x0 .&. 0xAAAAAAAAAAAAAAAA) `shiftR`  1)
  x2 = ((x1 .&. 0x3333333333333333) `shiftL`  2) .|. ((x1 .&. 0xCCCCCCCCCCCCCCCC) `shiftR`  2)
  x3 = ((x2 .&. 0x0F0F0F0F0F0F0F0F) `shiftL`  4) .|. ((x2 .&. 0xF0F0F0F0F0F0F0F0) `shiftR`  4)
  x4 = ((x3 .&. 0x00FF00FF00FF00FF) `shiftL`  8) .|. ((x3 .&. 0xFF00FF00FF00FF00) `shiftR`  8)
  x5 = ((x4 .&. 0x0000FFFF0000FFFF) `shiftL` 16) .|. ((x4 .&. 0xFFFF0000FFFF0000) `shiftR` 16)
  x6 = ((x5 .&. 0x00000000FFFFFFFF) `shiftL` 32) .|. ((x5 .&. 0xFFFFFFFF00000000) `shiftR` 32)
#else
reverseWord :: Word -> Word
reverseWord x0 = x5
 where
  x1 = ((x0 .&. 0x5555555555555555) `shiftL`  1) .|. ((x0 .&. 0xAAAAAAAAAAAAAAAA) `shiftR`  1)
  x2 = ((x1 .&. 0x3333333333333333) `shiftL`  2) .|. ((x1 .&. 0xCCCCCCCCCCCCCCCC) `shiftR`  2)
  x3 = ((x2 .&. 0x0F0F0F0F0F0F0F0F) `shiftL`  4) .|. ((x2 .&. 0xF0F0F0F0F0F0F0F0) `shiftR`  4)
  x4 = ((x3 .&. 0x00FF00FF00FF00FF) `shiftL`  8) .|. ((x3 .&. 0xFF00FF00FF00FF00) `shiftR`  8)
  x5 = ((x4 .&. 0x0000FFFF0000FFFF) `shiftL` 16) .|. ((x4 .&. 0xFFFF0000FFFF0000) `shiftR` 16)
#endif

reversePartialWord :: Int -> Word -> Word
reversePartialWord n w | n >= wordSize = reverseWord w
                       | otherwise     = reverseWord w `shiftR` (wordSize - n)

ffs :: Word -> Maybe Int
ffs 0 = Nothing
ffs x = Just $! (popCount (x `xor` complement (-x)) - 1)
{-# INLINE ffs #-}

selectWord :: Word -> Word -> (Int, Word)
selectWord m x = loop 0 0 0
 where
  loop !i !ct !y
    | i >= wordSize = (ct, y)
    | testBit m i = loop (i + 1)
                         (ct + 1)
                         (if testBit x i then setBit y ct else y)
    | otherwise = loop (i + 1) ct y

#if WORD_SIZE_IN_BITS == 64

-- | Insert 0 between each consecutive bits of an input.
-- xyzw --> (x0y0, z0w0)
sparseBits :: Word -> (Word, Word)
sparseBits w = (x, y)
  where
    x = sparseBitsInternal (w .&. loMask 32)
    y = sparseBitsInternal (w `shiftR` 32)

sparseBitsInternal :: Word -> Word
sparseBitsInternal x = x4
  where
    t  = (x  `xor` (x  `shiftR` 16)) .&. 0x00000000ffff0000
    x0 = x  `xor` (t  `xor` (t  `shiftL` 16));

    t0 = (x0 `xor` (x0 `shiftR` 8)) .&. 0x0000ff000000ff00;
    x1 = x0 `xor` (t0 `xor` (t0 `shiftL` 8));
    t1 = (x1 `xor` (x1 `shiftR` 4)) .&. 0x00f000f000f000f0;
    x2 = x1 `xor` (t1 `xor` (t1 `shiftL` 4));
    t2 = (x2 `xor` (x2 `shiftR` 2)) .&. 0x0c0c0c0c0c0c0c0c;
    x3 = x2 `xor` (t2 `xor` (t2 `shiftL` 2));
    t3 = (x3 `xor` (x3 `shiftR` 1)) .&. 0x2222222222222222;
    x4 = x3 `xor` (t3 `xor` (t3 `shiftL` 1));

#else

-- | Insert 0 between each consecutive bits of an input.
-- xyzw --> (x0y0, z0w0)
sparseBits :: Word -> (Word, Word)
sparseBits w = (x, y)
  where
    x = sparseBitsInternal (w .&. loMask 16)
    y = sparseBitsInternal (w `shiftR` 16)

sparseBitsInternal :: Word -> Word
sparseBitsInternal x0 = x4
  where
    t0 = (x0 `xor` (x0 `shiftR` 8)) .&. 0x0000ff00;
    x1 = x0 `xor` (t0 `xor` (t0 `shiftL` 8));
    t1 = (x1 `xor` (x1 `shiftR` 4)) .&. 0x00f000f0;
    x2 = x1 `xor` (t1 `xor` (t1 `shiftL` 4));
    t2 = (x2 `xor` (x2 `shiftR` 2)) .&. 0x0c0c0c0c;
    x3 = x2 `xor` (t2 `xor` (t2 `shiftL` 2));
    t3 = (x3 `xor` (x3 `shiftR` 1)) .&. 0x22222222;
    x4 = x3 `xor` (t3 `xor` (t3 `shiftL` 1));

#endif

loMask :: Int -> Word
loMask n = 1 `shiftL` n - 1

hiMask :: Int -> Word
hiMask n = complement (1 `shiftL` n - 1)
