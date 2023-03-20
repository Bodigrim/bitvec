{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MagicHash                  #-}

module Data.Bit.Utils
  ( lgWordSize
  , modWordSize
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
  , fromPrimVector
  , toPrimVector
  ) where

import Data.Bits
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Base as UB
#if __GLASGOW_HASKELL__ >= 810
import GHC.Exts
#endif

import Data.Bit.PdepPext

-- | The number of bits in a 'Word'.  A handy constant to have around when defining 'Word'-based bulk operations on bit vectors.
wordSize :: Int
wordSize = finiteBitSize (0 :: Word)

-- | The base 2 logarithm of 'wordSize'.
lgWordSize :: Int
lgWordSize = case wordSize of
  32 -> 5
  64 -> 6
  _  -> error "lgWordSize: unknown architecture"

wordSizeMask :: Int
wordSizeMask = wordSize - 1

wordSizeMaskC :: Int
wordSizeMaskC = complement wordSizeMask

divWordSize :: Bits a => a -> a
divWordSize x = unsafeShiftR x lgWordSize
{-# INLINE divWordSize #-}

modWordSize :: Int -> Int
modWordSize x = x .&. (wordSize - 1)
{-# INLINE modWordSize #-}

mulWordSize :: Bits a => a -> a
mulWordSize x = unsafeShiftL x lgWordSize
{-# INLINE mulWordSize #-}

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
mask b
  | b >= wordSize = complement 0
  | b < 0         = 0
  | otherwise     = bit b - 1

masked :: Int -> Word -> Word
masked b x = x .&. mask b

-- meld 2 words by taking the low 'b' bits from 'lo' and the rest from 'hi'
meld :: Int -> Word -> Word -> Word
meld b lo hi = (lo .&. m) .|. (hi .&. complement m) where m = mask b
{-# INLINE meld #-}

#if __GLASGOW_HASKELL__ >= 810

reverseWord :: Word -> Word
reverseWord (W# w#) = W# (bitReverse# w#)

#else

reverseWord :: Word -> Word
reverseWord = case wordSize of
  32 -> reverseWord32
  64 -> reverseWord64
  _  -> error "reverseWord: unknown architecture"

reverseWord64 :: Word -> Word
reverseWord64 x0 = x6
 where
  x1 = ((x0 .&. 0x5555555555555555) `shiftL`  1) .|. ((x0 .&. 0xAAAAAAAAAAAAAAAA) `shiftR`  1)
  x2 = ((x1 .&. 0x3333333333333333) `shiftL`  2) .|. ((x1 .&. 0xCCCCCCCCCCCCCCCC) `shiftR`  2)
  x3 = ((x2 .&. 0x0F0F0F0F0F0F0F0F) `shiftL`  4) .|. ((x2 .&. 0xF0F0F0F0F0F0F0F0) `shiftR`  4)
  x4 = ((x3 .&. 0x00FF00FF00FF00FF) `shiftL`  8) .|. ((x3 .&. 0xFF00FF00FF00FF00) `shiftR`  8)
  x5 = ((x4 .&. 0x0000FFFF0000FFFF) `shiftL` 16) .|. ((x4 .&. 0xFFFF0000FFFF0000) `shiftR` 16)
  x6 = ((x5 .&. 0x00000000FFFFFFFF) `shiftL` 32) .|. ((x5 .&. 0xFFFFFFFF00000000) `shiftR` 32)

reverseWord32 :: Word -> Word
reverseWord32 x0 = x5
 where
  x1 = ((x0 .&. 0x55555555) `shiftL`  1) .|. ((x0 .&. 0xAAAAAAAA) `shiftR`  1)
  x2 = ((x1 .&. 0x33333333) `shiftL`  2) .|. ((x1 .&. 0xCCCCCCCC) `shiftR`  2)
  x3 = ((x2 .&. 0x0F0F0F0F) `shiftL`  4) .|. ((x2 .&. 0xF0F0F0F0) `shiftR`  4)
  x4 = ((x3 .&. 0x00FF00FF) `shiftL`  8) .|. ((x3 .&. 0xFF00FF00) `shiftR`  8)
  x5 = ((x4 .&. 0x0000FFFF) `shiftL` 16) .|. ((x4 .&. 0xFFFF0000) `shiftR` 16)

#endif

reversePartialWord :: Int -> Word -> Word
reversePartialWord n w
  | n >= wordSize = reverseWord w
  | otherwise     = reverseWord w `shiftR` (wordSize - n)

ffs :: Word -> Maybe Int
ffs 0 = Nothing
ffs x = Just $! (popCount (x `xor` complement (-x)) - 1)
{-# INLINE ffs #-}

selectWord :: Word -> Word -> (Int, Word)
selectWord msk src = (popCount msk, pext src msk)
{-# INLINE selectWord #-}

-- | Insert 0 between each consecutive bits of an input.
-- xyzw --> (x0y0, z0w0)
sparseBits :: Word -> (Word, Word)
sparseBits = case wordSize of
  32 -> sparseBits32
  64 -> sparseBits64
  _  -> error "sparseBits: unknown architecture"

sparseBits64 :: Word -> (Word, Word)
sparseBits64 w = (x, y)
  where
    x = sparseBitsInternal64 (w .&. loMask 32)
    y = sparseBitsInternal64 (w `shiftR` 32)

sparseBitsInternal64 :: Word -> Word
sparseBitsInternal64 x = x4
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

sparseBits32 :: Word -> (Word, Word)
sparseBits32 w = (x, y)
  where
    x = sparseBitsInternal32 (w .&. loMask 16)
    y = sparseBitsInternal32 (w `shiftR` 16)

sparseBitsInternal32 :: Word -> Word
sparseBitsInternal32 x0 = x4
  where
    t0 = (x0 `xor` (x0 `shiftR` 8)) .&. 0x0000ff00;
    x1 = x0 `xor` (t0 `xor` (t0 `shiftL` 8));
    t1 = (x1 `xor` (x1 `shiftR` 4)) .&. 0x00f000f0;
    x2 = x1 `xor` (t1 `xor` (t1 `shiftL` 4));
    t2 = (x2 `xor` (x2 `shiftR` 2)) .&. 0x0c0c0c0c;
    x3 = x2 `xor` (t2 `xor` (t2 `shiftL` 2));
    t3 = (x3 `xor` (x3 `shiftR` 1)) .&. 0x22222222;
    x4 = x3 `xor` (t3 `xor` (t3 `shiftL` 1));

loMask :: Int -> Word
loMask n = 1 `unsafeShiftL` n - 1
{-# INLINE loMask #-}

hiMask :: Int -> Word
hiMask n = complement (1 `unsafeShiftL` n - 1)
{-# INLINE hiMask #-}

fromPrimVector :: P.Vector Word -> U.Vector Word
fromPrimVector = UB.V_Word
{-# INLINE fromPrimVector #-}

toPrimVector :: U.Vector Word -> P.Vector Word
toPrimVector (UB.V_Word ws) = ws
{-# INLINE toPrimVector #-}
