{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}

module Data.Bit.Utils where

#include "MachDeps.h"

import Data.Bits
import Data.List

-- various internal utility functions and constants

lg2 :: Int -> Int
lg2 n = i
    where Just i = findIndex (>= toInteger n) (iterate (`shiftL` 1) 1)


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

-- number of bits storable in n words
nBits :: Bits a => a -> a
nBits = mulWordSize

aligned :: Int -> Bool
aligned    x = x .&. wordSizeMask == 0

notAligned :: Int -> Bool
notAligned x = x /= alignDown x

-- round a number of bits up to the nearest multiple of word size
alignUp :: Int -> Int
alignUp x
    | x == x'   = x'
    | otherwise = x' + wordSize
    where x' = alignDown x

-- round a number of bits down to the nearest multiple of word size
alignDown :: Int -> Int
alignDown x = x .&. wordSizeMaskC

-- create a mask consisting of the lower n bits
mask :: Int -> Word
mask b = m
    where
        m   | b >= finiteBitSize m = complement 0
            | b < 0                = 0
            | otherwise            = bit b - 1

masked :: Int -> Word -> Word
masked b x = x .&. mask b

isMasked :: Int -> Word -> Bool
isMasked b x = masked b x == x

-- meld 2 words by taking the low 'b' bits from 'lo' and the rest from 'hi'
meld :: Int -> Word -> Word -> Word
meld b lo hi = (lo .&. m) .|. (hi .&. complement m)
    where m = mask b

-- given a bit offset 'k' and 2 words, extract a word by taking the 'k' highest bits of the first word and the 'wordSize - k' lowest bits of the second word.
{-# INLINE extractWord #-}
extractWord :: Int -> Word -> Word -> Word
extractWord k lo hi = (lo `shiftR` k) .|. (hi `shiftL` (wordSize - k))

-- given a bit offset 'k', 2 words 'lo' and 'hi' and a word 'x', overlay 'x' onto 'lo' and 'hi' at the position such that (k `elem` [0..wordSize] ==> uncurry (extractWord k) (spliceWord k lo hi x) == x) and (k `elem` [0..wordSize] ==> spliceWord k lo hi (extractWord k lo hi) == (lo,hi))
{-# INLINE spliceWord #-}
spliceWord :: Int -> Word -> Word -> Word -> (Word, Word)
spliceWord k lo hi x =
    ( meld k lo (x `shiftL` k)
    , meld k (x `shiftR` (wordSize - k)) hi
    )

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
reversePartialWord n w
    | n >= wordSize = reverseWord w
    | otherwise     = reverseWord w `shiftR` (wordSize - n)

diff :: Bits a => a -> a -> a
diff w1 w2 = w1 .&. complement w2

ffs :: Word -> Maybe Int
ffs 0 = Nothing
ffs x = Just $! (popCount (x `xor` complement (-x)) - 1)
{-# INLINE ffs #-}

-- TODO: this can probably be faster
-- the interface is very specialized here; 'j' is an offset to add to every bit index and the result is a difference list
bitsInWord :: Int -> Word -> [Int] -> [Int]
bitsInWord j = loop id
    where
        loop is !w = case ffs w of
            Nothing -> is
            Just i  -> loop (is . (j + i :)) (clearBit w i)

-- TODO: faster!
selectWord :: Word -> Word -> (Int, Word)
selectWord m x = loop 0 0 0
    where
        loop !i !ct !y
            | i >= wordSize = (ct, y)
            | testBit m i   = loop (i+1) (ct+1) (if testBit x i then setBit y ct else y)
            | otherwise     = loop (i+1) ct y

loMask :: Int -> Word
loMask n = 1 `shiftL` n - 1

hiMask :: Int -> Word
hiMask n = complement (1 `shiftL` n - 1)
