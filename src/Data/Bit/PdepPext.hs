-- |
-- Module:      Data.Bit.PdepPext
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
--
-- | Parallel bit deposit and extract instructions.
-- https://en.wikipedia.org/wiki/Bit_Manipulation_Instruction_Sets#Parallel_bit_deposit_and_extract

{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

module Data.Bit.PdepPext
  ( pdep
  , pext
  ) where

#if MIN_VERSION_base(4,11,0)

import GHC.Exts

pdep :: Word -> Word -> Word
pdep (W# src#) (W# mask#) = W# (pdep# src# mask#)

pext :: Word -> Word -> Word
pext (W# src#) (W# mask#) = W# (pext# src# mask#)

#else

import Data.Bits

pdep :: Word -> Word -> Word
pdep = go 0
  where
    go :: Word -> Word -> Word -> Word
    go result _ 0 = result
    go result src mask = go newResult newSrc newMask
      where
        lowest    = 1 `shiftL` countTrailingZeros mask
        newResult = if src .&. 1 == 0 then result else result .|. lowest
        newSrc    = src `shiftR` 1
        newMask   = mask .&. complement lowest

pext :: Word -> Word -> Word
pext src mask = loop 0 0 0
  where
    loop i count acc
      | i >= finiteBitSize (0 :: Word)
      = acc
      | testBit mask i
      = loop (i + 1) (count + 1) (if testBit src i then setBit acc count else acc)
      | otherwise
      = loop (i + 1) count acc

#endif
