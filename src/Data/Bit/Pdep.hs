-- |
-- Module:      Data.Bit.Pdep
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
--
-- | Parallel bit deposit instruction.
-- https://en.wikipedia.org/wiki/Bit_Manipulation_Instruction_Sets#Parallel_bit_deposit_and_extract

{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

module Data.Bit.Pdep
  ( pdep
  ) where

#if MIN_VERSION_base(4,11,0)

import GHC.Exts

pdep :: Word -> Word -> Word
pdep (W# src#) (W# mask#) = W# (pdep# src# mask#)

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

#endif
