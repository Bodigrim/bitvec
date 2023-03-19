-- |
-- Module:      Data.Bit.PdepPext
-- Copyright:   (c) 2022 Andrew Lelechenko
-- Licence:     BSD3
--
-- | Parallel bit deposit and extract instructions.
-- https://en.wikipedia.org/wiki/Bit_Manipulation_Instruction_Sets#Parallel_bit_deposit_and_extract

{-# LANGUAGE MagicHash    #-}

module Data.Bit.PdepPext
  ( pdep
  , pext
  ) where

import GHC.Exts

pdep :: Word -> Word -> Word
pdep (W# src#) (W# mask#) = W# (pdep# src# mask#)

pext :: Word -> Word -> Word
pext (W# src#) (W# mask#) = W# (pext# src# mask#)
