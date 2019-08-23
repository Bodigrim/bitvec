{-# LANGUAGE CPP #-}

#ifndef BITVEC_THREADSAFE
-- |
-- Module:      Data.Bit
-- Copyright:   (c) 2019 Andrew Lelechenko, 2012-2016 James Cook
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- This module exposes an interface with thread-unsafe writes and flips.
-- Consider using "Data.Bit.ThreadSafe", which is thread-safe, but slower (up to 20%).
module Data.Bit
#else
-- |
-- Module:      Data.Bit.ThreadSafe
-- Copyright:   (c) 2019 Andrew Lelechenko, 2012-2016 James Cook
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- This module exposes an interface with thread-safe writes and flips.
-- Consider using "Data.Bit", which is faster (up to 20%), but thread-unsafe.
module Data.Bit.ThreadSafe
#endif
  ( Bit(..)

  , unsafeFlipBit
  , flipBit

  -- * Immutable conversions
  , castFromWords
  , castToWords
  , cloneToWords

  -- * Immutable operations
  , zipBits
  , invertBits
  , reverseBits
  , bitIndex
  , nthBitIndex
  , countBits
  , listBits
  , selectBits
  , excludeBits

  -- * Mutable conversions
  , castFromWordsM
  , castToWordsM
  , cloneToWordsM

  -- * Mutable operations
  , zipInPlace
  , invertInPlace
  , reverseInPlace
  , selectBitsInPlace
  , excludeBitsInPlace

  -- * F(2) polynomials
  , F2Poly
  , unF2Poly
  , toF2Poly
  ) where

import Prelude hiding (and, or)

#ifndef BITVEC_THREADSAFE
import Data.Bit.F2Poly
import Data.Bit.Immutable
import Data.Bit.Internal
import Data.Bit.Mutable
#else
import Data.Bit.F2PolyTS
import Data.Bit.ImmutableTS
import Data.Bit.InternalTS
import Data.Bit.MutableTS
#endif
