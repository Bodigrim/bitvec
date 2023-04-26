{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK prune #-}

#ifndef BITVEC_THREADSAFE
-- |
-- Module:      Data.Bit
-- Copyright:   (c) 2019-2022 Andrew Lelechenko, 2012-2016 James Cook
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- This module exposes an interface with non-thread-safe writes and flips.
-- Additionally, concurrently modifying non-intersecting slices of the same underlying array
-- may lead to unexpected results.
-- Consider using "Data.Bit.ThreadSafe", which is thread-safe, but slower
-- (usually 10-20%, up to 50% for short vectors).
--
-- @since 0.1
module Data.Bit
#else
-- |
-- Module:      Data.Bit.ThreadSafe
-- Copyright:   (c) 2019-2022 Andrew Lelechenko, 2012-2016 James Cook
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- This module exposes an interface with thread-safe writes and flips.
-- Additionally, concurrently modifying non-intersecting slices of the same underlying array
-- works as expected. However, operations that affect multiple elements are not
-- guaranteed to be atomic.
-- Consider using "Data.Bit", which is faster
-- (usually 10-20%, up to 50% for short vectors), but not thread-safe.
--
-- @since 1.0
module Data.Bit.ThreadSafe
#endif
  ( Bit(..)
  , U.Vector(BitVec)
  , U.MVector(BitMVec)

  , unsafeFlipBit
  , flipBit

  -- * Immutable conversions
  , castFromWords
  , castToWords
  , cloneToWords

  , castFromWords8
  , castToWords8
  , cloneToWords8

  , cloneFromByteString
  , cloneToByteString

  -- * Immutable operations
  , zipBits
  , mapBits
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
  , mapInPlace
  , invertInPlace
  , reverseInPlace
  , selectBitsInPlace
  , excludeBitsInPlace

  -- * Binary polynomials
  , F2Poly
  , unF2Poly
  , toF2Poly
  , gcdExt
  ) where

import Prelude hiding (and, or)
import qualified Data.Vector.Unboxed as U

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
