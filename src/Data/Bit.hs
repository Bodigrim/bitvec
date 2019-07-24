{-# LANGUAGE CPP #-}

#ifndef BITVEC_THREADSAFE
-- |
-- Module:      Data.Bit
-- Copyright:   (c) 2019 Andrew Lelechenko, 2012-2016 James Cook
-- Licence:     PublicDomain
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- This module exposes a faster, but thread-unsafe implementation.
-- Consider using "Data.Bit.ThreadSafe", which is thread-safe, but slower (up to 2x).
module Data.Bit
#else
-- |
-- Module:      Data.Bit.ThreadSafe
-- Copyright:   (c) 2019 Andrew Lelechenko, 2012-2016 James Cook
-- Licence:     PublicDomain
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- This module exposes a slower (up to 2x), but thread-safe implementation.
-- Consider using "Data.Bit", which is faster, but thread-unsafe.
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
     , countBits
     , listBits
     , selectBits
     , excludeBits
     , bitIndex

     -- * Mutable conversions
     , castFromWordsM
     , castToWordsM
     , cloneToWordsM

     -- * Mutable operations
     , invertInPlace
     , zipInPlace
     , selectBitsInPlace
     , excludeBitsInPlace
     , reverseInPlace
    ) where

import Prelude hiding (and, or)

#ifndef BITVEC_THREADSAFE
import Data.Bit.Immutable
import Data.Bit.Internal
import Data.Bit.Mutable
#else
import Data.Bit.ImmutableTS
import Data.Bit.InternalTS
import Data.Bit.MutableTS
#endif
