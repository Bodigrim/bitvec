-- |
-- Module:      Data.Bit
-- Copyright:   (c) 2019 Andrew Lelechenko, 2012-2016 James Cook
-- Licence:     PublicDomain
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
module Data.Bit
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

import Data.Bit.Immutable
import Data.Bit.Internal
import Data.Bit.Mutable
