module Data.Bit
     ( Bit(..)

     , unsafeInvert
     , invert

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
