module Data.Bit
    ( Bit(..)

     -- * Mutable conversions
     , castFromWords
     , castToWords
     , cloneToWords

     -- * Mutable operations
     , invertInPlace
     , zipInPlace
     , selectBitsInPlace
     , excludeBitsInPlace
     , reverseInPlace
    ) where

import Data.Bit.Internal
import Data.Bit.Mutable
