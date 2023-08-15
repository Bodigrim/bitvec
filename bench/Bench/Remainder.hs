{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}

module Bench.Remainder
  ( benchRemainder
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import GHC.Exts
#ifdef MIN_VERSION_ghc_bignum
import GHC.Num.Integer
#else
import GHC.Integer.Logarithms
#endif
import Test.Tasty.Bench

import Bench.Common

benchRemainder :: Int -> Benchmark
benchRemainder k = bgroup (show (1 `shiftL` k :: Int))
  [ bench labelBit     $ nf (\x -> rem (toF2Poly $ randomVec Bit (2 * k)) x) (toF2Poly $ randomVec2 Bit k)
  , bench labelBitTS   $ nf (\x -> rem (TS.toF2Poly $ randomVec TS.Bit (2 * k)) x) (TS.toF2Poly $ randomVec2 TS.Bit k)
  , bench labelInteger $ nf (\x -> binRem (randomInteger (2 * k)) x) (randomInteger2 k)
  ]

binRem :: Integer -> Integer -> Integer
binRem x y = go x
  where
#ifdef MIN_VERSION_ghc_bignum
    binLog n = I# (word2Int# (integerLog2# n))
#else
    binLog n = I# (integerLog2# n)
#endif
    ly = binLog y

    go z = if lz < ly then z else go (z `xor` (y `shiftL` (lz - ly)))
      where
        lz = binLog z
