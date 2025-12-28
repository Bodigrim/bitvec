{-# LANGUAGE MagicHash #-}
{- HLINT ignore "Avoid lambda" -}

module Bench.Remainder
  ( benchRemainder
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import GHC.Exts
import GHC.Num.Integer
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
    binLog n = I# (word2Int# (integerLog2# n))
    ly = binLog y

    go z = if lz < ly then z else go (z `xor` (y `shiftL` (lz - ly)))
      where
        lz = binLog z
