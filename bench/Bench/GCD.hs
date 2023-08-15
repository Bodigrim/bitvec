module Bench.GCD
  ( benchGCD
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import Test.Tasty.Bench

import Bench.Common

benchGCD :: Int -> Benchmark
benchGCD k = bgroup (show (1 `shiftL` k :: Int))
  [ bench labelBit   $ nf (uncurry    gcdExt) (   toF2Poly $ randomVec    Bit k,    toF2Poly $ randomVec2    Bit k)
  , bench labelBitTS $ nf (uncurry TS.gcdExt) (TS.toF2Poly $ randomVec TS.Bit k, TS.toF2Poly $ randomVec2 TS.Bit k)
  ]
