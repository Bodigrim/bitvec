module Bench.Invert
  ( benchInvert
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import qualified Data.Vector.Unboxed as U
import Test.Tasty.Bench

import Bench.Common

benchInvert :: Int -> Benchmark
benchInvert k = bgroup (show (1 `shiftL` k :: Int))
  [ bench labelBit    $ nf invertBits    (randomVec Bit k)
  , bench labelBitTS  $ nf TS.invertBits (randomVec TS.Bit k)
  , bench labelVector $ nf (U.map not)   (randomVec id k)
  ]
