module Bench.BitIndex
  ( benchBitIndex
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Test.Tasty.Bench

import Bench.Common

allFalseButLast :: MU.Unbox a => (Bool -> a) -> Int -> U.Vector a
allFalseButLast f k = U.generate n (\i -> f (i == n - 1))
  where
    n = 1 `shiftL` k

benchBitIndex :: Int -> Benchmark
benchBitIndex k = bgroup (show (1 `shiftL` k :: Int))
  [ bench labelBit    $ nf (bitIndex (Bit True))       (allFalseButLast Bit k)
  , bench labelBitTS  $ nf (TS.bitIndex (TS.Bit True)) (allFalseButLast TS.Bit k)
  , bench labelVector $ nf (U.elemIndex True)          (allFalseButLast id k)
  ]
