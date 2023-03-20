module Bench.BitIndex
  ( benchBitIndex
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Test.Tasty.Bench

randomVec :: MU.Unbox a => (Bool -> a) -> Int -> U.Vector a
randomVec f k = U.generate n (\i -> f (i == n - 1))
  where
    n = 1 `shiftL` k

benchBitIndex :: Int -> Benchmark
benchBitIndex k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit"    $ nf bitIndexBit      (randomVec Bit k)
  , bench "BitTS"  $ nf bitIndexBitTS    (randomVec TS.Bit k)
  , bench "Vector" $ nf elemIndexVector  (randomVec id k)
  ]

bitIndexBit :: U.Vector Bit -> Maybe Int
bitIndexBit = bitIndex (Bit True)

bitIndexBitTS :: U.Vector TS.Bit -> Maybe Int
bitIndexBitTS = TS.bitIndex (TS.Bit True)

elemIndexVector :: U.Vector Bool -> Maybe Int
elemIndexVector = U.elemIndex True
