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
  [ bench "Bit/bitIndex"       $ nf bitIndexBit      (randomVec Bit k)
  , bench "Bit/nthBitIndex"    $ nf nthBitIndexBit   (randomVec Bit k)
  , bench "Bit/elemIndex"      $ nf elemIndexBit     (randomVec Bit k)
  , bench "Bit.TS/bitIndex"    $ nf bitIndexBitTS    (randomVec TS.Bit k)
  , bench "Bit.TS/nthBitIndex" $ nf nthBitIndexBitTS (randomVec TS.Bit k)
  , bench "Bit.TS/elemIndex"   $ nf elemIndexBitTS   (randomVec TS.Bit k)
  , bench "Vector"             $ nf elemIndexVector  (randomVec id k)
  ]

bitIndexBit :: U.Vector Bit -> Maybe Int
bitIndexBit = bitIndex (Bit True)

nthBitIndexBit :: U.Vector Bit -> Maybe Int
nthBitIndexBit = nthBitIndex (Bit True) 1

elemIndexBit :: U.Vector Bit -> Maybe Int
elemIndexBit = U.elemIndex (Bit True)

bitIndexBitTS :: U.Vector TS.Bit -> Maybe Int
bitIndexBitTS = TS.bitIndex (TS.Bit True)

nthBitIndexBitTS :: U.Vector TS.Bit -> Maybe Int
nthBitIndexBitTS = TS.nthBitIndex (TS.Bit True) 1

elemIndexBitTS :: U.Vector TS.Bit -> Maybe Int
elemIndexBitTS = U.elemIndex (TS.Bit True)

elemIndexVector :: U.Vector Bool -> Maybe Int
elemIndexVector = U.elemIndex True
