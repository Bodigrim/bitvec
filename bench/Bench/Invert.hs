module Bench.Invert
  ( benchInvert
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import System.Random
import Test.Tasty.Bench

randomBools :: [Bool]
randomBools
  = map (> (0 :: Int))
  . randoms
  . mkStdGen
  $ 42

randomVec :: MU.Unbox a => (Bool -> a) -> Int -> U.Vector a
randomVec f k = U.fromList (map f (take n randomBools))
  where
    n = 1 `shiftL` k

benchInvert :: Int -> Benchmark
benchInvert k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit"    $ nf invertBit    (randomVec Bit k)
  , bench "BitTS"  $ nf invertBitTS  (randomVec TS.Bit k)
  , bench "Vector" $ nf invertVector (randomVec id k)
  ]

invertBit :: U.Vector Bit -> U.Vector Bit
invertBit = invertBits

invertBitTS :: U.Vector TS.Bit -> U.Vector TS.Bit
invertBitTS = TS.invertBits

invertVector :: U.Vector Bool -> U.Vector Bool
invertVector = U.map not
