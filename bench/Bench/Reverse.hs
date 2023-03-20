module Bench.Reverse
  ( benchReverse
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

benchReverse :: Int -> Benchmark
benchReverse k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit"    $ nf reverseBit    (randomVec Bit k)
  , bench "BitTS"  $ nf reverseBitTS  (randomVec TS.Bit k)
  , bench "Vector" $ nf reverseVector (randomVec id k)
  ]

reverseBit :: U.Vector Bit -> U.Vector Bit
reverseBit = reverseBits

reverseBitTS :: U.Vector TS.Bit -> U.Vector TS.Bit
reverseBitTS = TS.reverseBits

reverseVector :: U.Vector Bool -> U.Vector Bool
reverseVector = U.reverse
