module Bench.Reverse
  ( benchReverse
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Gauge.Main
import System.Random

randomBools :: [Bool]
randomBools
  = map (\i -> if i > (0 :: Int) then True else False)
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
  , bench "Bit.TS" $ nf reverseBitTS  (randomVec TS.Bit k)
  , bench "Vector" $ nf reverseVector (randomVec id k)
  ]

reverseBit :: U.Vector Bit -> Int
reverseBit vec =
  if unBit (U.modify (\v -> reverseInPlace v >> reverseInPlace v) vec U.! (U.length vec - 1)) then 1 else 0

reverseBitTS :: U.Vector TS.Bit -> Int
reverseBitTS vec =
  if TS.unBit (U.modify (\v -> TS.reverseInPlace v >> TS.reverseInPlace v) vec U.! (U.length vec - 1)) then 1 else 0

reverseVector :: U.Vector Bool -> Int
reverseVector vec =
  if (U.force (U.reverse (U.reverse vec)) U.! (U.length vec - 1)) then 1 else 0
