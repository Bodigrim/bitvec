module Bench.Remainder
  ( benchRemainder
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
randomVec f k = U.fromList (map f (take (2 * n) randomBools))
  where
    n = 1 `shiftL` k

randomVec2 :: MU.Unbox a => (Bool -> a) -> Int -> U.Vector a
randomVec2 f k = U.fromList (map f (take n $ drop (2 * n) randomBools))
  where
    n = 1 `shiftL` k

benchRemainder :: Int -> Benchmark
benchRemainder k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit/remainder"    $ nf (\x -> remainderBit    (randomVec Bit k) x)    (randomVec2 Bit k)
  , bench "Bit.TS/remainder" $ nf (\x -> remainderBitTS  (randomVec TS.Bit k) x) (randomVec2 TS.Bit k)
  ]

remainderBit :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
remainderBit xs ys = unF2Poly (toF2Poly xs `rem` toF2Poly ys)

remainderBitTS :: U.Vector TS.Bit -> U.Vector TS.Bit -> U.Vector TS.Bit
remainderBitTS xs ys = TS.unF2Poly (TS.toF2Poly xs `rem` TS.toF2Poly ys)
