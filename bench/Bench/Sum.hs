module Bench.Sum
  ( benchSum
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

randomVec2 :: MU.Unbox a => (Bool -> a) -> Int -> U.Vector a
randomVec2 f k = U.fromList (map f (take n $ drop n randomBools))
  where
    n = 1 `shiftL` k

benchSum :: Int -> Benchmark
benchSum k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit/add"    $ nf (\x -> sumBit    (randomVec Bit k) x)    (randomVec2 Bit k)
  , bench "Bit/sum"    $ nf sum [(1 :: F2Poly) .. fromInteger (1 `shiftL` k)]
  , bench "Bit.TS/add" $ nf (\x -> sumBitTS  (randomVec TS.Bit k) x) (randomVec2 TS.Bit k)
  , bench "Bit.TS/sum" $ nf sum [(1 :: TS.F2Poly) .. fromInteger (1 `shiftL` k)]
  ]

sumBit :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
sumBit xs ys = unF2Poly (toF2Poly xs + toF2Poly ys)

sumBitTS :: U.Vector TS.Bit -> U.Vector TS.Bit -> U.Vector TS.Bit
sumBitTS xs ys = TS.unF2Poly (TS.toF2Poly xs + TS.toF2Poly ys)
