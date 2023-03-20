module Bench.GCD
  ( benchGCD
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import System.Random
import Test.Tasty.Bench

randomBools :: [Bool]
randomBools = map (> (0 :: Int)) $ randoms $ mkStdGen 42

randomVec :: MU.Unbox a => (Bool -> a) -> Int -> U.Vector a
randomVec f k = U.fromList $ map f $ take n randomBools
  where
    n = 1 `shiftL` k

randomVec' :: MU.Unbox a => (Bool -> a) -> Int -> U.Vector a
randomVec' f k = U.fromList $ map f $ take n $ drop n randomBools
  where
    n = 1 `shiftL` k

benchGCD :: Int -> Benchmark
benchGCD k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit"   $ nf (uncurry    gcdExt) (   toF2Poly $ randomVec    Bit k,    toF2Poly $ randomVec'    Bit k)
  , bench "BitTS" $ nf (uncurry TS.gcdExt) (TS.toF2Poly $ randomVec TS.Bit k, TS.toF2Poly $ randomVec' TS.Bit k)
  ]
