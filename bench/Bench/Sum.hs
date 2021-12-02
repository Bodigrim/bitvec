module Bench.Sum
  ( benchSum
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import Data.Foldable
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

randomVec2 :: MU.Unbox a => (Bool -> a) -> Int -> U.Vector a
randomVec2 f k = U.fromList (map f (take n $ drop n randomBools))
  where
    n = 1 `shiftL` k

randomInteger :: Int -> Integer
randomInteger k = toInteger $ toF2Poly $ randomVec Bit k

randomInteger2 :: Int -> Integer
randomInteger2 k = toInteger $ toF2Poly $ randomVec2 Bit k

benchSum :: Int -> Benchmark
benchSum k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit/add"     $ nf (\x -> (+) (toF2Poly $ randomVec Bit k) x)    (toF2Poly $ randomVec2 Bit k)
  , bench "Bit/sum"     $ nf (foldl' (+) 0) [(1 :: F2Poly) .. fromInteger (1 `shiftL` k)]
  , bench "Bit.TS/add"  $ nf (\x -> (+) (TS.toF2Poly $ randomVec TS.Bit k) x) (TS.toF2Poly $ randomVec2 TS.Bit k)
  , bench "Bit.TS/sum"  $ nf (foldl' (+) 0) [(1 :: TS.F2Poly) .. fromInteger (1 `shiftL` k)]
  , bench "Integer/add" $ nf (\x -> xor (randomInteger k) x) (randomInteger2 k)
  , bench "Integer/sum" $ nf (foldl' xor 0) [(1 :: Integer) .. fromInteger (1 `shiftL` k)]
  ]
