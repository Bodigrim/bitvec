module Bench.Product
  ( benchProduct
  , benchProductShort
  , benchSquare
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

randomVec2 :: MU.Unbox a => (Bool -> a) -> Int -> U.Vector a
randomVec2 f k = U.fromList (map f (take n $ drop n randomBools))
  where
    n = 1 `shiftL` k

randomInteger :: Int -> Integer
randomInteger k = toInteger $ toF2Poly $ randomVec Bit k

randomInteger2 :: Int -> Integer
randomInteger2 k = toInteger $ toF2Poly $ randomVec2 Bit k

benchProduct :: Int -> Benchmark
benchProduct k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit"     $ nf (\x -> (*) (toF2Poly $ randomVec Bit k) x)    (toF2Poly $ randomVec2 Bit k)
  , bench "BitTS"   $ nf (\x -> (*) (TS.toF2Poly $ randomVec TS.Bit k) x) (TS.toF2Poly $ randomVec2 TS.Bit k)
  , bench "Integer" $ nf (\x -> binMul (randomInteger k) x) (randomInteger2 k)
  ]

benchProductShort :: Int -> Benchmark
benchProductShort k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit"     $ nf (\x -> (*) (toF2Poly $ randomVec Bit k) x)    (toF2Poly $ U.take 32 $ randomVec2 Bit k)
  , bench "BitTS"   $ nf (\x -> (*) (TS.toF2Poly $ randomVec TS.Bit k) x) (TS.toF2Poly $ U.take 32 $ randomVec2 TS.Bit k)
  , bench "Integer" $ nf (\x -> binMul (randomInteger k) x) ((1 `shiftL` 32 - 1) .&. randomInteger2 k)
  ]

benchSquare :: Int -> Benchmark
benchSquare k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit"     $ nf (\x -> (*) (toF2Poly $ randomVec Bit k) x)    (toF2Poly $ randomVec Bit k)
  , bench "BitTS"   $ nf (\x -> (*) (TS.toF2Poly $ randomVec TS.Bit k) x) (TS.toF2Poly $ randomVec TS.Bit k)
  , bench "Integer" $ nf (\x -> binMul (randomInteger k) x) (randomInteger k)
  ]

binMul :: Integer -> Integer -> Integer
binMul = go 0
  where
    go :: Integer -> Integer -> Integer -> Integer
    go acc _ 0 = acc
    go acc x y = go (if odd y then acc `xor` x else acc) (x `shiftL` 1) (y `shiftR` 1)
