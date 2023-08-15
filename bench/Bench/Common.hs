module Bench.Common 
  ( labelBit
  , labelBitTS
  , labelVector
  , labelIntSet
  , labelInteger
  , randomBools
  , randomBools2
  , randomVec
  , randomVec2
  , randomSet
  , randomSet2
  , randomInteger
  , randomInteger2
  , randomIndices
  , randomIndicesAndBools
  ) where

import Data.Bit
import Data.Bits
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import System.Random

labelBit :: String
labelBit = "Vector Bit"

labelBitTS :: String
labelBitTS = "Vector TS.Bit"

labelVector :: String
labelVector = "Vector Bool"

labelIntSet :: String
labelIntSet = "IntSet"

labelInteger :: String
labelInteger = "Integer"

seed1 :: Int
seed1 = 42

seed2 :: Int
seed2 = 123

mkRandomBools :: Int -> [Bool]
mkRandomBools seed = map (> (0 :: Int)) $ randoms $ mkStdGen seed

randomBools :: [Bool]
randomBools = mkRandomBools seed1

randomBools2 :: [Bool]
randomBools2 = mkRandomBools seed2

mkRandomVec :: MU.Unbox a => Int -> (Bool -> a) -> Int -> U.Vector a
mkRandomVec seed f k = U.fromList $ map f $ take n $ mkRandomBools seed
  where
    n = 1 `shiftL` k

randomVec :: MU.Unbox a => (Bool -> a) -> Int -> U.Vector a
randomVec = mkRandomVec seed1

randomVec2 :: MU.Unbox a => (Bool -> a) -> Int -> U.Vector a
randomVec2 = mkRandomVec seed2

mkRandomSet :: Int -> Int -> IS.IntSet
mkRandomSet seed k = IS.fromAscList (map fst (filter snd (zip [0..] (take n (mkRandomBools seed)))))
  where
    n = 1 `shiftL` k

randomSet :: Int -> IS.IntSet
randomSet = mkRandomSet seed1

randomSet2 :: Int -> IS.IntSet
randomSet2 = mkRandomSet seed2

mkRandomInteger :: Int -> Int -> Integer
mkRandomInteger seed k = toInteger $ toF2Poly $ mkRandomVec seed Bit k

randomInteger :: Int -> Integer
randomInteger = mkRandomInteger seed1

randomInteger2 :: Int -> Integer
randomInteger2 = mkRandomInteger seed2

randomIndices :: [Int]
randomIndices = map fst randomIndicesAndBools

randomIndicesAndBools :: [(Int, Bool)]
randomIndicesAndBools
  = map (\x -> if x > 0 then (x, True) else (x .&. maxBound, False))
  . randoms
  . mkStdGen
  $ 42
