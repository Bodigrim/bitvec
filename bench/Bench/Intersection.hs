module Bench.Intersection
  ( benchIntersection
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import qualified Data.IntSet as IS
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

randomSet :: Int -> IS.IntSet
randomSet k = IS.fromAscList (map fst (filter snd (zip [0..] (take n randomBools))))
  where
    n = 1 `shiftL` k

randomSet2 :: Int -> IS.IntSet
randomSet2 k = IS.fromAscList (map fst (filter snd (zip [0..] (take n $ drop n randomBools))))
  where
    n = 1 `shiftL` k

benchIntersection :: Int -> Benchmark
benchIntersection k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit/zipBits"    $ nf (intersectionBit    (randomVec Bit k))    (randomVec2 Bit k)
  , bench "Bit/zipWith"    $ nf (intersectionBit'   (randomVec Bit k))    (randomVec2 Bit k)
  , bench "Bit.TS/zipBits" $ nf (intersectionBitTS  (randomVec TS.Bit k)) (randomVec2 TS.Bit k)
  , bench "Bit.TS/zipWith" $ nf (intersectionBitTS' (randomVec TS.Bit k)) (randomVec2 TS.Bit k)
  , bench "Vector"         $ nf (intersectionVector (randomVec id k))     (randomVec2 id k)
  , bench "IntSet"         $ nf (intersectionIntSet (randomSet k))        (randomSet2 k)
  ]

intersectionBit :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
intersectionBit = zipBits (.&.)

intersectionBit' :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
intersectionBit' = U.zipWith (\(Bit x) (Bit y) -> Bit (x && y))

intersectionBitTS :: U.Vector TS.Bit -> U.Vector TS.Bit -> U.Vector TS.Bit
intersectionBitTS = TS.zipBits (.&.)

intersectionBitTS' :: U.Vector TS.Bit -> U.Vector TS.Bit -> U.Vector TS.Bit
intersectionBitTS' = U.zipWith (\(TS.Bit x) (TS.Bit y) -> TS.Bit (x && y))

intersectionVector :: U.Vector Bool -> U.Vector Bool -> U.Vector Bool
intersectionVector = U.zipWith (&&)

intersectionIntSet :: IS.IntSet -> IS.IntSet -> IS.IntSet
intersectionIntSet = IS.union
