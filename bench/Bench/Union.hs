module Bench.Union
  ( benchUnion
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import qualified Data.IntSet as IS
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

randomSet :: Int -> IS.IntSet
randomSet k = IS.fromAscList (map fst (filter snd (zip [0..] (take n randomBools))))
  where
    n = 1 `shiftL` k

randomSet2 :: Int -> IS.IntSet
randomSet2 k = IS.fromAscList (map fst (filter snd (zip [0..] (take n $ drop n randomBools))))
  where
    n = 1 `shiftL` k

benchUnion :: Int -> Benchmark
benchUnion k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit"    $ nf (\x -> unionBit    (randomVec Bit k) x)    (randomVec2 Bit k)
  , bench "BitTS"  $ nf (\x -> unionBitTS  (randomVec TS.Bit k) x) (randomVec2 TS.Bit k)
  , bench "Vector" $ nf (\x -> unionVector (randomVec id k) x)     (randomVec2 id k)
  , bench "IntSet" $ nf (unionIntSet (randomSet k))                (randomSet2 k)
  ]

unionBit :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
unionBit = zipBits (.|.)

unionBitTS :: U.Vector TS.Bit -> U.Vector TS.Bit -> U.Vector TS.Bit
unionBitTS = TS.zipBits (.|.)

unionVector :: U.Vector Bool -> U.Vector Bool -> U.Vector Bool
unionVector = U.zipWith (||)

unionIntSet :: IS.IntSet -> IS.IntSet -> IS.IntSet
unionIntSet = IS.union
