module Bench.RandomRead
  ( benchRandomRead
  ) where

import Control.Monad.ST
import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
-- import qualified Data.IntSet as IS
-- import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import System.Random
import Test.Tasty.Bench

randomVec :: [Bool]
randomVec
  = map (> (0 :: Int))
  . randoms
  . mkStdGen
  $ 42

randomReads :: [Int]
randomReads
  = map abs
  . randoms
  . mkStdGen
  $ 42

benchRandomRead :: Int -> Benchmark
benchRandomRead k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit"    $ nf randomReadBit    k
  , bench "BitTS"  $ nf randomReadBitTS  k
  , bench "Vector" $ nf randomReadVector k
  -- , bench "IntSet" $ nf randomReadIntSet k
  ]

randomReadBit :: Int -> Int
randomReadBit k = runST $ do
  let n = 1 `shiftL` k
  vec <- U.unsafeThaw (U.fromList (map Bit $ take n randomVec))
  let go acc [] = pure acc
      go acc (i : is) = do
        Bit b <- MU.unsafeRead vec (i .&. (1 `shiftL` k - 1))
        go (acc + if b then 1 else 0) is
  go 0 (take (mult * n) randomReads)

randomReadBitTS :: Int -> Int
randomReadBitTS k = runST $ do
  let n = 1 `shiftL` k
  vec <- U.unsafeThaw (U.fromList (map TS.Bit $ take n randomVec))
  let go acc [] = pure acc
      go acc (i : is) = do
        TS.Bit b <- MU.unsafeRead vec (i .&. (1 `shiftL` k - 1))
        go (acc + if b then 1 else 0) is
  go 0 (take (mult * n) randomReads)

randomReadVector :: Int -> Int
randomReadVector k = runST $ do
  let n = 1 `shiftL` k
  vec <- U.unsafeThaw (U.fromList (take n randomVec))
  let go acc [] = pure acc
      go acc (i : is) = do
        b <- MU.unsafeRead vec (i .&. (1 `shiftL` k - 1))
        go (acc + if b then 1 else 0) is
  go 0 (take (mult * n) randomReads)

-- randomReadIntSet :: Int -> Int
-- randomReadIntSet k = foldl' (+) 0 [ doRead (c + i `shiftL` 1 - i - c) | c <- [0 .. mult - 1], i <- randomReads ]
--   where
--     n = 1 `shiftL` k
--     vec = IS.fromDistinctAscList $ map fst $ filter snd
--       $ zip [0..] $ take n randomVec
--     doRead i = if IS.member (i .&. (1 `shiftL` k - 1)) vec then 1 else 0

mult :: Int
mult = 100
