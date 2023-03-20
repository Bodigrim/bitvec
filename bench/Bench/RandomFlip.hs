module Bench.RandomFlip
  ( benchRandomFlip
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import Data.Foldable
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed.Mutable as MU
import System.Random
import Test.Tasty.Bench

randomFlips :: [Int]
randomFlips
  = map abs
  . randoms
  . mkStdGen
  $ 42

benchRandomFlip :: Int -> Benchmark
benchRandomFlip k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit"    $ nf randomFlipBit    k
  , bench "BitTS"  $ nf randomFlipBitTS  k
  , bench "Vector" $ nf randomFlipVector k
  , bench "IntSet" $ nf randomFlipIntSet k
  ]

randomFlipBit :: Int -> Int
randomFlipBit k = runST $ do
  let n = 1 `shiftL` k
  vec <- MU.new n
  forM_ (take (mult * n) randomFlips) $
    \i -> unsafeFlipBit vec (i .&. (1 `shiftL` k - 1))
  Bit i <- MU.unsafeRead vec 0
  pure $ if i then 1 else 0

randomFlipBitTS :: Int -> Int
randomFlipBitTS k = runST $ do
  let n = 1 `shiftL` k
  vec <- MU.new n
  forM_ (take (mult * n) randomFlips) $
    \i -> TS.unsafeFlipBit vec (i .&. (1 `shiftL` k - 1))
  TS.Bit i <- MU.unsafeRead vec 0
  pure $ if i then 1 else 0

randomFlipVector :: Int -> Int
randomFlipVector k = runST $ do
  let n = 1 `shiftL` k
  vec <- MU.new n
  forM_ (take (mult * n) randomFlips) $
    \i -> MU.unsafeModify vec complement (i .&. (1 `shiftL` k - 1))
  i <- MU.unsafeRead vec 0
  pure $ if i then 1 else 0

randomFlipIntSet :: Int -> Int
randomFlipIntSet k = if IS.member 0 vec then 1 else 0
  where
    n = 1 `shiftL` k
    vec = foldl'
      (\acc i -> let j = i .&. (1 `shiftL` k - 1) in (if IS.member j acc then IS.delete else IS.insert) j acc)
      mempty
      (take (mult * n) randomFlips)

mult :: Int
mult = 100
