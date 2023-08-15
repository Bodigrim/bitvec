module Bench.RandomWrite
  ( benchRandomWrite
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import Data.Foldable
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed.Mutable as MU
import Test.Tasty.Bench

import Bench.Common

benchRandomWrite :: Int -> Benchmark
benchRandomWrite k = bgroup (show (1 `shiftL` k :: Int))
  [ bench labelBit    $ nf randomWriteBit    k
  , bench labelBitTS  $ nf randomWriteBitTS  k
  , bench labelVector $ nf randomWriteVector k
  , bench labelIntSet $ nf randomWriteIntSet k
  ]

randomWriteBit :: Int -> Int
randomWriteBit k = runST $ do
  let n = 1 `shiftL` k
  vec <- MU.new n
  forM_ (take (mult * n) randomIndicesAndBools) $
    \(i, b) -> MU.unsafeWrite vec (i .&. (1 `shiftL` k - 1)) (Bit b)
  Bit i <- MU.unsafeRead vec 0
  pure $ if i then 1 else 0

randomWriteBitTS :: Int -> Int
randomWriteBitTS k = runST $ do
  let n = 1 `shiftL` k
  vec <- MU.new n
  forM_ (take (mult * n) randomIndicesAndBools) $
    \(i, b) -> MU.unsafeWrite vec (i .&. (1 `shiftL` k - 1)) (TS.Bit b)
  TS.Bit i <- MU.unsafeRead vec 0
  pure $ if i then 1 else 0

randomWriteVector :: Int -> Int
randomWriteVector k = runST $ do
  let n = 1 `shiftL` k
  vec <- MU.new n
  forM_ (take (mult * n) randomIndicesAndBools) $
    \(i, b) -> MU.unsafeWrite vec (i .&. (1 `shiftL` k - 1)) b
  i <- MU.unsafeRead vec 0
  pure $ if i then 1 else 0

randomWriteIntSet :: Int -> Int
randomWriteIntSet k = if IS.member 0 vec then 1 else 0
  where
    n = 1 `shiftL` k
    vec = foldl'
      (\acc (i, b) -> (if b then IS.insert else IS.delete) (i .&. (1 `shiftL` k - 1)) acc)
      mempty
      (take (mult * n) randomIndicesAndBools)

mult :: Int
mult = 100
