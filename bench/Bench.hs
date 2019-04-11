module Main where

import Control.Monad
import Control.Monad.ST
import Data.Bit
import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as MU
import Gauge.Main

main :: IO ()
main = defaultMain
  [ bgroup "randomWrite" $ map benchRandomWrite [5..10]
  ]

benchRandomWrite :: Int -> Benchmark
benchRandomWrite k = bench (show (2 ^ k)) $ nf doRandomWrite k

doRandomWrite :: Int -> Int
doRandomWrite k = runST $ do
  let n = 2 ^ k
      ixs = scanl xor 0 [0..n-1]
      vals = take 100 $ cycle [Bit True, Bit False]
  vec <- MU.new n
  forM_ vals $ \v -> forM_ ixs $ \i -> MU.unsafeWrite vec i v
  Bit i <- MU.unsafeRead vec 0
  pure $ if i then 1 else 0
