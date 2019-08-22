module Main where

import Gauge.Main

import Bench.BitIndex
import Bench.Intersection
import Bench.Invert
import Bench.Product
import Bench.RandomFlip
import Bench.RandomRead
import Bench.RandomWrite
import Bench.Reverse
import Bench.Union

main :: IO ()
main = defaultMain
  [ bgroup "bitIndex"     $ map benchBitIndex     [5..10]
  , bgroup "invert"       $ map benchInvert       [5..10]
  , bgroup "intersection" $ map benchIntersection [5..10]
  , bgroup "product"      $ map benchProduct      [5..10]
  , bgroup "randomWrite"  $ map benchRandomWrite  [5..10]
  , bgroup "randomFlip"   $ map benchRandomFlip   [5..10]
  , bgroup "randomRead"   $ map benchRandomRead   [5..10]
  , bgroup "reverse"      $ map benchReverse      [5..10]
  , bgroup "union"        $ map benchUnion        [5..10]
  ]
