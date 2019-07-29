module Main where

import Gauge.Main

import Bench.RandomFlip
import Bench.RandomRead
import Bench.RandomWrite
import Bench.Reverse

main :: IO ()
main = defaultMain
  [ bgroup "randomWrite" $ map benchRandomWrite   [5..10]
  , bgroup "randomFlip"  $ map benchRandomFlip    [5..10]
  , bgroup "randomRead"  $ map benchRandomRead    [5..10]
  , bgroup "reverse"     $ map benchReverse       [5..10]
  ]
