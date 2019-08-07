module Main where

import Gauge.Main

import Bench.BitIndex
import Bench.Invert
import Bench.RandomFlip
import Bench.RandomRead
import Bench.RandomWrite
import Bench.Reverse

main :: IO ()
main = defaultMain
  [ bgroup "bitIndex"    $ map benchBitIndex      [5..10]
  , bgroup "invert"      $ map benchInvert        [5..10]
  , bgroup "randomWrite" $ map benchRandomWrite   [5..10]
  , bgroup "randomFlip"  $ map benchRandomFlip    [5..10]
  , bgroup "randomRead"  $ map benchRandomRead    [5..10]
  , bgroup "reverse"     $ map benchReverse       [5..10]
  ]
