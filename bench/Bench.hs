module Main where

import Test.Tasty.Bench

import Bench.BitIndex
import Bench.GCD
import Bench.Intersection
import Bench.Invert
import Bench.Product
import Bench.RandomFlip
import Bench.RandomRead
import Bench.RandomWrite
import Bench.Remainder
import Bench.Reverse
import Bench.Sum
import Bench.Union

main :: IO ()
main = defaultMain
  [ bgroup "bitIndex"     $ map benchBitIndex     [5..14]
  , bgroup "invert"       $ map benchInvert       [5..14]
  , bgroup "gcdExt"       $ map benchGCD          [5..14]
  , bgroup "intersection" $ map benchIntersection [5..14]
  , bgroup "product"      $ map benchProduct      [5..14]
  , bgroup "randomWrite"  $ map benchRandomWrite  [5..14]
  , bgroup "randomFlip"   $ map benchRandomFlip   [5..14]
  , bgroup "randomRead"   $ map benchRandomRead   [5..14]
  , bgroup "remainder"    $ map benchRemainder    [5..14]
  , bgroup "reverse"      $ map benchReverse      [5..14]
  , bgroup "sum"          $ map benchSum          [5..14]
  , bgroup "union"        $ map benchUnion        [5..14]
  ]
