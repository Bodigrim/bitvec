module Main where

import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer

import Bench.BitIndex
import Bench.Common
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
main = defaultMain $ map (mapLeafBenchmarks addCompare)
  [ bgroup "add"          $ map benchAdd          [5..14]
  , bgroup "bitIndex"     $ map benchBitIndex     [5..14]
  , bgroup "flip"         $ map benchRandomFlip   [5..14]
  , bgroup "gcdExt"       $ map benchGCD          [5..14]
  , bgroup "intersection" $ map benchIntersection [5..14]
  , bgroup "invert"       $ map benchInvert       [5..14]
  , bgroup "product"      $ map benchProduct      [5..14]
  , bgroup "productShort" $ map benchProductShort [5..14]
  , bgroup "read"         $ map benchRandomRead   [5..14]
  , bgroup "remainder"    $ map benchRemainder    [5..11]
  , bgroup "reverse"      $ map benchReverse      [5..14]
  , bgroup "square"       $ map benchSquare       [5..14]
  , bgroup "sum"          $ map benchSum          [5..14]
  , bgroup "union"        $ map benchUnion        [5..20]
  , bgroup "write"        $ map benchRandomWrite  [5..14]
  ]

addCompare :: ([String] -> Benchmark -> Benchmark)
addCompare (name : path)
  | name /= labelBit = bcompare (printAwkExpr (locateBenchmark (labelBit : path)))
addCompare _ = id
