module Main where

import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer

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
main = defaultMain $ map (mapLeafBenchmarks addCompare)
  [ bgroup "bitIndex"     $ map benchBitIndex     [5..14]
  , bgroup "invert"       $ map benchInvert       [5..14]
  , bgroup "gcdExt"       $ map benchGCD          [5..14]
  , bgroup "intersection" $ map benchIntersection [5..14]
  , bgroup "product"      $ map benchProduct      [5..14]
  , bgroup "productShort" $ map benchProductShort [5..14]
  , bgroup "square"       $ map benchSquare       [5..14]
  , bgroup "write"        $ map benchRandomWrite  [5..14]
  , bgroup "flip"         $ map benchRandomFlip   [5..14]
  , bgroup "read"         $ map benchRandomRead   [5..14]
  , bgroup "remainder"    $ map benchRemainder    [5..14]
  , bgroup "reverse"      $ map benchReverse      [5..14]
  , bgroup "add"          $ map benchAdd          [5..14]
  , bgroup "sum"          $ map benchSum          [5..14]
  , bgroup "union"        $ map benchUnion        [5..14]
  ]

bitBenchName :: String
bitBenchName = "Bit"

addCompare :: ([String] -> Benchmark -> Benchmark)
addCompare (name : path)
  | name /= bitBenchName = bcompare (printAwkExpr (locateBenchmark (bitBenchName : path)))
addCompare _ = id
