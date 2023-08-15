{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}

module Bench.Union
  ( benchUnion
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as U
import Test.Tasty.Bench

import Bench.Common

benchUnion :: Int -> Benchmark
benchUnion k = bgroup (show (1 `shiftL` k :: Int))
  [ bench labelBit    $ nf (\x -> zipBits (.|.)    (randomVec Bit k) x)    (randomVec2 Bit k)
  , bench labelBitTS  $ nf (\x -> TS.zipBits (.|.) (randomVec TS.Bit k) x) (randomVec2 TS.Bit k)
  , bench labelVector $ nf (\x -> U.zipWith (||)   (randomVec id k) x)     (randomVec2 id k)
  , bench labelIntSet $ nf (IS.union (randomSet k))                        (randomSet2 k)
  ]
