{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}

module Bench.Sum
  ( benchAdd
  , benchSum
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import Data.Foldable
import Test.Tasty.Bench

import Bench.Common

benchAdd :: Int -> Benchmark
benchAdd k = bgroup (show (1 `shiftL` k :: Int))
  [ bench labelBit     $ nf (\x -> (+) (toF2Poly $ randomVec Bit k) x)    (toF2Poly $ randomVec2 Bit k)
  , bench labelBitTS   $ nf (\x -> (+) (TS.toF2Poly $ randomVec TS.Bit k) x) (TS.toF2Poly $ randomVec2 TS.Bit k)
  , bench labelInteger $ nf (\x -> xor (randomInteger k) x) (randomInteger2 k)
  ]

benchSum :: Int -> Benchmark
benchSum k = bgroup (show (1 `shiftL` k :: Int))
  [ bench labelBit     $ nf (foldl' (+) 0) [(1 :: F2Poly) .. fromInteger (1 `shiftL` k)]
  , bench labelBitTS   $ nf (foldl' (+) 0) [(1 :: TS.F2Poly) .. fromInteger (1 `shiftL` k)]
  , bench labelInteger $ nf (foldl' xor 0) [(1 :: Integer) .. fromInteger (1 `shiftL` k)]
  ]
