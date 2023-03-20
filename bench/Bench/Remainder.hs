{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module Bench.Remainder
  ( benchRemainder
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Exts
#ifdef MIN_VERSION_ghc_bignum
import GHC.Num.Integer
#else
import GHC.Integer.Logarithms
#endif
import System.Random
import Test.Tasty.Bench

randomBools :: [Bool]
randomBools
  = map (> (0 :: Int))
  . randoms
  . mkStdGen
  $ 42

randomVec :: MU.Unbox a => (Bool -> a) -> Int -> U.Vector a
randomVec f k = U.fromList (map f (take (2 * n) randomBools))
  where
    n = 1 `shiftL` k

randomVec2 :: MU.Unbox a => (Bool -> a) -> Int -> U.Vector a
randomVec2 f k = U.fromList (map f (take n $ drop (2 * n) randomBools))
  where
    n = 1 `shiftL` k

randomInteger :: Int -> Integer
randomInteger k = toInteger $ toF2Poly $ randomVec Bit k

randomInteger2 :: Int -> Integer
randomInteger2 k = toInteger $ toF2Poly $ randomVec2 Bit k

benchRemainder :: Int -> Benchmark
benchRemainder k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit"     $ nf (\x -> rem (toF2Poly $ randomVec Bit k) x) (toF2Poly $ randomVec2 Bit k)
  , bench "BitTS"   $ nf (\x -> rem (TS.toF2Poly $ randomVec TS.Bit k) x) (TS.toF2Poly $ randomVec2 TS.Bit k)
  , bench "Integer" $ nf (\x -> binRem (randomInteger k) x) (randomInteger2 k)
  ]

binRem :: Integer -> Integer -> Integer
binRem x y = go x
  where
#ifdef MIN_VERSION_ghc_bignum
    binLog n = I# (word2Int# (integerLog2# n))
#else
    binLog n = I# (integerLog2# n)
#endif
    ly = binLog y

    go z = if lz < ly then z else go (z `xor` (y `shiftL` (lz - ly)))
      where
        lz = binLog z
