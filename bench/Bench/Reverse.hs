module Bench.Reverse
  ( benchReverse
  ) where

import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Gauge.Main
import System.Random

randomBools :: [Bool]
randomBools
  = map (\i -> if i > (0 :: Int) then True else False)
  . randoms
  . mkStdGen
  $ 42

randomVec :: MU.Unbox a => (Bool -> a) -> Int -> U.Vector a
randomVec f k = U.fromList (map f (take n randomBools))
  where
    n = 1 `shiftL` k

benchReverse :: Int -> Benchmark
benchReverse k = bgroup (show (1 `shiftL` k :: Int))
  [ bench "Bit/reverseInPlace"    $ nf reverseBit    (randomVec Bit k)
  , bench "Bit/reverse"           $ nf reverseBit'   (randomVec Bit k)
  , bench "Bit.TS/reverseInPlace" $ nf reverseBitTS  (randomVec TS.Bit k)
  , bench "Bit.TS/reverse"        $ nf reverseBitTS' (randomVec TS.Bit k)
  , bench "Vector"                $ nf reverseVector (randomVec id k)
  ]

reverseBit :: U.Vector Bit -> U.Vector Bit
reverseBit = U.modify reverseInPlace

reverseBit' :: U.Vector Bit -> U.Vector Bit
reverseBit' = U.reverse

reverseBitTS :: U.Vector TS.Bit -> U.Vector TS.Bit
reverseBitTS = U.modify TS.reverseInPlace

reverseBitTS' :: U.Vector TS.Bit -> U.Vector TS.Bit
reverseBitTS' = U.reverse

reverseVector :: U.Vector Bool -> U.Vector Bool
reverseVector = U.reverse
