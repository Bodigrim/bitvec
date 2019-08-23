{-# LANGUAGE CPP                        #-}

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#ifndef BITVEC_THREADSAFE
module Data.Bit.F2Poly
#else
module Data.Bit.F2PolyTS
#endif
  ( F2Poly(..)
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
#ifndef BITVEC_THREADSAFE
import Data.Bit.Immutable
import Data.Bit.Internal
#else
import Data.Bit.ImmutableTS
import Data.Bit.InternalTS
#endif
import Data.Bit.Utils
import Data.Bits
import Data.Coerce
import Data.List hiding (dropWhileEnd)
import Data.Typeable
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Generics

newtype F2Poly = F2Poly { unF2Poly :: U.Vector Bit }
  deriving (Show, Typeable, Generic, NFData)

instance Eq F2Poly where
  F2Poly xs == F2Poly ys = dropWhileEnd xs == dropWhileEnd ys

instance Num F2Poly where
  (+) = coerce (zipBits0 xor)
  (-) = coerce (zipBits0 xor)
  negate = id
  abs    = id
  signum = id
  fromInteger = F2Poly . U.singleton . fromInteger
  (*) = coerce karatsuba

karatsubaThreshold :: Int
karatsubaThreshold = 4096

karatsuba :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
karatsuba xs ys
  | lenXs <= karatsubaThreshold || lenYs <= karatsubaThreshold
  = mulBits xs ys
  | otherwise = runST $ do
    zs <- MU.unsafeNew lenZs
    forM_ [0, wordSize .. lenZs - 1] $ \k -> do
      let z0  = indexWord0 zs0   k
          z11 = indexWord0 zs11 (k - m)
          z10 = indexWord0 zs0  (k - m)
          z12 = indexWord0 zs2  (k - m)
          z2  = indexWord0 zs2  (k - 2 * m)
      writeWord zs k (z0 `xor` z11 `xor` z10 `xor` z12 `xor` z2)
    U.unsafeFreeze zs
  where
    lenXs = U.length xs
    lenYs = U.length ys
    lenZs = lenXs + lenYs - 1

    m'    = ((lenXs `min` lenYs) + 1) `quot` 2
    m     = if karatsubaThreshold < wordSize then m' else m' - modWordSize m'

    xs0  = U.slice 0 m xs
    xs1  = U.slice m (lenXs - m) xs
    ys0  = U.slice 0 m ys
    ys1  = U.slice m (lenYs - m) ys

    xs01 = zipBits0 xor xs0 xs1
    ys01 = zipBits0 xor ys0 ys1
    zs0  = karatsuba xs0 ys0
    zs2  = karatsuba xs1 ys1
    zs11 = karatsuba xs01 ys01

indexWord0 :: U.Vector Bit -> Int -> Word
indexWord0 bv i
  | i <= - wordSize         = 0
  | lenI <= 0               = 0
  | i < 0, lenI >= wordSize = word0
  | i < 0                   = word0 .&. loMask lenI
  | lenI >= wordSize        = word
  | otherwise               = word .&. loMask lenI
  where
    lenI  = U.length bv - i
    word  = indexWord bv i
    word0 = indexWord bv 0 `unsafeShiftL` (- i)

mulBits :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
mulBits xs ys
  | lenXs == 0 || lenYs == 0 = U.empty
  | otherwise = U.generate lenZs go
  where
    lenXs = U.length xs
    lenYs = U.length ys
    lenZs = lenXs + lenYs - 1
    rys   = reverseBits ys

    go :: Int -> Bit
    go k = zipAndCountParityBits (U.drop xFrom xs) (U.drop yFrom rys)
      where
        xFrom = max (k - (lenYs - 1)) 0
        yFrom = max 0 (lenYs - 1 - k)

zipAndCountParityBits :: U.Vector Bit -> U.Vector Bit -> Bit
zipAndCountParityBits xs ys
  | nMod == 0 = fromIntegral $ popCnt
  | otherwise = fromIntegral $ popCnt `xor` lastPopCnt
  where
    n = min (U.length xs) (U.length ys)
    nMod = modWordSize n
    ff i = indexWord xs i .&. indexWord ys i
    popCnt = foldl' (\acc i -> acc `xor` popCount (ff i)) 0 [0, wordSize .. n - nMod - 1]
    lastPopCnt = popCount (ff (n - nMod) .&. loMask nMod)

dropWhileEnd
  :: U.Vector Bit
  -> U.Vector Bit
dropWhileEnd xs = U.unsafeSlice 0 (go (U.length xs)) xs
  where
    go 0 = 0
    go n = if unBit (U.unsafeIndex xs (n - 1)) then n else go (n - 1)
