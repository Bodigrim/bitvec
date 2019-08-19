{-# LANGUAGE CPP #-}

#ifndef BITVEC_THREADSAFE
module Data.Bit.F2Poly
#else
module Data.Bit.F2PolyTS
#endif
  ( F2Poly(..)
  ) where

#ifndef BITVEC_THREADSAFE
import Data.Bit.Immutable
import Data.Bit.Internal
#else
import Data.Bit.ImmutableTS
import Data.Bit.InternalTS
#endif
import Data.Bits
import Data.Coerce
import qualified Data.Vector.Unboxed as U

newtype F2Poly = F2Poly { unF2Poly :: U.Vector Bit }
  deriving (Show)

instance Eq F2Poly where
  F2Poly xs == F2Poly ys = dropWhileEnd xs == dropWhileEnd ys

instance Num F2Poly where
  (+) = coerce (zipBits0 xor)
  (-) = coerce (zipBits0 xor)
  negate = id
  abs    = id
  signum = id
  fromInteger = F2Poly . U.singleton . fromInteger
  (*) = coerce mulBits

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
    go k = fromIntegral $ countBits $ zipBits (.&.) (U.drop xFrom xs) (U.drop yFrom rys)
      where
        xFrom = max (k - (lenYs - 1)) 0
        yFrom = max 0 (lenYs - 1 - k)


dropWhileEnd
  :: U.Vector Bit
  -> U.Vector Bit
dropWhileEnd xs = U.unsafeSlice 0 (go (U.length xs)) xs
  where
    go 0 = 0
    go n = if unBit (U.unsafeIndex xs (n - 1)) then n else go (n - 1)
