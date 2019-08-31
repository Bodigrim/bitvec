{-# LANGUAGE CPP                        #-}

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE RankNTypes                 #-}

#ifndef BITVEC_THREADSAFE
module Data.Bit.F2Poly
#else
module Data.Bit.F2PolyTS
#endif
  ( F2Poly
  , unF2Poly
  , toF2Poly
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.ST
#ifndef BITVEC_THREADSAFE
import Data.Bit.Immutable
import Data.Bit.Internal
import Data.Bit.Mutable
#else
import Data.Bit.ImmutableTS
import Data.Bit.InternalTS
import Data.Bit.MutableTS
#endif
import Data.Bit.Utils
import Data.Bits
import Data.Coerce
import Data.List hiding (dropWhileEnd)
import Data.Typeable
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Generics

#if UseIntegerGmp
import Data.Primitive.ByteArray
import qualified Data.Vector.Primitive as P
import GHC.Exts
import GHC.Integer.GMP.Internals
import GHC.Integer.Logarithms
import Unsafe.Coerce
#endif

-- | Binary polynomials of one variable, backed
-- by an unboxed 'Data.Vector.Unboxed.Vector' 'Bit'.
--
-- Polynomials are stored normalized, without leading zero coefficients.
--
-- 'Ord' instance does not make much sense mathematically,
-- it is defined only for the sake of 'Data.Set.Set', 'Data.Map.Map', etc.
--
-- >>> :set -XBinaryLiterals
-- >>> -- (1 + x) (1 + x + x^2) = 1 + x^3 (mod 2)
-- >>> 0b11 * 0b111 :: F2Poly
-- F2Poly {unF2Poly = [1,0,0,1]}
newtype F2Poly = F2Poly {
  unF2Poly :: U.Vector Bit
  -- ^ Convert 'F2Poly' to a vector of coefficients
  -- (first element corresponds to a constant term).
  }
  deriving (Eq, Ord, Show, Typeable, Generic, NFData)

-- | Make 'F2Poly' from a list of coefficients
-- (first element corresponds to a constant term).
toF2Poly :: U.Vector Bit -> F2Poly
toF2Poly xs = F2Poly $ dropWhileEnd $ castFromWords $ cloneToWords xs

-- | Addition and multiplication are evaluated modulo 2.
--
-- 'abs' = 'id' and 'signum' = 'const' 1.
--
-- 'fromInteger' converts a binary polynomial, encoded as 'Integer',
-- to 'F2Poly' encoding.
instance Num F2Poly where
  (+) = coerce xorBits
  (-) = coerce xorBits
  negate = id
  abs    = id
  signum = const (F2Poly (U.singleton (Bit True)))
  (*) = coerce ((dropWhileEnd .) . karatsuba)
#if UseIntegerGmp
  fromInteger !n = case n of
    S# i#   -> F2Poly $ BitVec 0 (wordSize - I# (word2Int# (clz# (int2Word# i#))))
                      $ fromBigNat $ wordToBigNat (int2Word# i#)
    Jp# bn# -> F2Poly $ BitVec 0 (I# (integerLog2# n) + 1) $ fromBigNat bn#
    Jn#{}   -> error "F2Poly.fromInteger: argument must be non-negative"
#else
  fromInteger = F2Poly . dropWhileEnd . integerToBits
#endif

instance Enum F2Poly where
  fromEnum = fromIntegral
#if UseIntegerGmp
  toEnum !(I# i#) = F2Poly $ BitVec 0 (wordSize - I# (word2Int# (clz# (int2Word# i#))))
                           $ fromBigNat $ wordToBigNat (int2Word# i#)
#else
  toEnum = fromIntegral
#endif

instance Real F2Poly where
  toRational = fromIntegral

-- | 'toInteger' converts a binary polynomial, encoded as 'F2Poly',
-- to 'Integer' encoding.
instance Integral F2Poly where
  toInteger = bitsToInteger . unF2Poly
  quotRem (F2Poly xs) (F2Poly ys) = (F2Poly (dropWhileEnd qs), F2Poly (dropWhileEnd rs))
    where
      (qs, rs) = quotRemBits xs ys
  rem = coerce ((dropWhileEnd .) . remBits)
  divMod = quotRem
  mod = rem

xorBits
  :: U.Vector Bit
  -> U.Vector Bit
  -> U.Vector Bit
#if UseIntegerGmp
-- GMP has platform-dependent ASM implementations for mpn_xor_n,
-- which are impossible to beat by native Haskell.
xorBits (BitVec _ 0 _) ys = ys
xorBits xs (BitVec _ 0 _) = xs
xorBits (BitVec 0 lx xarr) (BitVec 0 ly yarr) = case lx `compare` ly of
  LT -> BitVec 0 ly zs
  EQ -> dropWhileEnd $ BitVec 0 (lx `min` (sizeofByteArray zs `shiftL` 3)) zs
  GT -> BitVec 0 lx zs
  where
    zs = fromBigNat (toBigNat xarr `xorBigNat` toBigNat yarr)
#endif
xorBits xs ys = dropWhileEnd $ runST $ do
  let lx = U.length xs
      ly = U.length ys
      (shorterLen, longerLen, longer) = if lx >= ly then (ly, lx, xs) else (lx, ly, ys)
  zs <- MU.replicate longerLen (Bit False)
  forM_ [0, wordSize .. shorterLen - 1] $ \i ->
    writeWord zs i (indexWord xs i `xor` indexWord ys i)
  U.unsafeCopy (MU.drop shorterLen zs) (U.drop shorterLen longer)
  U.unsafeFreeze zs

-- | Must be >= wordSize.
karatsubaThreshold :: Int
karatsubaThreshold = 4096

karatsuba :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
karatsuba xs ys
  | xs == ys = sqrBits xs
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

    xs01 = xorBits xs0 xs1
    ys01 = xorBits ys0 ys1
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
  | lenXs <= wordSize && lenYs <= wordSize = mulShortShort x0 y0
  | lenYs <= wordSize                      = mulLongShort  xs y0
  | lenXs <= wordSize                      = mulLongShort  ys x0
  | otherwise = runST $ do
    zs <- MU.replicate lenZs (Bit False)
    forM_ [0 .. lenYs - 1] $ \k ->
      MU.unsafeWrite zs k
        (zipAndCountParityBits xs (U.unsafeSlice (lenYs - 1 - k) (k + 1) rys))
    forM_ [lenYs .. lenZs - 1] $ \k ->
      MU.unsafeWrite zs k
        (zipAndCountParityBits (U.unsafeSlice (k - (lenYs - 1)) (lenXs + lenYs + 1 - k) xs) rys)
    U.unsafeFreeze zs
  where
    lenXs = U.length xs
    lenYs = U.length ys
    lenZs = lenXs + lenYs - 1
    rys   = reverseBits ys
    x0 = indexWord xs 0 .&. loMask lenXs
    y0 = indexWord ys 0 .&. loMask lenYs

mulShortShort :: Word -> Word -> U.Vector Bit
mulShortShort xs ys = runST $ do
  zs <- MU.replicate lenZs (Bit False)
  forM_ [0 .. lenYs - 1] $ \k -> do
    let yk = rys `shiftR` (lenYs - 1 - k)
        l  = (k + 1) `min` lenXs
    MU.unsafeWrite zs k (fromIntegral $ popCount $ xs .&. yk .&. loMask l)
  forM_ [lenYs .. lenZs - 1] $ \k -> do
    let xk = xs `shiftR` (k - (lenYs - 1))
        l  = (lenXs + lenYs + 1 - k) `min` lenYs
    MU.unsafeWrite zs k (fromIntegral $ popCount $ xk .&. rys .&. loMask l)
  U.unsafeFreeze zs
  where
    clzXs = countLeadingZeros xs
    lenXs = wordSize - clzXs
    clzYs = countLeadingZeros ys
    lenYs = wordSize - clzYs
    lenZs = lenXs + lenYs - 1
    rys   = reverseWord (ys `shiftL` clzYs)

mulLongShort :: U.Vector Bit -> Word -> U.Vector Bit
mulLongShort xs ys = runST $ do
  zs <- MU.replicate lenZs (Bit False)
  forM_ [0 .. lenYs - 1] $ \k -> do
    let yk = rys `shiftR` (lenYs - 1 - k)
        l  = (k + 1) `min` lenXs
    MU.unsafeWrite zs k (fromIntegral $ popCount $ x0 .&. yk .&. loMask l)
  forM_ [lenYs .. lenZs - 1] $ \k -> do
    let xk = indexWord xs (k - (lenYs - 1))
        l  = (lenXs + lenYs + 1 - k) `min` lenYs
    MU.unsafeWrite zs k (fromIntegral $ popCount $ xk .&. rys .&. loMask l)
  U.unsafeFreeze zs
  where
    lenXs = U.length xs
    clzYs = countLeadingZeros ys
    lenYs = wordSize - clzYs
    lenZs = lenXs + lenYs - 1
    rys   = reverseWord (ys `shiftL` clzYs)
    x0    = indexWord xs 0

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

sqrBits :: U.Vector Bit -> U.Vector Bit
sqrBits xs = runST $ do
    let lenXs = U.length xs
    zs <- MU.replicate (lenXs `shiftL` 1) (Bit False)
    forM_ [0, wordSize .. lenXs - 1] $ \i -> do
      let (z0, z1) = sparseBits (indexWord xs i)
      writeWord zs (i `shiftL` 1) z0
      writeWord zs (i `shiftL` 1 + wordSize) z1
    U.unsafeFreeze zs

quotRemBits :: U.Vector Bit -> U.Vector Bit -> (U.Vector Bit, U.Vector Bit)
quotRemBits xs ys
  | U.null ys = throw DivideByZero
  | U.length xs < U.length ys = (U.empty, xs)
  | otherwise = runST $ do
    let lenXs = U.length xs
        lenYs = U.length ys
        lenQs = lenXs - lenYs + 1
    qs <- MU.replicate lenQs (Bit False)
    rs <- MU.replicate lenXs (Bit False)
    U.unsafeCopy rs xs
    forM_ [lenQs - 1, lenQs - 2 .. 0] $ \i -> do
      Bit r <- MU.unsafeRead rs (lenYs - 1 + i)
      when r $ do
        MU.unsafeWrite qs i (Bit True)
        zipInPlace xor ys (MU.drop i rs)
    let rs' = MU.unsafeSlice 0 lenYs rs
    (,) <$> U.unsafeFreeze qs <*> U.unsafeFreeze rs'

remBits :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
remBits xs ys
  | U.null ys = throw DivideByZero
  | U.length xs < U.length ys = xs
  | otherwise = runST $ do
    let lenXs = U.length xs
        lenYs = U.length ys
        lenQs = lenXs - lenYs + 1
    rs <- MU.replicate lenXs (Bit False)
    U.unsafeCopy rs xs
    forM_ [lenQs - 1, lenQs - 2 .. 0] $ \i -> do
      Bit r <- MU.unsafeRead rs (lenYs - 1 + i)
      when r $ do
        zipInPlace xor ys (MU.drop i rs)
    let rs' = MU.unsafeSlice 0 lenYs rs
    U.unsafeFreeze rs'

dropWhileEnd
  :: U.Vector Bit
  -> U.Vector Bit
dropWhileEnd xs = U.unsafeSlice 0 (go (U.length xs)) xs
  where
    go n
      | n < wordSize = wordSize - countLeadingZeros (indexWord xs 0 .&. loMask n)
      | otherwise    = case indexWord xs (n - wordSize) of
        0 -> go (n - wordSize)
        w -> n - countLeadingZeros w

#if UseIntegerGmp

bitsToByteArray :: U.Vector Bit -> ByteArray#
bitsToByteArray xs = arr
  where
    ys = if U.null xs then U.singleton 0 else cloneToWords xs
    !(P.Vector _ _ (ByteArray arr)) = toPrimVector ys

fromBigNat :: BigNat -> ByteArray
fromBigNat = unsafeCoerce
-- fromBigNat (BN# arr) = ByteArray arr

toBigNat :: ByteArray -> BigNat
toBigNat = unsafeCoerce
-- toBigNat (ByteArray arr) = BN# arr

bitsToInteger :: U.Vector Bit -> Integer
bitsToInteger xs = bigNatToInteger (BN# (bitsToByteArray xs))

#else

integerToBits :: Integer -> U.Vector Bit
integerToBits x = U.generate (bitLen x) (Bit . testBit x)

bitLen :: Integer -> Int
bitLen x
  = fst
  $ head
  $ dropWhile (\(_, b) -> x >= b)
  $ map (\a -> (a, 1 `shiftL` a))
  $ map (1 `shiftL`)
  $ [0..]

bitsToInteger :: U.Vector Bit -> Integer
bitsToInteger = U.ifoldl' (\acc i (Bit b) -> if b then acc `setBit` i else acc) 0

#endif
