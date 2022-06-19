{-# LANGUAGE CPP                        #-}

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , gcdExt
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
import Data.Char
import Data.Coerce
import Data.Primitive.ByteArray
import Data.Typeable
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import GHC.Exts
import GHC.Generics
import Numeric

#ifdef MIN_VERSION_ghc_bignum
import GHC.Num.BigNat
import GHC.Num.Integer
#else
import GHC.Integer.GMP.Internals
import GHC.Integer.Logarithms
#endif

-- | Binary polynomials of one variable, backed
-- by an unboxed 'Data.Vector.Unboxed.Vector' 'Bit'.
--
-- Polynomials are stored normalized, without leading zero coefficients.
--
-- The 'Ord' instance does not make much sense mathematically,
-- it is defined only for the sake of 'Data.Set.Set', 'Data.Map.Map', etc.
--
-- >>> :set -XBinaryLiterals
-- >>> -- (1 + x) * (1 + x + x^2) = 1 + x^3 (mod 2)
-- >>> 0b11 * 0b111 :: F2Poly
-- 0b1001
--
-- @since 1.0.1.0
newtype F2Poly = F2Poly {
  unF2Poly :: U.Vector Bit
  -- ^ Convert an 'F2Poly' to a vector of coefficients
  -- (first element corresponds to a constant term).
  --
  -- >>> :set -XBinaryLiterals
  -- >>> unF2Poly 0b1101
  -- [1,0,1,1]
  --
  -- @since 1.0.1.0
  }
  deriving (Eq, Ord, Typeable, Generic, NFData)

-- | Make an 'F2Poly' from a list of coefficients
-- (first element corresponds to a constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toF2Poly [1,0,1,1,0,0]
-- 0b1101
--
-- @since 1.0.1.0
toF2Poly :: U.Vector Bit -> F2Poly
toF2Poly xs = F2Poly $ dropWhileEnd $ castFromWords $ cloneToWords xs

zero :: F2Poly
zero = F2Poly $ BitVec 0 0 $
#ifdef MIN_VERSION_ghc_bignum
  ByteArray (unBigNat bigNatZero)
#else
  fromBigNat zeroBigNat
#endif

one :: F2Poly
one = F2Poly $ BitVec 0 1 $
#ifdef MIN_VERSION_ghc_bignum
  ByteArray (unBigNat bigNatOne)
#else
  fromBigNat oneBigNat
#endif

-- -- | A valid 'F2Poly' has offset 0 and no trailing garbage.
-- _isValid :: F2Poly -> Bool
-- _isValid (F2Poly (BitVec o l arr)) = o == 0 && l == l'
--   where
--     l' = U.length $ dropWhileEnd $ BitVec 0 (sizeofByteArray arr `shiftL` 3) arr

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
  signum = const one
  (*) = coerce ((dropWhileEnd .) . karatsuba)
#ifdef MIN_VERSION_ghc_bignum
  fromInteger !n = case n of
    IS i#
      | n < 0     -> throw Underflow
      | otherwise -> F2Poly $ BitVec 0 (wordSize - I# (word2Int# (clz# (int2Word# i#))))
                     $ ByteArray (bigNatFromWord# (int2Word# i#))
    IP bn# -> F2Poly $ BitVec 0 (I# (word2Int# (integerLog2# n)) + 1) $ ByteArray bn#
    IN{}   -> throw Underflow
  {-# INLINE fromInteger #-}
#else
  fromInteger !n = case n of
    S# i#
      | n < 0     -> throw Underflow
      | otherwise -> F2Poly $ BitVec 0 (wordSize - I# (word2Int# (clz# (int2Word# i#))))
                      $ fromBigNat $ wordToBigNat (int2Word# i#)
    Jp# bn# -> F2Poly $ BitVec 0 (I# (integerLog2# n) + 1) $ fromBigNat bn#
    Jn#{}   -> throw Underflow
  {-# INLINE fromInteger #-}
#endif

  {-# INLINE (+)         #-}
  {-# INLINE (-)         #-}
  {-# INLINE negate      #-}
  {-# INLINE abs         #-}
  {-# INLINE signum      #-}
  {-# INLINE (*)         #-}

instance Enum F2Poly where
  fromEnum = fromIntegral
#ifdef MIN_VERSION_ghc_bignum
  toEnum (I# i#) = F2Poly $ BitVec 0 (wordSize - I# (word2Int# (clz# (int2Word# i#))))
                           $ ByteArray (bigNatFromWord# (int2Word# i#))
#else
  toEnum (I# i#) = F2Poly $ BitVec 0 (wordSize - I# (word2Int# (clz# (int2Word# i#))))
                           $ fromBigNat $ wordToBigNat (int2Word# i#)
#endif

instance Real F2Poly where
  toRational = fromIntegral

-- | 'toInteger' converts a binary polynomial, encoded as 'F2Poly',
-- to an 'Integer' encoding.
instance Integral F2Poly where
#ifdef MIN_VERSION_ghc_bignum
  toInteger xs = integerFromBigNat# (bitsToByteArray (unF2Poly xs))
#else
  toInteger xs = bigNatToInteger (BN# (bitsToByteArray (unF2Poly xs)))
#endif
  quotRem (F2Poly xs) (F2Poly ys) = (F2Poly (dropWhileEnd qs), F2Poly (dropWhileEnd rs))
    where
      (qs, rs) = quotRemBits xs ys
  divMod = quotRem
  mod = rem

instance Show F2Poly where
  show = (:) '0' . (:) 'b' . flip (showIntAtBase 2 intToDigit) "" . toInteger

-- | Inputs must be valid for wrapping into F2Poly: no trailing garbage is allowed.
xorBits
  :: U.Vector Bit
  -> U.Vector Bit
  -> U.Vector Bit
xorBits (BitVec _ 0 _) ys = ys
xorBits xs (BitVec _ 0 _) = xs
-- GMP has platform-dependent ASM implementations for mpn_xor_n,
-- which are impossible to beat by native Haskell.
#ifdef MIN_VERSION_ghc_bignum
xorBits (BitVec 0 lx (ByteArray xarr)) (BitVec 0 ly (ByteArray yarr)) = case lx `compare` ly of
  LT -> BitVec 0 ly zs
  EQ -> dropWhileEnd $ BitVec 0 (lx `min` (sizeofByteArray zs `shiftL` 3)) zs
  GT -> BitVec 0 lx zs
  where
    zs = ByteArray (xarr `bigNatXor` yarr)
#else
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

-- | Must be >= 2 * wordSize.
karatsubaThreshold :: Int
karatsubaThreshold = 2048

karatsuba :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
karatsuba xs ys
  | xs == ys = sqrBits xs
  | lenXs <= karatsubaThreshold || lenYs <= karatsubaThreshold
  = mulBits xs ys
  | otherwise = runST $ do
    zs <- MU.unsafeNew lenZs
    forM_ [0 .. divWordSize (lenZs - 1)] $ \k -> do
      let z0  = indexWord0 zs0   k
          z11 = indexWord0 zs11 (k - m)
          z10 = indexWord0 zs0  (k - m)
          z12 = indexWord0 zs2  (k - m)
          z2  = indexWord0 zs2  (k - 2 * m)
      writeWord zs (mulWordSize k) (z0 `xor` z11 `xor` z10 `xor` z12 `xor` z2)
    U.unsafeFreeze zs
  where
    lenXs = U.length xs
    lenYs = U.length ys
    lenZs = lenXs + lenYs - 1

    m    = (min lenXs lenYs + 1) `unsafeShiftR` (lgWordSize + 1)
    m'   = mulWordSize m

    xs0  = U.unsafeSlice 0 m' xs
    xs1  = U.unsafeSlice m' (lenXs - m') xs
    ys0  = U.unsafeSlice 0 m' ys
    ys1  = U.unsafeSlice m' (lenYs - m') ys

    xs01 = xorBits xs0 xs1
    ys01 = xorBits ys0 ys1
    zs0  = karatsuba xs0 ys0
    zs2  = karatsuba xs1 ys1
    zs11 = karatsuba xs01 ys01

indexWord0 :: U.Vector Bit -> Int -> Word
indexWord0 bv i'
  | i < 0 || lenI <= 0 = 0
  | lenI >= wordSize   = word
  | otherwise          = word .&. loMask lenI
  where
    i     = mulWordSize i'
    lenI  = U.length bv - i
    word  = indexWord bv i

mulBits :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
mulBits xs ys
  | lenXs == 0 || lenYs == 0 = U.empty
  | lenXs >= lenYs           = mulBits' xs ys
  | otherwise                = mulBits' ys xs
  where
    lenXs = U.length xs
    lenYs = U.length ys

mulBits' :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
mulBits' xs ys = runST $ do
  zs <- MU.replicate lenZs (Bit False)
  forM_ [0 .. lenYs - 1] $ \k ->
    when (unBit (U.unsafeIndex ys k)) $
      zipInPlace xor xs (MU.unsafeSlice k (lenZs - k) zs)
  U.unsafeFreeze zs
  where
    lenXs = U.length xs
    lenYs = U.length ys
    lenZs = lenXs + lenYs - 1

sqrBits :: U.Vector Bit -> U.Vector Bit
sqrBits xs = runST $ do
  let lenXs = U.length xs
  zs <- MU.replicate (mulWordSize (nWords lenXs `shiftL` 1)) (Bit False)
  forM_ [0, wordSize .. lenXs - 1] $ \i -> do
    let (z0, z1) = sparseBits (indexWord xs i)
    writeWord zs (i `shiftL` 1) z0
    writeWord zs ((i `shiftL` 1) + wordSize) z1
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

bitsToByteArray :: U.Vector Bit -> ByteArray#
bitsToByteArray xs = arr
  where
    ys = if U.null xs then U.singleton (0 :: Word) else cloneToWords xs
    !(P.Vector _ _ (ByteArray arr)) = toPrimVector ys

#ifdef MIN_VERSION_ghc_bignum
#else
fromBigNat :: BigNat -> ByteArray
fromBigNat (BN# arr) = ByteArray arr

toBigNat :: ByteArray -> BigNat
toBigNat (ByteArray arr) = BN# arr
#endif

-- | Execute the extended Euclidean algorithm.
-- For polynomials @a@ and @b@, compute their unique greatest common divisor @g@
-- and the unique coefficient polynomial @s@ satisfying \( a \cdot s + b \cdot t = g \).
--
-- >>> :set -XBinaryLiterals
-- >>> gcdExt 0b101 0b0101
-- (0b101,0b0)
-- >>> gcdExt 0b11 0b111
-- (0b1,0b10)
--
-- @since 1.0.2.0
gcdExt :: F2Poly -> F2Poly -> (F2Poly, F2Poly)
gcdExt = go one zero
  where
    go s s' r r'
      | r' == 0   = (r, s)
      | otherwise = case quotRem r r' of
        (q, r'') -> go s' (s - q * s') r' r''
