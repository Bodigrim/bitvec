{-# LANGUAGE CPP                  #-}

{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifndef BITVEC_THREADSAFE
module Data.Bit.Immutable
#else
module Data.Bit.ImmutableTS
#endif
  ( castFromWords
  , castToWords
  , cloneToWords

  , castFromWords8
  , castToWords8
  , cloneToWords8

  , zipBits
  , invertBits
  , selectBits
  , excludeBits
  , reverseBits

  , bitIndex
  , nthBitIndex
  , countBits
  , listBits
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
#if UseLibGmp
import Data.Bit.Gmp
#endif
#ifndef BITVEC_THREADSAFE
import Data.Bit.Internal
import Data.Bit.Mutable
#else
import Data.Bit.InternalTS
import Data.Bit.MutableTS
#endif
import Data.Bit.Pdep
import Data.Bit.Utils
import Data.Primitive.ByteArray
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word
import Unsafe.Coerce

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS == 64
#define GMP_LIMB_SHIFT 3
#elif WORD_SIZE_IN_BITS == 32
#define GMP_LIMB_SHIFT 2
#else
#error unsupported WORD_SIZE_IN_BITS config
#endif

instance {-# OVERLAPPING #-} Bits (Vector Bit) where
  (.&.) = zipBits (.&.)
  (.|.) = zipBits (.|.)
  xor   = zipBits xor
  complement = invertBits
  bitSize _ = error "bitSize is undefined"
  bitSizeMaybe _ = Nothing
  isSigned _ = False
  zeroBits = U.empty
  popCount = countBits

  testBit v n
    | n < 0 || n >= U.length v = False
    | otherwise = unBit (U.unsafeIndex v n)

  bit n
    | n < 0 = U.empty
    | otherwise = runST $ do
      v <- MU.replicate (n + 1) (Bit False)
      MU.unsafeWrite v n (Bit True)
      U.unsafeFreeze v

  shift v n = case n `compare` 0 of
    -- shift right
    LT
      | U.length v + n < 0 -> U.empty
      | otherwise -> runST $ do
        u <- MU.new (U.length v + n)
        U.copy u (U.drop (- n) v)
        U.unsafeFreeze u
    -- do not shift
    EQ -> v
    -- shift left
    GT -> runST $ do
      u <- MU.new (U.length v + n)
      MU.set (MU.take n u) (Bit False)
      U.copy (MU.drop n u) v
      U.unsafeFreeze u

  rotate v n'
    | U.null v = v
    | otherwise = runST $ do
      let l = U.length v
          n = n' `mod` l
      u <- MU.new l
      U.copy (MU.drop n u) (U.take (l - n) v)
      U.copy (MU.take n u) (U.drop (l - n) v)
      U.unsafeFreeze u

-- | Cast an unboxed vector of words
-- to an unboxed vector of bits.
-- Cf. 'castFromWordsM'.
--
-- >>> castFromWords (Data.Vector.Unboxed.singleton 123)
-- [1,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
castFromWords :: U.Vector Word -> U.Vector Bit
castFromWords ws = BitVec (mulWordSize off) (mulWordSize len) arr
  where
    P.Vector off len arr = toPrimVector ws

-- | Try to cast an unboxed vector of bits
-- to an unboxed vector of words.
-- It succeeds if a vector of bits is aligned.
-- Use 'cloneToWords' otherwise.
-- Cf. 'castToWordsM'.
--
-- prop> castToWords (castFromWords v) == Just v
castToWords :: U.Vector Bit -> Maybe (U.Vector Word)
castToWords (BitVec s n ws)
  | aligned s, aligned n =
    Just $ fromPrimVector $ P.Vector (divWordSize s) (divWordSize n) ws
  | otherwise = Nothing

-- | Clone an unboxed vector of bits
-- to a new unboxed vector of words.
-- If the bits don't completely fill the words,
-- the last word will be zero-padded.
-- Cf. 'cloneToWordsM'.
--
-- >>> cloneToWords (read "[1,1,0,1,1,1,1,0]")
-- [123]
cloneToWords :: U.Vector Bit -> U.Vector Word
cloneToWords v = runST $ do
  v' <- U.unsafeThaw v
  w  <- cloneToWordsM v'
  U.unsafeFreeze w
{-# INLINE cloneToWords #-}

-- | Cast a unboxed vector of 'Word8'
-- to an unboxed vector of bits.
-- This can be used in conjunction
-- with @bytestring-to-vector@ package
-- to convert from 'Data.ByteString.ByteString':
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Vector.Storable.ByteString
-- >>> castFromWords8 (Data.Vector.convert (byteStringToVector "abc"))
-- [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
castFromWords8 :: U.Vector Word8 -> U.Vector Bit
castFromWords8 ws = BitVec (off `shiftL` 3) (len `shiftL` 3) arr
  where
    P.Vector off len arr = unsafeCoerce ws

-- | Try to cast an unboxed vector of bits
-- to an unboxed vector of 'Word8'.
-- It succeeds if a vector of bits is aligned.
-- Use 'cloneToWords8' otherwise.
--
-- prop> castToWords8 (castFromWords8 v) == Just v
castToWords8 :: U.Vector Bit -> Maybe (U.Vector Word8)
castToWords8 (BitVec s n ws)
  | s .&. 7 == 0, n .&. 7 == 0 =
    Just $ unsafeCoerce $ P.Vector (s `shiftR` 3) (n `shiftR` 3) ws
  | otherwise = Nothing

-- | Clone an unboxed vector of bits
-- to a new unboxed vector of 'Word8'.
-- If the bits don't completely fill the words,
-- the last 'Word8' will be zero-padded.
-- This can be used in conjunction
-- with @bytestring-to-vector@ package
-- to convert to 'Data.ByteString.ByteString':
--
-- >>> :set -XOverloadedLists
-- >>> import Data.Vector.Storable.ByteString
-- >>> vectorToByteString (Data.Vector.convert (Data.Bit.cloneToWords8 [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1]))
-- "ab#"
cloneToWords8 :: U.Vector Bit -> U.Vector Word8
cloneToWords8 v = runST $ do
  v' <- U.unsafeThaw v
  w  <- cloneToWords8M v'
  U.unsafeFreeze w
{-# INLINE cloneToWords8 #-}

-- | Zip two vectors with the given function.
-- Similar to 'Data.Vector.Unboxed.zipWith',
-- but up to 1000x (!) faster.
--
-- For sufficiently dense sets, represented as bitmaps,
-- 'zipBits' is up to 32x faster than
-- 'Data.IntSet.union', 'Data.IntSet.intersection', etc.
--
-- >>> import Data.Bits
-- >>> zipBits (.&.) (read "[1,1,0]") (read "[0,1,1]") -- intersection
-- [0,1,0]
-- >>> zipBits (.|.) (read "[1,1,0]") (read "[0,1,1]") -- union
-- [1,1,1]
-- >>> zipBits (\x y -> x .&. complement y) (read "[1,1,0]") (read "[0,1,1]") -- difference
-- [1,0,0]
-- >>> zipBits xor (read "[1,1,0]") (read "[0,1,1]") -- symmetric difference
-- [1,0,1]
zipBits
  :: (forall a . Bits a => a -> a -> a)
  -> U.Vector Bit
  -> U.Vector Bit
  -> U.Vector Bit
zipBits _ (BitVec _ 0 _) _ = U.empty
zipBits _ _ (BitVec _ 0 _) = U.empty
#if UseLibGmp
zipBits f (BitVec 0 l1 arg1) (BitVec 0 l2 arg2) = runST $ do
    let l = l1 `min` l2
        w = nWords l
        b = w `shiftL` GMP_LIMB_SHIFT
    brr <- newByteArray b
    let ff = unBit $ f (Bit False) (Bit False)
        ft = unBit $ f (Bit False) (Bit True)
        tf = unBit $ f (Bit True)  (Bit False)
        tt = unBit $ f (Bit True)  (Bit True)
    case (ff, ft, tf, tt) of
      (False, False, False, False) -> setByteArray brr 0 w (zeroBits :: Word)
      (False, False, False, True)  -> mpnAndN  brr arg1 arg2 w
      (False, False, True,  False) -> mpnAndnN brr arg1 arg2 w
      (False, False, True,  True)  -> copyByteArray brr 0 arg1 0 b
      (False, True,  False, False) -> mpnAndnN brr arg2 arg1 w
      (False, True,  False, True)  -> copyByteArray brr 0 arg2 0 b
      (False, True,  True,  False) -> mpnXorN  brr arg1 arg2 w
      (False, True,  True,  True)  -> mpnIorN  brr arg1 arg2 w
      (True,  False, False, False) -> mpnNiorN brr arg1 arg2 w
      (True,  False, False, True)  -> mpnXnorN brr arg1 arg2 w
      (True,  False, True,  False) -> mpnCom   brr arg2      w
      (True,  False, True,  True)  -> mpnIornN brr arg1 arg2 w
      (True,  True,  False, False) -> mpnCom   brr arg1      w
      (True,  True,  False, True)  -> mpnIornN brr arg2 arg1 w
      (True,  True,  True,  False) -> mpnNandN brr arg1 arg2 w
      (True,  True,  True,  True)  -> setByteArray brr 0 w (complement zeroBits :: Word)
    BitVec 0 l <$> unsafeFreezeByteArray brr
#endif
zipBits f xs ys = runST $ do
  let n = min (U.length xs) (U.length ys)
  zs <- MU.new n
  forM_ [0, wordSize .. n - 1] $ \i ->
    writeWord zs i (f (indexWord xs i) (indexWord ys i))
  U.unsafeFreeze zs
{-# INLINE zipBits #-}

-- | Invert (flip) all bits.
--
-- >>> invertBits (read "[0,1,0,1,0]")
-- [1,0,1,0,1]
invertBits
  :: U.Vector Bit
  -> U.Vector Bit
invertBits (BitVec _ 0 _) = U.empty
#if UseLibGmp
invertBits (BitVec 0 l arg) = runST $ do
  let w = nWords l
  brr <- newByteArray (w `shiftL` GMP_LIMB_SHIFT)
  mpnCom brr arg w
  BitVec 0 l <$> unsafeFreezeByteArray brr
#endif
invertBits xs = runST $ do
  let n = U.length xs
  ys <- MU.new n
  forM_ [0, wordSize .. n - 1] $ \i ->
    writeWord ys i (complement (indexWord xs i))
  U.unsafeFreeze ys

-- | For each set bit of the first argument, deposit
-- the corresponding bit of the second argument
-- to the result. Similar to the parallel deposit instruction (PDEP).
--
-- >>> selectBits (read "[0,1,0,1,1]") (read "[1,1,0,0,1]")
-- [1,0,1]
--
-- Here is a reference (but slow) implementation:
--
-- > import qualified Data.Vector.Unboxed as U
-- > selectBits mask ws == U.map snd (U.filter (unBit . fst) (U.zip mask ws))
selectBits :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
selectBits is xs = runST $ do
  xs1 <- U.thaw xs
  n   <- selectBitsInPlace is xs1
  U.unsafeFreeze (MU.take n xs1)

-- | For each unset bit of the first argument, deposit
-- the corresponding bit of the second argument
-- to the result.
--
-- >>> excludeBits (read "[0,1,0,1,1]") (read "[1,1,0,0,1]")
-- [1,0]
--
-- Here is a reference (but slow) implementation:
--
-- > import qualified Data.Vector.Unboxed as U
-- > excludeBits mask ws == U.map snd (U.filter (not . unBit . fst) (U.zip mask ws))
excludeBits :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
excludeBits is xs = runST $ do
  xs1 <- U.thaw xs
  n   <- excludeBitsInPlace is xs1
  U.unsafeFreeze (MU.take n xs1)

-- | Reverse the order of bits.
--
-- >>> reverseBits (read "[1,1,0,1,0]")
-- [0,1,0,1,1]
--
-- Consider using @vector-rotcev@ package
-- to reverse vectors in O(1) time.
reverseBits :: U.Vector Bit -> U.Vector Bit
reverseBits xs = runST $ do
  let n    = U.length xs
  ys <- MU.new n

  forM_ [0, wordSize .. n - wordSize] $ \i ->
    writeWord ys (n - i - wordSize) (reverseWord (indexWord xs i))

  let nMod = modWordSize n
  when (nMod /= 0) $ do
    let x = indexWord xs (mulWordSize (divWordSize n))
    y <- readWord ys 0
    writeWord ys 0 (meld nMod (reversePartialWord nMod x) y)

  U.unsafeFreeze ys

clipLoBits :: Bit -> Int -> Word -> Word
clipLoBits (Bit True ) k w = w `unsafeShiftR` k
clipLoBits (Bit False) k w = (w `unsafeShiftR` k) .|. hiMask (wordSize - k)

clipHiBits :: Bit -> Int -> Word -> Word
clipHiBits (Bit True ) k w = w .&. loMask k
clipHiBits (Bit False) k w = w .|. hiMask k

-- | Return the index of the first bit in the vector
-- with the specified value, if any.
-- Similar to 'Data.Vector.Unboxed.elemIndex', but up to 64x faster.
--
-- >>> bitIndex (Bit True) (read "[0,0,1,0,1]")
-- Just 2
-- >>> bitIndex (Bit True) (read "[0,0,0,0,0]")
-- Nothing
--
-- prop> bitIndex bit == nthBitIndex bit 1
--
-- One can also use it to reduce a vector with disjunction or conjunction:
--
-- >>> import Data.Maybe
-- >>> isAnyBitSet   = isJust    . bitIndex (Bit True)
-- >>> areAllBitsSet = isNothing . bitIndex (Bit False)
bitIndex :: Bit -> U.Vector Bit -> Maybe Int
bitIndex b (BitVec off len arr)
  | len == 0 = Nothing
  | offBits == 0 = case modWordSize len of
    0    -> bitIndexInWords b offWords lWords arr
    nMod -> case bitIndexInWords b offWords (lWords - 1) arr of
      r@Just{} -> r
      Nothing  -> (+ mulWordSize (lWords - 1)) <$> bitIndexInWord
        b
        (clipHiBits b nMod (indexByteArray arr (offWords + lWords - 1)))
  | otherwise = case modWordSize (off + len) of
    0 ->
      case
          bitIndexInWord b (clipLoBits b offBits (indexByteArray arr offWords))
        of
          r@Just{} -> r
          Nothing ->
            (+ (wordSize - offBits))
              <$> bitIndexInWords b (offWords + 1) (lWords - 1) arr
    nMod -> case lWords of
      1 -> bitIndexInWord
        b
        (clipHiBits b len (clipLoBits b offBits (indexByteArray arr offWords)))
      _ ->
        case
            bitIndexInWord
              b
              (clipLoBits b offBits (indexByteArray arr offWords))
          of
            r@Just{} -> r
            Nothing ->
              (+ (wordSize - offBits))
                <$> case bitIndexInWords b (offWords + 1) (lWords - 2) arr of
                      r@Just{} -> r
                      Nothing ->
                        (+ mulWordSize (lWords - 2)) <$> bitIndexInWord
                          b
                          (clipHiBits
                            b
                            nMod
                            (indexByteArray arr (offWords + lWords - 1))
                          )
 where
  offBits  = modWordSize off
  offWords = divWordSize off
  lWords   = nWords (offBits + len)

bitIndexInWord :: Bit -> Word -> Maybe Int
bitIndexInWord (Bit True ) = ffs
bitIndexInWord (Bit False) = ffs . complement

bitIndexInWords :: Bit -> Int -> Int -> ByteArray -> Maybe Int
bitIndexInWords (Bit True) !off !len !arr = go off
 where
  go !n
    | n >= off + len = Nothing
    | otherwise = case ffs (indexByteArray arr n) of
      Nothing  -> go (n + 1)
      r@Just{} -> r
bitIndexInWords (Bit False) !off !len !arr = go off
 where
  go !n
    | n >= off + len = Nothing
    | otherwise = case ffs (complement (indexByteArray arr n)) of
      Nothing  -> go (n + 1)
      r@Just{} -> r

-- | Return the index of the @n@-th bit in the vector
-- with the specified value, if any.
-- Here @n@ is 1-based and the index is 0-based.
-- Non-positive @n@ results in an error.
--
-- >>> nthBitIndex (Bit True) 2 (read "[0,1,0,1,1,1,0]")
-- Just 3
-- >>> nthBitIndex (Bit True) 5 (read "[0,1,0,1,1,1,0]")
-- Nothing
--
-- One can use 'nthBitIndex' to implement
-- to implement @select{0,1}@ queries
-- for <https://en.wikipedia.org/wiki/Succinct_data_structure succinct dictionaries>.
nthBitIndex :: Bit -> Int -> U.Vector Bit -> Maybe Int
nthBitIndex _ k _ | k <= 0 = error "nthBitIndex: n must be positive"
nthBitIndex b k (BitVec off len arr)
  | len == 0 = Nothing
  | offBits == 0 = either (const Nothing) Just $ case modWordSize len of
    0    -> nthInWords b k offWords lWords arr
    nMod -> case nthInWords b k offWords (lWords - 1) arr of
      r@Right{} -> r
      Left k'   -> (+ mulWordSize (lWords - 1)) <$> nthInWord
        b
        k'
        (clipHiBits b nMod (indexByteArray arr (offWords + lWords - 1)))
  | otherwise = either (const Nothing) Just $ case modWordSize (off + len) of
    0 ->
      case nthInWord b k (clipLoBits b offBits (indexByteArray arr offWords)) of
        r@Right{} -> r
        Left k' ->
          (+ (wordSize - offBits))
            <$> nthInWords b k' (offWords + 1) (lWords - 1) arr
    nMod -> case lWords of
      1 -> nthInWord
        b
        k
        (clipHiBits b len (clipLoBits b offBits (indexByteArray arr offWords)))
      _ ->
        case
            nthInWord b k (clipLoBits b offBits (indexByteArray arr offWords))
          of
            r@Right{} -> r
            Left k' ->
              (+ (wordSize - offBits))
                <$> case nthInWords b k' (offWords + 1) (lWords - 2) arr of
                      r@Right{} -> r
                      Left k''  -> (+ mulWordSize (lWords - 2)) <$> nthInWord
                        b
                        k''
                        (clipHiBits
                          b
                          nMod
                          (indexByteArray arr (offWords + lWords - 1))
                        )
 where
  offBits  = modWordSize off
  offWords = divWordSize off
  lWords   = nWords (offBits + len)

nthInWord :: Bit -> Int -> Word -> Either Int Int
nthInWord (Bit b) k v = if k > c then Left (k - c) else Right (unsafeNthTrueInWord k w)
 where
  w = if b then v else complement v
  c = popCount w

nthInWords :: Bit -> Int -> Int -> Int -> ByteArray -> Either Int Int
nthInWords (Bit True) !k !off !len !arr = go off k
 where
  go !n !l
    | n >= off + len = Left l
    | otherwise = if l > c
      then go (n + 1) (l - c)
      else Right (mulWordSize (n - off) + unsafeNthTrueInWord l w)
   where
    w = indexByteArray arr n
    c = popCount w
nthInWords (Bit False) !k !off !len !arr = go off k
 where
  go !n !l
    | n >= off + len = Left l
    | otherwise = if l > c
      then go (n + 1) (l - c)
      else Right (mulWordSize (n - off) + unsafeNthTrueInWord l w)
   where
    w = complement (indexByteArray arr n)
    c = popCount w

unsafeNthTrueInWord :: Int -> Word -> Int
unsafeNthTrueInWord l w = countTrailingZeros (pdep (1 `shiftL` (l - 1)) w)

-- | Return the number of set bits in a vector (population count, popcount).
--
-- >>> countBits (read "[1,1,0,1,0,1]")
-- 4
--
-- One can combine 'countBits' with 'Data.Vector.Unboxed.take'
-- to implement @rank{0,1}@ queries
-- for <https://en.wikipedia.org/wiki/Succinct_data_structure succinct dictionaries>.
countBits :: U.Vector Bit -> Int
countBits (BitVec _ 0 _)                      = 0
#if UseLibGmp
countBits (BitVec 0 len arr) | modWordSize len == 0 =
  fromIntegral (mpnPopcount arr (divWordSize len))
#endif
countBits (BitVec off len arr) | offBits == 0 = case modWordSize len of
  0    -> countBitsInWords (P.Vector offWords lWords arr)
  nMod -> countBitsInWords (P.Vector offWords (lWords - 1) arr)
    + popCount (indexByteArray arr (offWords + lWords - 1) .&. loMask nMod)
 where
  offBits  = modWordSize off
  offWords = divWordSize off
  lWords   = nWords (offBits + len)
countBits (BitVec off len arr) = case modWordSize (off + len) of
  0 -> popCount (indexByteArray arr offWords `unsafeShiftR` offBits :: Word)
    + countBitsInWords (P.Vector (offWords + 1) (lWords - 1) arr)
  nMod -> case lWords of
    1 -> popCount
      ((indexByteArray arr offWords `unsafeShiftR` offBits) .&. loMask len)
    _ ->
      popCount (indexByteArray arr offWords `unsafeShiftR` offBits :: Word)
        + countBitsInWords (P.Vector (offWords + 1) (lWords - 2) arr)
        + popCount (indexByteArray arr (offWords + lWords - 1) .&. loMask nMod)
 where
  offBits  = modWordSize off
  offWords = divWordSize off
  lWords   = nWords (offBits + len)

countBitsInWords :: P.Vector Word -> Int
countBitsInWords = P.foldl' (\acc word -> popCount word + acc) 0

-- | Return the indices of set bits in a vector.
--
-- >>> listBits (read "[1,1,0,1,0,1]")
-- [0,1,3,5]
listBits :: U.Vector Bit -> [Int]
listBits (BitVec _ 0 _)                      = []
listBits (BitVec off len arr) | offBits == 0 = case modWordSize len of
  0 -> listBitsInWords 0 (P.Vector offWords lWords arr) []
  nMod ->
    listBitsInWords 0 (P.Vector offWords (lWords - 1) arr)
      $ map (+ mulWordSize (lWords - 1))
      $ filter (testBit (indexByteArray arr (offWords + lWords - 1) :: Word))
               [0 .. nMod - 1]
 where
  offBits  = modWordSize off
  offWords = divWordSize off
  lWords   = nWords (offBits + len)
listBits (BitVec off len arr) = case modWordSize (off + len) of
  0 ->
    filter
        (testBit (indexByteArray arr offWords `unsafeShiftR` offBits :: Word))
        [0 .. wordSize - offBits - 1]
      ++ listBitsInWords (wordSize - offBits)
                         (P.Vector (offWords + 1) (lWords - 1) arr)
                         []
  nMod -> case lWords of
    1 -> filter
      (testBit (indexByteArray arr offWords `unsafeShiftR` offBits :: Word))
      [0 .. len - 1]
    _ ->
      filter
          (testBit (indexByteArray arr offWords `unsafeShiftR` offBits :: Word))
          [0 .. wordSize - offBits - 1]
        ++ ( listBitsInWords (wordSize - offBits)
                             (P.Vector (offWords + 1) (lWords - 2) arr)
           $ map (+ (mulWordSize (lWords - 1) - offBits))
           $ filter
               (testBit (indexByteArray arr (offWords + lWords - 1) :: Word))
               [0 .. nMod - 1]
           )
 where
  offBits  = modWordSize off
  offWords = divWordSize off
  lWords   = nWords (offBits + len)

listBitsInWord :: Int -> Word -> [Int]
listBitsInWord offset word =
  map (+ offset) $ filter (testBit word) $ [0 .. wordSize - 1]

listBitsInWords :: Int -> P.Vector Word -> [Int] -> [Int]
listBitsInWords offset = flip $ P.ifoldr
  (\i word acc -> listBitsInWord (offset + mulWordSize i) word ++ acc)
