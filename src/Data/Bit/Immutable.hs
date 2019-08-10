{-# LANGUAGE CPP              #-}

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

#ifndef BITVEC_THREADSAFE
module Data.Bit.Immutable
#else
module Data.Bit.ImmutableTS
#endif
  ( castFromWords
  , castToWords
  , cloneToWords

  , zipBits

  , selectBits
  , excludeBits
  , bitIndex

  , nthBitIndex
  , countBits
  , listBits
  ) where

import Control.Monad.ST
import Data.Bits
#ifndef BITVEC_THREADSAFE
import Data.Bit.Internal
import Data.Bit.Mutable
#else
import Data.Bit.InternalTS
import Data.Bit.MutableTS
#endif
import Data.Bit.Select1
import Data.Bit.Utils
import Data.Primitive.ByteArray
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Unsafe.Coerce

-- | Cast a vector of words to a vector of bits.
-- Cf. 'Data.Bit.castFromWordsM'.
--
-- >>> castFromWords (Data.Vector.Unboxed.singleton 123)
-- [1,1,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
castFromWords :: U.Vector Word -> U.Vector Bit
castFromWords ws = BitVec (mulWordSize off) (mulWordSize len) arr
  where P.Vector off len arr = unsafeCoerce ws

-- | Try to cast a vector of bits to a vector of words.
-- It succeeds if a vector of bits is aligned.
-- Use 'cloneToWords' otherwise.
-- Cf. 'Data.Bit.castToWordsM'.
--
-- prop> castToWords (castFromWords v) == Just v
castToWords :: U.Vector Bit -> Maybe (U.Vector Word)
castToWords (BitVec s n ws)
  | aligned s, aligned n = Just $ unsafeCoerce $ P.Vector (divWordSize s)
                                                          (divWordSize n)
                                                          ws
  | otherwise = Nothing

-- | Clone a vector of bits to a new unboxed vector of words.
-- If the bits don't completely fill the words, the last word will be zero-padded.
-- Cf. 'Data.Bit.cloneToWordsM'.
--
-- >>> cloneToWords (read "[1,1,0,1,1,1,1,0]")
-- [123]
cloneToWords :: U.Vector Bit -> U.Vector Word
cloneToWords v = runST $ do
  v' <- U.unsafeThaw v
  w  <- cloneToWordsM v'
  U.unsafeFreeze w
{-# INLINE cloneToWords #-}

-- | Zip two vectors with the given function.
-- Similar to 'Data.Vector.Unboxed.zipWith', but up to 16x faster.
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
zipBits f xs ys | U.length xs >= U.length ys = zs
                | otherwise                  = U.slice 0 (U.length xs) zs
  where zs = U.modify (zipInPlace f xs) ys
{-# INLINE zipBits #-}

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
nthInWord (Bit b) k v = if k > c then Left (k - c) else Right (select1 w k - 1)
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
      else Right (mulWordSize (n - off) + select1 w l - 1)
   where
    w = indexByteArray arr n
    c = popCount w
nthInWords (Bit False) !k !off !len !arr = go off k
 where
  go !n !l
    | n >= off + len = Left l
    | otherwise = if l > c
      then go (n + 1) (l - c)
      else Right (mulWordSize (n - off) + select1 w l - 1)
   where
    w = complement (indexByteArray arr n)
    c = popCount w

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
