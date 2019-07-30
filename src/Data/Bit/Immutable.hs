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

import Control.Monad
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
castFromWords
    :: U.Vector Word
    -> U.Vector Bit
castFromWords ws = BitVec (mulWordSize off) (mulWordSize len) arr
    where
        P.Vector off len arr = unsafeCoerce ws

-- | Try to cast a vector of bits to a vector of words.
-- It succeeds if a vector of bits is aligned.
-- Use 'cloneToWords' otherwise.
-- Cf. 'Data.Bit.castToWordsM'.
--
-- prop> castToWords (castFromWords v) == Just v
castToWords
    :: U.Vector Bit
    -> Maybe (U.Vector Word)
castToWords (BitVec s n ws)
    | aligned s
    , aligned n
    = Just $ unsafeCoerce $ P.Vector (divWordSize s) (divWordSize n) ws
    | otherwise
    = Nothing

-- | Clone a vector of bits to a new unboxed vector of words.
-- If the bits don't completely fill the words, the last word will be zero-padded.
-- Cf. 'Data.Bit.cloneToWordsM'.
--
-- >>> cloneToWords (read "[1,1,0,1,1,1,1,0]")
-- [123]
cloneToWords
    :: U.Vector Bit
    -> U.Vector Word
cloneToWords v = runST $ do
    v' <- U.unsafeThaw v
    w <- cloneToWordsM v'
    U.unsafeFreeze w
{-# INLINE cloneToWords #-}

-- | Zip two vectors with the given function.
-- Similar to 'Data.Vector.Unboxed.zipWith', but much faster.
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
    :: (forall a. Bits a => a -> a -> a)
    -> U.Vector Bit
    -> U.Vector Bit
    -> U.Vector Bit
zipBits f xs ys
    | U.length xs >= U.length ys = zs
    | otherwise = U.slice 0 (U.length xs) zs
    where
        zs = U.modify (zipInPlace f xs) ys

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
    n <- selectBitsInPlace is xs1
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
    n <- excludeBitsInPlace is xs1
    U.unsafeFreeze (MU.take n xs1)

-- | Return the index of the first bit in the vector
-- with the specified value, if any.
-- Similar to 'Data.Vector.Unboxed.elemIndex', but much faster.
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
bitIndex b xs = mfilter (< n) (loop 0)
    where
        !n = U.length xs
        !ff | unBit b   = ffs
            | otherwise = ffs . complement

        loop !i
            | i >= n    = Nothing
            | otherwise = fmap (i +) (ff (indexWord xs i)) `mplus` loop (i + wordSize)

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
nthBitIndex _ k _
    | k <= 0 = error "nthBitIndex: n must be positive"
nthBitIndex (Bit True) k (BitVec off len arr)
    | len == 0 = Nothing
    | offBits == 0 = case modWordSize len of
        0 -> case nth1InWords k (P.Vector offWords lWords arr) of
            Right x -> Just x
            Left{}  -> Nothing
        nMod -> case nth1InWords k (P.Vector offWords (lWords - 1) arr) of
            Right x -> Just x
            Left k' -> case nth1 k' (indexByteArray arr (offWords + lWords - 1) .&. loMask nMod) of
                Right x -> Just $ mulWordSize (lWords - 1) + x
                Left{}  -> Nothing
    | otherwise = case modWordSize (off + len) of
        0 -> case nth1 k (indexByteArray arr offWords `unsafeShiftR` offBits) of
            Right x -> Just x
            Left k' -> case nth1InWords k' (P.Vector (offWords + 1) (lWords - 1) arr) of
                Right x -> Just $ wordSize - offBits + x
                Left {} -> Nothing
        nMod -> case lWords of
            1 -> case nth1 k ((indexByteArray arr offWords `unsafeShiftR` offBits) .&. loMask len) of
                Right x -> Just x
                Left{}  -> Nothing
            _ -> case nth1 k (indexByteArray arr offWords `unsafeShiftR` offBits) of
                Right x -> Just x
                Left k' -> case nth1InWords k' (P.Vector (offWords + 1) (lWords - 2) arr) of
                    Right x  -> Just $ wordSize - offBits + x
                    Left k'' -> case nth1 k'' (indexByteArray arr (offWords + lWords - 1) .&. loMask nMod) of
                        Right x -> Just $ mulWordSize (lWords - 1) - offBits + x
                        Left{}  -> Nothing
    where
        offBits  = modWordSize off
        offWords = divWordSize off
        lWords   = nWords (offBits + len)
nthBitIndex (Bit False) k (BitVec off len arr)
    | len == 0 = Nothing
    | offBits == 0 = case modWordSize len of
        0 -> case nth0InWords k (P.Vector offWords lWords arr) of
            Right x -> Just x
            Left{}  -> Nothing
        nMod -> case nth0InWords k (P.Vector offWords (lWords - 1) arr) of
            Right x -> Just x
            Left k' -> case nth0 k' (indexByteArray arr (offWords + lWords - 1) .|. hiMask nMod) of
                Right x -> Just $ mulWordSize (lWords - 1) + x
                Left{}  -> Nothing
    | otherwise = case modWordSize (off + len) of
        0 -> case nth0 k (indexByteArray arr offWords `unsafeShiftR` offBits .|. hiMask (wordSize - offBits)) of
            Right x -> Just x
            Left k' -> case nth0InWords k' (P.Vector (offWords + 1) (lWords - 1) arr) of
                Right x -> Just $ wordSize - offBits + x
                Left {} -> Nothing
        nMod -> case lWords of
            1 -> case nth0 k ((indexByteArray arr offWords `unsafeShiftR` offBits) .|. hiMask len) of
                Right x -> Just x
                Left{}  -> Nothing
            _ -> case nth0 k ((indexByteArray arr offWords `unsafeShiftR` offBits) .|. hiMask (wordSize - offBits)) of
                Right x -> Just x
                Left k' -> case nth0InWords k' (P.Vector (offWords + 1) (lWords - 2) arr) of
                    Right x  -> Just $ wordSize - offBits + x
                    Left k'' -> case nth0 k'' (indexByteArray arr (offWords + lWords - 1) .|. hiMask nMod) of
                        Right x -> Just $ mulWordSize (lWords - 1) - offBits + x
                        Left{}  -> Nothing
    where
        offBits  = modWordSize off
        offWords = divWordSize off
        lWords   = nWords (offBits + len)

nth0 :: Int -> Word -> Either Int Int
nth0 k v = if k > c then Left (k - c) else Right (select1 w k - 1)
    where
        w = complement v
        c = popCount w

nth1 :: Int -> Word -> Either Int Int
nth1 k w = if k > c then Left (k - c) else Right (select1 w k - 1)
    where
        c = popCount w

nth0InWords :: Int -> P.Vector Word -> Either Int Int
nth0InWords k vec = go 0 k
    where
        go n l
            | n >= P.length vec = Left l
            | otherwise = if l > c then go (n + 1) (l - c) else Right (mulWordSize n + select1 w l - 1)
            where
                w = complement (vec P.! n)
                c = popCount w

nth1InWords :: Int -> P.Vector Word -> Either Int Int
nth1InWords k vec = go 0 k
    where
        go n l
            | n >= P.length vec = Left l
            | otherwise = if l > c then go (n + 1) (l - c) else Right (mulWordSize n + select1 w l - 1)
            where
                w = vec P.! n
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
countBits (BitVec _ 0 _) = 0
countBits (BitVec off len arr) | offBits == 0 = case modWordSize len of
    0    -> countBitsInWords (P.Vector offWords lWords arr)
    nMod -> countBitsInWords (P.Vector offWords (lWords - 1) arr) +
            popCount (indexByteArray arr (offWords + lWords - 1) .&. loMask nMod)
    where
        offBits  = modWordSize off
        offWords = divWordSize off
        lWords   = nWords (offBits + len)
countBits (BitVec off len arr) = case modWordSize (off + len) of
    0    -> popCount (indexByteArray arr offWords `unsafeShiftR` offBits :: Word) +
            countBitsInWords (P.Vector (offWords + 1) (lWords - 1) arr)
    nMod -> case lWords of
        1 -> popCount ((indexByteArray arr offWords `unsafeShiftR` offBits) .&. loMask len)
        _ ->
            popCount (indexByteArray arr offWords `unsafeShiftR` offBits :: Word) +
            countBitsInWords (P.Vector (offWords + 1) (lWords - 2) arr) +
            popCount (indexByteArray arr (offWords + lWords - 1) .&. loMask nMod)
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
listBits (BitVec _ 0 _) = []
listBits (BitVec off len arr) | offBits == 0 = case modWordSize len of
    0    -> listBitsInWords 0 (P.Vector offWords lWords arr) []
    nMod -> listBitsInWords 0 (P.Vector offWords (lWords - 1) arr) $
            map (+ mulWordSize (lWords - 1)) $
            filter (testBit (indexByteArray arr (offWords + lWords - 1) :: Word)) [0 .. nMod - 1]
    where
        offBits  = modWordSize off
        offWords = divWordSize off
        lWords   = nWords (offBits + len)
listBits (BitVec off len arr) = case modWordSize (off + len) of
    0    -> filter (testBit (indexByteArray arr offWords `unsafeShiftR` offBits :: Word)) [0 .. wordSize - offBits - 1] ++
            listBitsInWords (wordSize - offBits) (P.Vector (offWords + 1) (lWords - 1) arr) []
    nMod -> case lWords of
        1 -> filter (testBit (indexByteArray arr offWords `unsafeShiftR` offBits :: Word)) [0 .. len - 1]
        _ ->
            filter (testBit (indexByteArray arr offWords `unsafeShiftR` offBits :: Word)) [0 .. wordSize - offBits - 1] ++
            (listBitsInWords (wordSize - offBits) (P.Vector (offWords + 1) (lWords - 2) arr) $
            map (+ (mulWordSize (lWords - 1) - offBits)) $
            filter (testBit (indexByteArray arr (offWords + lWords - 1) :: Word)) [0 .. nMod - 1])
    where
        offBits  = modWordSize off
        offWords = divWordSize off
        lWords   = nWords (offBits + len)

listBitsInWord :: Int -> Word -> [Int]
listBitsInWord offset word
    = map (+ offset)
    $ filter (testBit word)
    $ [0 .. wordSize - 1]

listBitsInWords :: Int -> P.Vector Word -> [Int] -> [Int]
listBitsInWords offset = flip $ P.ifoldr
    (\i word acc -> listBitsInWord (offset + mulWordSize i) word ++ acc)
