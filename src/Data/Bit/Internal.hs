{-# LANGUAGE CPP                        #-}

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE ViewPatterns               #-}

#ifndef BITVEC_THREADSAFE
module Data.Bit.Internal
#else
module Data.Bit.InternalTS
#endif
    ( Bit(..)
    , U.Vector(BitVec)
    , U.MVector(BitMVec)
    , indexWord
    , readWord
    , writeWord

    , unsafeFlipBit
    , flipBit

    , nthBitIndex
    , countBits
    , listBits
    ) where

#include "vector.h"

import Control.Monad
import Control.Monad.Primitive
import Data.Bit.Select1
import Data.Bit.Utils
import Data.Bits
import Data.Typeable
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Primitive       as P
import qualified Data.Vector.Unboxed         as U

#ifdef BITVEC_THREADSAFE
import Data.Primitive.ByteArray
import GHC.Exts
#endif

-- | A newtype wrapper with a custom instance
-- of "Data.Vector.Unboxed", which packs booleans
-- as efficient as possible (8 values per byte).
-- Vectors of `Bit` use 8x less memory
-- than vectors of 'Bool' (which stores one value per byte),
-- but random writes
-- are slightly slower.
newtype Bit = Bit { unBit :: Bool }
    deriving (Bounded, Enum, Eq, Ord, FiniteBits, Bits, Typeable)

instance Show Bit where
    showsPrec _ (Bit False) = showString "0"
    showsPrec _ (Bit True ) = showString "1"

instance Read Bit where
    readsPrec p (' ':rest) = readsPrec p rest
    readsPrec _ ('0':rest) = [(Bit False, rest)]
    readsPrec _ ('1':rest) = [(Bit True, rest)]
    readsPrec _ _ = []

instance U.Unbox Bit

-- Ints are offset and length in bits
data instance U.MVector s Bit = BitMVec !Int !Int !(P.MVector s Word)
data instance U.Vector    Bit = BitVec  !Int !Int !(P.Vector    Word)

readBit :: Int -> Word -> Bit
readBit i w = Bit (w .&. (1 `unsafeShiftL` i) /= 0)
{-# INLINE readBit #-}

extendToWord :: Bit -> Word
extendToWord (Bit False) = 0
extendToWord (Bit True)  = complement 0

-- | read a word at the given bit offset in little-endian order (i.e., the LSB will correspond to the bit at the given address, the 2's bit will correspond to the address + 1, etc.).  If the offset is such that the word extends past the end of the vector, the result is zero-padded.
indexWord :: U.Vector Bit -> Int -> Word
indexWord (BitVec 0 n v) i
    | aligned i         = masked b lo
    | j + 1 == nWords n = masked b (extractWord k lo 0 )
    | otherwise         = masked b (extractWord k lo hi)
        where
            b = n - i
            j  = divWordSize i
            k  = modWordSize i
            lo = v V.!  j
            hi = v V.! (j+1)
indexWord (BitVec s n v) i = indexWord (BitVec 0 (n + s) v) (i + s)

-- | read a word at the given bit offset in little-endian order (i.e., the LSB will correspond to the bit at the given address, the 2's bit will correspond to the address + 1, etc.).  If the offset is such that the word extends past the end of the vector, the result is zero-padded.
readWord :: PrimMonad m => U.MVector (PrimState m) Bit -> Int -> m Word
readWord (BitMVec 0 n v) i
    | aligned i         = liftM (masked b) lo
    | j + 1 == nWords n = liftM (masked b) (liftM2 (extractWord k) lo (return 0))
    | otherwise         = liftM (masked b) (liftM2 (extractWord k) lo hi)
        where
            b = n - i
            j = divWordSize i
            k = modWordSize i
            lo = MV.read v  j
            hi = MV.read v (j+1)
readWord (BitMVec s n v) i = readWord (BitMVec 0 (n + s) v) (i + s)

-- | write a word at the given bit offset in little-endian order (i.e., the LSB will correspond to the bit at the given address, the 2's bit will correspond to the address + 1, etc.).  If the offset is such that the word extends past the end of the vector, the word is truncated and as many low-order bits as possible are written.
writeWord :: PrimMonad m => U.MVector (PrimState m) Bit -> Int -> Word -> m ()
writeWord (BitMVec 0 n v) i x
    | aligned i    =
        if b < wordSize
            then do
                y <- MV.read v j
                MV.write v j (meld b x y)
            else MV.write v j x
    | j + 1 == nWords n = do
        lo <- MV.read v  j
        let x' = if b < wordSize
                    then meld b x (extractWord k lo 0)
                    else x
            (lo', _hi) = spliceWord k lo 0 x'
        MV.write v  j    lo'
    | otherwise    = do
        lo <- MV.read v  j
        hi <- if j + 1 == nWords n
            then return 0
            else MV.read v (j+1)
        let x' = if b < wordSize
                    then meld b x (extractWord k lo hi)
                    else x
            (lo', hi') = spliceWord k lo hi x'
        MV.write v  j    lo'
        MV.write v (j+1) hi'
    where
        b = n - i
        j  = divWordSize i
        k  = modWordSize i
writeWord (BitMVec s n v) i x = writeWord (BitMVec 0 (n + s) v) (i + s) x

instance MV.MVector U.MVector Bit where
    {-# INLINE basicInitialize #-}
    basicInitialize (BitMVec _ 0 _) = pure ()
    basicInitialize (BitMVec 0 n v) = case modWordSize n of
        0 -> MV.basicInitialize v
        nMod -> do
            let vLen = MV.basicLength v
            MV.basicInitialize (MV.slice 0 (vLen - 1) v)
            MV.modify v (\val -> val .&. hiMask nMod) (vLen - 1)
    basicInitialize (BitMVec s n v) = case modWordSize (s + n) of
        0 -> do
            let vLen = MV.basicLength v
            MV.basicInitialize (MV.slice 1 (vLen - 1) v)
            MV.modify v (\val -> val .&. loMask s) 0
        nMod -> do
            let vLen = MV.basicLength v
                lohiMask = loMask s .|. hiMask nMod
            if vLen == 1
                then MV.modify v (\val -> val .&. lohiMask) 0
                else do
                    MV.basicInitialize (MV.slice 1 (vLen - 2) v)
                    MV.modify v (\val -> val .&. loMask s) 0
                    MV.modify v (\val -> val .&. hiMask nMod) (vLen - 1)

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew       n   = liftM (BitMVec 0 n) (MV.basicUnsafeNew       (nWords n))

    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n x = liftM (BitMVec 0 n) (MV.basicUnsafeReplicate (nWords n) (extendToWord x))

    {-# INLINE basicOverlaps #-}
    basicOverlaps (BitMVec _ _ v1) (BitMVec _ _ v2) = MV.basicOverlaps v1 v2

    {-# INLINE basicLength #-}
    basicLength      (BitMVec _ n _)     = n

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead  (BitMVec s _ v) !i'   = let i = s + i' in liftM (readBit (modWordSize i)) (MV.basicUnsafeRead v (divWordSize i))

    {-# INLINE basicUnsafeWrite #-}
#ifndef BITVEC_THREADSAFE
    basicUnsafeWrite (BitMVec s _ v) !i' !x = do
        let i = s + i'
        let j = divWordSize i; k = modWordSize i; kk = 1 `unsafeShiftL` k
        w <- MV.basicUnsafeRead v j
        when (Bit (w .&. kk /= 0) /= x) $
            MV.basicUnsafeWrite v j (w `xor` kk)
#else
    basicUnsafeWrite (BitMVec s _ (P.MVector o _ (MutableByteArray mba))) !i' (Bit b) = do
        let i       = s + i'
            !(I# j) = o + divWordSize i
            !(I# k) = 1 `unsafeShiftL` modWordSize i
        primitive $ \state ->
            let !(# state', _ #) = (if b then fetchOrIntArray# mba j k state else fetchAndIntArray# mba j (notI# k) state) in
                (# state', () #)
#endif

    {-# INLINE basicClear #-}
    basicClear _ = pure ()

    {-# INLINE basicSet #-}
    basicSet (BitMVec _ 0 _) _ = pure ()
    basicSet (BitMVec 0 n v) (extendToWord -> x) = case modWordSize n of
        0 ->  MV.basicSet v x
        nMod -> do
            let vLen = MV.basicLength v
            MV.basicSet (MV.slice 0 (vLen - 1) v) x
            MV.modify v (\val -> val .&. hiMask nMod .|. x .&. loMask nMod) (vLen - 1)
    basicSet (BitMVec s n v) (extendToWord -> x) = case modWordSize (s + n) of
        0 -> do
            let vLen = MV.basicLength v
            MV.basicSet (MV.slice 1 (vLen - 1) v) x
            MV.modify v (\val -> val .&. loMask s .|. x .&. hiMask s) 0
        nMod -> do
            let vLen = MV.basicLength v
                lohiMask = loMask s .|. hiMask nMod
            if vLen == 1
                then MV.modify v (\val -> val .&. lohiMask .|. x .&. complement lohiMask) 0
                else do
                    MV.basicSet (MV.slice 1 (vLen - 2) v) x
                    MV.modify v (\val -> val .&. loMask s .|. x .&. hiMask s) 0
                    MV.modify v (\val -> val .&. hiMask nMod .|. x .&. loMask nMod) (vLen - 1)

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy _ (BitMVec _ 0 _) = pure ()
    basicUnsafeCopy (BitMVec 0 _ dst) (BitMVec 0 n src) = case modWordSize n of
        0 -> MV.basicUnsafeCopy dst src
        nMod -> do
            let vLen = MV.basicLength src
            MV.basicUnsafeCopy (MV.slice 0 (vLen - 1) dst) (MV.slice 0 (vLen - 1) src)
            valSrc <- MV.basicUnsafeRead src (vLen - 1)
            MV.modify dst (\val -> val .&. hiMask nMod .|. valSrc .&. loMask nMod) (vLen - 1)
    basicUnsafeCopy (BitMVec dstShift _ dst) (BitMVec s n src)
        | dstShift == s = case modWordSize (s + n) of
            0 -> do
                let vLen = MV.basicLength src
                MV.basicUnsafeCopy (MV.slice 1 (vLen - 1) dst) (MV.slice 1 (vLen - 1) src)
                valSrc <- MV.basicUnsafeRead src 0
                MV.modify dst (\val -> val .&. loMask s .|. valSrc .&. hiMask s) 0
            nMod -> do
                let vLen = MV.basicLength src
                    lohiMask = loMask s .|. hiMask nMod
                if vLen == 1
                    then do
                        valSrc <- MV.basicUnsafeRead src 0
                        MV.modify dst (\val -> val .&. lohiMask .|. valSrc .&. complement lohiMask) 0
                    else do
                        MV.basicUnsafeCopy (MV.slice 1 (vLen - 2) dst) (MV.slice 1 (vLen - 2) src)
                        valSrcFirst <- MV.basicUnsafeRead src 0
                        MV.modify dst (\val -> val .&. loMask s .|. valSrcFirst .&. hiMask s) 0
                        valSrcLast <- MV.basicUnsafeRead src (vLen - 1)
                        MV.modify dst (\val -> val .&. hiMask nMod .|. valSrcLast .&. loMask nMod) (vLen - 1)

    basicUnsafeCopy dst@(BitMVec _ len _) src = do_copy 0
      where
        n = alignUp len

        do_copy i
            | i < n = do
                x <- readWord src i
                writeWord dst i x
                do_copy (i+wordSize)
            | otherwise = return ()

    {-# INLINE basicUnsafeMove #-}
    basicUnsafeMove !dst !src@(BitMVec srcShift srcLen _)
        | MV.basicOverlaps dst src = do
            -- Align shifts of src and srcCopy to speed up basicUnsafeCopy srcCopy src
            -- TODO write tests on copy and move inside array
            srcCopy <- BitMVec srcShift srcLen <$> MV.basicUnsafeNew (nWords (srcShift + srcLen))
            MV.basicUnsafeCopy srcCopy src
            MV.basicUnsafeCopy dst srcCopy
        | otherwise = MV.basicUnsafeCopy dst src

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice offset n (BitMVec s _ v) =
        BitMVec relStartBit n (MV.basicUnsafeSlice startWord (endWord - startWord) v)
            where
                absStartBit = s + offset
                relStartBit = modWordSize absStartBit
                absEndBit   = absStartBit + n
                endWord     = nWords absEndBit
                startWord   = divWordSize absStartBit

    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (BitMVec s n v) by =
        BitMVec s (n + by) <$> if delta == 0 then pure v else MV.basicUnsafeGrow v delta
        where
            delta = nWords (s + n + by) - nWords (s + n)

#ifndef BITVEC_THREADSAFE

-- | Flip the bit at the given position.
-- No bounds checks are performed.
-- Equivalent to 'flip' 'Data.Vector.Unboxed.Mutable.unsafeModify' 'Data.Bits.complement',
-- but slightly faster.
--
-- In general there is no reason to 'Data.Vector.Unboxed.Mutable.unsafeModify' bit vectors:
-- either you modify it with 'id' (which is 'id' altogether)
-- or with 'Data.Bits.complement' (which is 'unsafeFlipBit').
--
-- >>> Data.Vector.Unboxed.modify (\v -> unsafeFlipBit v 1) (read "[1,1,1]")
-- [1,0,1]
unsafeFlipBit :: PrimMonad m => U.MVector (PrimState m) Bit -> Int -> m ()
unsafeFlipBit (BitMVec s _ v) !i' = do
    let i = s + i'
    let j = divWordSize i; k = modWordSize i; kk = 1 `unsafeShiftL` k
    w <- MV.basicUnsafeRead v j
    MV.basicUnsafeWrite v j (w `xor` kk)
{-# INLINE unsafeFlipBit #-}

-- | Flip the bit at the given position.
-- Equivalent to 'flip' 'Data.Vector.Unboxed.Mutable.modify' 'Data.Bits.complement',
-- but slightly faster.
--
-- In general there is no reason to 'Data.Vector.Unboxed.Mutable.modify' bit vectors:
-- either you modify it with 'id' (which is 'id' altogether)
-- or with 'Data.Bits.complement' (which is 'flipBit').
--
-- >>> Data.Vector.Unboxed.modify (\v -> flipBit v 1) (read "[1,1,1]")
-- [1,0,1]
flipBit :: PrimMonad m => U.MVector (PrimState m) Bit -> Int -> m ()
flipBit v i = BOUNDS_CHECK(checkIndex) "flipBit" i (MV.length v) $ unsafeFlipBit v i
{-# INLINE flipBit #-}

#else

-- | Flip the bit at the given position.
-- No bounds checks are performed.
-- Equivalent to 'flip' 'Data.Vector.Unboxed.Mutable.unsafeModify' 'Data.Bits.complement',
-- but slightly faster and atomic.
--
-- In general there is no reason to 'Data.Vector.Unboxed.Mutable.unsafeModify' bit vectors:
-- either you modify it with 'id' (which is 'id' altogether)
-- or with 'Data.Bits.complement' (which is 'unsafeFlipBit').
--
-- >>> Data.Vector.Unboxed.modify (\v -> unsafeFlipBit v 1) (read "[1,1,1]")
-- [1,0,1]
unsafeFlipBit :: PrimMonad m => U.MVector (PrimState m) Bit -> Int -> m ()
unsafeFlipBit (BitMVec s _ (P.MVector o _ (MutableByteArray mba))) !i' = do
    let i       = s + i'
        !(I# j) = o + divWordSize i
        !(I# k) = 1 `unsafeShiftL` modWordSize i
    primitive $ \state ->
        let !(# state', _ #) = fetchXorIntArray# mba j k state in
            (# state', () #)
{-# INLINE unsafeFlipBit #-}

-- | Flip the bit at the given position.
-- Equivalent to 'flip' 'Data.Vector.Unboxed.Mutable.modify' 'Data.Bits.complement',
-- but slightly faster and atomic
--
-- In general there is no reason to 'Data.Vector.Unboxed.Mutable.modify' bit vectors:
-- either you modify it with 'id' (which is 'id' altogether)
-- or with 'Data.Bits.complement' (which is 'flipBit').
--
-- >>> Data.Vector.Unboxed.modify (\v -> flipBit v 1) (read "[1,1,1]")
-- [1,0,1]
flipBit :: PrimMonad m => U.MVector (PrimState m) Bit -> Int -> m ()
flipBit v i = BOUNDS_CHECK(checkIndex) "flipBit" i (MV.length v) $ unsafeFlipBit v i
{-# INLINE flipBit #-}

#endif

instance V.Vector U.Vector Bit where
    basicUnsafeFreeze (BitMVec s n v) = liftM (BitVec  s n) (V.basicUnsafeFreeze v)
    basicUnsafeThaw   (BitVec  s n v) = liftM (BitMVec s n) (V.basicUnsafeThaw   v)
    basicLength       (BitVec  _ n _) = n

    basicUnsafeIndexM (BitVec s _ v) !i' = let i = s + i' in liftM (readBit (modWordSize i)) (V.basicUnsafeIndexM v (divWordSize i))

    basicUnsafeCopy dst src = do
        src1 <- V.basicUnsafeThaw src
        MV.basicUnsafeCopy dst src1

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice offset n (BitVec s _ v) =
        BitVec relStartBit n (V.basicUnsafeSlice startWord (endWord - startWord) v)
            where
                absStartBit = s + offset
                relStartBit = modWordSize absStartBit
                absEndBit   = absStartBit + n
                endWord     = nWords absEndBit
                startWord   = divWordSize absStartBit

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
nthBitIndex _ k
    | k <= 0 = error "nthBitIndex: n must be positive"
nthBitIndex (Bit True) k = \case
    BitVec _ 0 _ -> Nothing
    BitVec 0 n v -> let l = V.basicLength v in case modWordSize n of
        0 -> case nth1InWords k v of
            Right x -> Just x
            Left{}  -> Nothing
        nMod -> case nth1InWords k (V.slice 0 (l - 1) v) of
            Right x -> Just x
            Left k' -> case nth1 k' (V.last v .&. loMask nMod) of
                Right x -> Just $ mulWordSize (l - 1) + x
                Left{}  -> Nothing
    BitVec s n v -> let l = V.basicLength v in case modWordSize (s + n) of
        0 -> case nth1 k (V.head v `unsafeShiftR` s) of
            Right x -> Just x
            Left k' -> case nth1InWords k' (V.slice 1 (l - 1) v) of
                Right x -> Just $ wordSize - s + x
                Left {} -> Nothing
        nMod -> case l of
            1 -> case nth1 k ((V.head v `unsafeShiftR` s) .&. loMask n) of
                Right x -> Just x
                Left{}  -> Nothing
            _ -> case nth1 k (V.head v `unsafeShiftR` s) of
                Right x -> Just x
                Left k' -> case nth1InWords k' (V.slice 1 (l - 2) v) of
                    Right x  -> Just $ wordSize - s + x
                    Left k'' -> case nth1 k'' (V.last v .&. loMask nMod) of
                        Right x -> Just $ mulWordSize (l - 1) - s + x
                        Left{}  -> Nothing
nthBitIndex (Bit False) k = \case
    BitVec _ 0 _ -> Nothing
    BitVec 0 n v -> let l = V.basicLength v in case modWordSize n of
        0 -> case nth0InWords k v of
            Right x -> Just x
            Left{}  -> Nothing
        nMod -> case nth0InWords k (V.slice 0 (l - 1) v) of
            Right x -> Just x
            Left k' -> case nth0 k' (V.last v .|. hiMask nMod) of
                Right x -> Just $ mulWordSize (l - 1) + x
                Left{}  -> Nothing
    BitVec s n v -> let l = V.basicLength v in case modWordSize (s + n) of
        0 -> case nth0 k (V.head v `unsafeShiftR` s .|. hiMask (wordSize - s)) of
            Right x -> Just x
            Left k' -> case nth0InWords k' (V.slice 1 (l - 1) v) of
                Right x -> Just $ wordSize - s + x
                Left {} -> Nothing
        nMod -> case l of
            1 -> case nth0 k ((V.head v `unsafeShiftR` s) .|. hiMask n) of
                Right x -> Just x
                Left{}  -> Nothing
            _ -> case nth0 k ((V.head v `unsafeShiftR` s) .|. hiMask (wordSize - s)) of
                Right x -> Just x
                Left k' -> case nth0InWords k' (V.slice 1 (l - 2) v) of
                    Right x  -> Just $ wordSize - s + x
                    Left k'' -> case nth0 k'' (V.last v .|. hiMask nMod) of
                        Right x -> Just $ mulWordSize (l - 1) - s + x
                        Left{}  -> Nothing

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
countBits (BitVec 0 n v) = case modWordSize n of
    0    -> countBitsInWords v
    nMod -> countBitsInWords (V.slice 0 (l - 1) v) +
            popCount (V.last v .&. loMask nMod)
    where
        l = V.basicLength v
countBits (BitVec s n v) = case modWordSize (s + n) of
    0    -> popCount (V.head v `unsafeShiftR` s) +
            countBitsInWords (V.slice 1 (l - 1) v)
    nMod -> case l of
        1 -> popCount ((V.head v `unsafeShiftR` s) .&. loMask n)
        _ ->
            popCount (V.head v `unsafeShiftR` s) +
            countBitsInWords (V.slice 1 (l - 2) v) +
            popCount (V.last v .&. loMask nMod)
    where
        l = V.basicLength v

countBitsInWords :: P.Vector Word -> Int
countBitsInWords = P.foldl' (\acc word -> popCount word + acc) 0

-- | Return the indices of set bits in a vector.
--
-- >>> listBits (read "[1,1,0,1,0,1]")
-- [0,1,3,5]
listBits :: U.Vector Bit -> [Int]
listBits (BitVec _ 0 _) = []
listBits (BitVec 0 n v) = case modWordSize n of
    0    -> listBitsInWords 0 v []
    nMod -> listBitsInWords 0 (V.slice 0 (l - 1) v) $
            map (+ mulWordSize (l - 1)) $
            filter (testBit $ V.last v) [0 .. nMod - 1]
    where
        l = V.basicLength v
listBits (BitVec s n v) = case modWordSize (s + n) of
    0    -> filter (testBit $ V.head v `unsafeShiftR` s) [0 .. wordSize - s - 1] ++
            listBitsInWords (wordSize - s) (V.slice 1 (l - 1) v) []
    nMod -> case l of
        1 -> filter (testBit $ V.head v `unsafeShiftR` s) [0 .. n - 1]
        _ ->
            filter (testBit $ V.head v `unsafeShiftR` s) [0 .. wordSize - s - 1] ++
            (listBitsInWords (wordSize - s) (V.slice 1 (l - 2) v) $
            map (+ (mulWordSize (l - 1) - s)) $
            filter (testBit $ V.last v) [0 .. nMod - 1])
    where
        l = V.basicLength v

listBitsInWord :: Int -> Word -> [Int]
listBitsInWord offset word
    = map (+ offset)
    $ filter (testBit word)
    $ [0 .. wordSize - 1]

listBitsInWords :: Int -> P.Vector Word -> [Int] -> [Int]
listBitsInWords offset = flip $ P.ifoldr
    (\i word acc -> listBitsInWord (offset + mulWordSize i) word ++ acc)
