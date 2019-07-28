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
import Data.Primitive.ByteArray
import Data.Typeable
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Primitive       as P
import qualified Data.Vector.Unboxed         as U

#ifdef BITVEC_THREADSAFE
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
    basicInitialize vec = MV.basicSet vec (Bit False)

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n
        | n < 0 = error $ "Data.Bit.basicUnsafeNew: negative length: " ++ show n
        | otherwise = do
            arr <- newByteArray (wordsToBytes $ nWords n)
            pure $ BitMVec 0 n $ P.MVector 0 (nWords n) arr

    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n x
        | n < 0 = error $ "Data.Bit.basicUnsafeReplicate: negative length: " ++ show n
        | otherwise = do
            arr <- newByteArray (wordsToBytes $ nWords n)
            setByteArray arr 0 (nWords n) (extendToWord x :: Word)
            pure $ BitMVec 0 n $ P.MVector 0 (nWords n) arr

    {-# INLINE basicOverlaps #-}
    basicOverlaps (BitMVec _ _ (P.MVector i m arr1)) (BitMVec _ _ (P.MVector j n arr2)) =
        sameMutableByteArray arr1 arr2 && (between i j (j + n) || between j i (i + m))
        where
          between x y z = x >= y && x < z

    {-# INLINE basicLength #-}
    basicLength (BitMVec _ n _) = n

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead  (BitMVec offBits _ (P.MVector offWords _ arr)) !i' = do
        let i = offBits + i'
        word <- readByteArray arr (offWords + divWordSize i)
        pure $ readBit (modWordSize i) word

    {-# INLINE basicUnsafeWrite #-}
#ifndef BITVEC_THREADSAFE
    basicUnsafeWrite (BitMVec offBits _ (P.MVector offWords _ arr)) !i' !x = do
        let i = offBits + i'
            j = divWordSize i
            k = modWordSize i
            kk = 1 `unsafeShiftL` k :: Word
        word <- readByteArray arr (offWords + j)
        when (Bit (word .&. kk /= 0) /= x) $
            writeByteArray arr (offWords + j) (word `xor` kk)
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
    basicSet (BitMVec 0 lBits (P.MVector offWords lWords arr)) (extendToWord -> x) = case modWordSize lBits of
        0 -> setByteArray arr offWords lWords (x :: Word)
        nMod -> do
            setByteArray arr offWords (lWords - 1) (x :: Word)
            lastWord <- readByteArray arr (offWords + lWords - 1)
            let lastWord' = lastWord .&. hiMask nMod .|. x .&. loMask nMod
            writeByteArray arr (offWords + lWords - 1) lastWord'
    basicSet (BitMVec offBits lBits (P.MVector offWords lWords arr)) (extendToWord -> x) = case modWordSize (offBits + lBits) of
        0 -> do
            firstWord <- readByteArray arr offWords
            let firstWord' = firstWord .&. loMask offBits .|. x .&. hiMask offBits
            writeByteArray arr offWords firstWord'
            setByteArray arr (offWords + 1) (lWords - 1) (x :: Word)
        nMod -> if lWords == 1 then do
                theOnlyWord <- readByteArray arr offWords
                let lohiMask = loMask offBits .|. hiMask nMod
                    theOnlyWord' = theOnlyWord .&. lohiMask .|. x .&. complement lohiMask
                writeByteArray arr offWords theOnlyWord'
            else do
                firstWord <- readByteArray arr offWords
                let firstWord' = firstWord .&. loMask offBits .|. x .&. hiMask offBits
                writeByteArray arr offWords firstWord'

                setByteArray arr (offWords + 1) (lWords - 2) (x :: Word)

                lastWord <- readByteArray arr (offWords + lWords - 1)
                let lastWord' = lastWord .&. hiMask nMod .|. x .&. loMask nMod
                writeByteArray arr (offWords + lWords - 1) lastWord'

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy _ (BitMVec _ 0 _) = pure ()
    basicUnsafeCopy (BitMVec 0 lDstBits (P.MVector offDstWords lDstWords dst)) (BitMVec 0 _ (P.MVector offSrcWords _ src)) = case modWordSize lDstBits of
        0 -> copyMutableByteArray dst (wordsToBytes offDstWords) src (wordsToBytes offSrcWords) (wordsToBytes lDstWords)
        nMod -> do
            copyMutableByteArray dst (wordsToBytes offDstWords) src (wordsToBytes offSrcWords) (wordsToBytes $ lDstWords - 1)

            lastWordSrc <- readByteArray src (offSrcWords + lDstWords - 1)
            lastWordDst <- readByteArray dst (offDstWords + lDstWords - 1)
            let lastWordDst' = lastWordDst .&. hiMask nMod .|. lastWordSrc .&. loMask nMod
            writeByteArray dst (offDstWords + lDstWords - 1) lastWordDst'
    basicUnsafeCopy (BitMVec offDstBits lDstBits (P.MVector offDstWords lDstWords dst)) (BitMVec offSrcBits _ (P.MVector offSrcWords _ src))
        | offDstBits == offSrcBits = case modWordSize (offSrcBits + lDstBits) of
            0 -> do
                firstWordSrc <- readByteArray src offSrcWords
                firstWordDst <- readByteArray dst offDstWords
                let firstWordDst' = firstWordDst .&. loMask offSrcBits .|. firstWordSrc .&. hiMask offSrcBits
                writeByteArray dst offDstWords firstWordDst'

                copyMutableByteArray dst (wordsToBytes $ offDstWords + 1) src (wordsToBytes $ offSrcWords + 1) (wordsToBytes $ lDstWords - 1)
            nMod -> if lDstWords == 1 then do
                    let lohiMask = loMask offSrcBits .|. hiMask nMod
                    theOnlyWordSrc <- readByteArray src offSrcWords
                    theOnlyWordDst <- readByteArray dst offDstWords
                    let theOnlyWordDst' = theOnlyWordDst .&. lohiMask .|. theOnlyWordSrc .&. complement lohiMask
                    writeByteArray dst offDstWords theOnlyWordDst'
                else do
                    firstWordSrc <- readByteArray src offSrcWords
                    firstWordDst <- readByteArray dst offDstWords
                    let firstWordDst' = firstWordDst .&. loMask offSrcBits .|. firstWordSrc .&. hiMask offSrcBits
                    writeByteArray dst offDstWords firstWordDst'

                    copyMutableByteArray dst (wordsToBytes $ offDstWords + 1) src (wordsToBytes $ offSrcWords + 1) (wordsToBytes $ lDstWords - 2)

                    lastWordSrc <- readByteArray src (offSrcWords + lDstWords - 1)
                    lastWordDst <- readByteArray dst (offDstWords + lDstWords - 1)
                    let lastWordDst' = lastWordDst .&. hiMask nMod .|. lastWordSrc .&. loMask nMod
                    writeByteArray dst (offDstWords + lDstWords - 1) lastWordDst'

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
            srcCopy <- MV.drop srcShift <$> MV.basicUnsafeNew (srcShift + srcLen)
            MV.basicUnsafeCopy srcCopy src
            MV.basicUnsafeCopy dst srcCopy
        | otherwise = MV.basicUnsafeCopy dst src

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice offset n (BitMVec s _ (P.MVector i _ arr)) =
        BitMVec relStartBit n (P.MVector (i + startWord) (endWord - startWord) arr)
            where
                absStartBit = s + offset
                relStartBit = modWordSize absStartBit
                absEndBit   = absStartBit + n
                endWord     = nWords absEndBit
                startWord   = divWordSize absStartBit

    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (BitMVec offBits lBits v@(P.MVector offWords lWords src)) byBits
        | byWords == 0 = pure $ BitMVec offBits (lBits + byBits) v
        | otherwise = do
            dst <- newByteArray (wordsToBytes $ lWords + byWords)
            copyMutableByteArray dst 0 src (wordsToBytes offWords) (wordsToBytes lWords)
            pure $ BitMVec offBits (lBits + byBits) $ P.MVector 0 (lWords + byWords) dst
        where
            byWords = nWords (offBits + lBits + byBits) - nWords (offBits + lBits)

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
