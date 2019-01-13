{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Vector.Unboxed.Bit.Internal
     ( Bit
     , U.Vector(BitVec)
     , U.MVector(BitMVec)

     , padWith
     , pad

     , indexWord
     , readWord
     , writeWord
     , cloneWords
     ) where

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Primitive
import           Data.Bit.Internal
import           Data.Bits
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed         as U

-- Ints are offset and length in bits
data instance U.MVector s Bit = BitMVec !Int !Int !(U.MVector s Word)
data instance U.Vector    Bit = BitVec  !Int !Int !(U.Vector    Word)

-- TODO: allow partial words to be read/written at beginning?

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

-- clone words from a bit-array into a new word array, without attempting any shortcuts (such as recognizing that they are already aligned, etc.)
{-# INLINE cloneWords #-}
cloneWords :: PrimMonad m => U.MVector (PrimState m) Bit -> m (U.MVector (PrimState m) Word)
cloneWords v@(BitMVec _ n _) = do
    ws <- MV.new (nWords n)
    let loop !i !j
            | i >= n    = return ()
            | otherwise = do
                readWord v i >>= MV.write ws j
                loop (i + wordSize) (j + 1)
    loop 0 0
    return ws

instance U.Unbox Bit

instance MV.MVector U.MVector Bit where
#if MIN_VERSION_vector(0,11,0)
    basicInitialize (BitMVec _ _ v) = MV.basicInitialize v
#endif

    basicUnsafeNew       n   = liftM (BitMVec 0 n) (MV.basicUnsafeNew       (nWords n))
    basicUnsafeReplicate n x = liftM (BitMVec 0 n) (MV.basicUnsafeReplicate (nWords n) (extendToWord x))

    basicOverlaps (BitMVec _ _ v1) (BitMVec _ _ v2) = MV.basicOverlaps v1 v2

    basicLength      (BitMVec _ n _)     = n
    basicUnsafeRead  (BitMVec 0 _ v) i   = liftM (readBit (modWordSize i)) (MV.basicUnsafeRead v (divWordSize i))
    basicUnsafeRead  (BitMVec s n v) i   = MV.basicUnsafeRead (BitMVec 0 (n + s) v) (i + s)
    basicUnsafeWrite (BitMVec 0 _ v) i x = do
        let j = divWordSize i; k = modWordSize i
        w <- MV.basicUnsafeRead v j
        MV.basicUnsafeWrite v j $ if toBool x
            then setBit   w k
            else clearBit w k

    basicUnsafeWrite (BitMVec s n v) i x =
         MV.basicUnsafeWrite (BitMVec 0 (n + s) v) (i + s) x
    basicSet         (BitMVec _ _ v)   x = MV.basicSet v (extendToWord x)

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy dst@(BitMVec _ len _) src = do_copy 0
      where
        n = alignUp len

        do_copy i
            | i < n = do
                x <- readWord src i
                writeWord dst i x
                do_copy (i+wordSize)
            | otherwise = return ()

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice offset n (BitMVec s _ v) =
        BitMVec relStartBit n (MV.basicUnsafeSlice startWord (endWord - startWord) v)
            where
                absStartBit = s + offset
                relStartBit = modWordSize absStartBit
                absEndBit   = absStartBit + n
                endWord     = nWords absEndBit
                startWord   = divWordSize absStartBit

instance V.Vector U.Vector Bit where
    basicUnsafeFreeze (BitMVec s n v) = liftM (BitVec  s n) (V.basicUnsafeFreeze v)
    basicUnsafeThaw   (BitVec  s n v) = liftM (BitMVec s n) (V.basicUnsafeThaw   v)
    basicLength       (BitVec  _ n _) = n

    basicUnsafeIndexM (BitVec 0 _ v) i = liftM (readBit (modWordSize i)) (V.basicUnsafeIndexM v (divWordSize i))
    basicUnsafeIndexM (BitVec s n v) i = V.basicUnsafeIndexM (BitVec 0 (n + s) v) (i + s)

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

padWith :: Bit -> Int -> U.Vector Bit -> U.Vector Bit
padWith b n' bitvec@(BitVec _ n _)
    | n' <= n   = bitvec
    | otherwise = runST $ do
        mv@(BitMVec mvStart _ ws) <- MV.replicate n' b
        when (mvStart /= 0) (fail "assertion failed: offset /= 0 after MV.new")

        V.copy (MV.basicUnsafeSlice 0 n mv) bitvec

        when (notAligned n) $ do
            let i = divWordSize n
                j = modWordSize n
            x <- MV.read ws i
            MV.write ws i (meld j x (extendToWord b))

        V.unsafeFreeze mv

pad :: Int -> U.Vector Bit -> U.Vector Bit
pad = padWith (fromBool False)
