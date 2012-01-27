{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE BangPatterns               #-}
module Data.Vector.Unboxed.Mutable.Bit
     ( module Data.Bit
     , module U
     
     , wordSize
     , cloneFromWords
     , cloneToWords
     , readWord
     , writeWord
     
     , mapMInPlaceWithIndex
     , mapMInPlace
     , zipInPlace
     
     , unionInPlace
     , intersectionInPlace
     , differenceInPlace
     , symDiffInPlace
     , invertInPlace
     , selectBitsInPlace
     , excludeBitsInPlace
     
     , countBitsM
     
     , and
     , or
     
     , any
     , anyBits
     , all
     , allBits
     
     , reverseInPlace
     ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bit
import           Data.Bit.Internal
import           Data.Bits
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as MV
import           Data.Vector.Unboxed.Safe    as U hiding (and, or, any, all, reverse, findIndex)
import           Data.Vector.Unboxed.Bit.Internal
import           Data.Word
import           Prelude                     as P hiding (and, or, any, all, reverse)


-- TODO: make names more consistent, especially use of "M" in map/zip/etc functions.
-- Consider splitting part of this module into a "dense IntSet" interface.

-- |Clone a specified number of bits from a vector of words into a new vector of bits (interpreting the words in little-endian order, as described at 'indexWord').  If there are not enough words for the number of bits requested, the vector will be zero-padded.
cloneFromWords :: PrimMonad m => Int -> U.MVector (PrimState m) Word -> m (U.MVector (PrimState m) Bit)
cloneFromWords n ws = do
    let wordsNeeded = nWords n
        wordsGiven  = MV.length ws
        fillNeeded  = wordsNeeded - wordsGiven
    
    v <- MV.new wordsNeeded
    
    if fillNeeded > 0
        then do
            MV.copy (MV.slice          0 wordsGiven v) ws
            MV.set  (MV.slice wordsGiven fillNeeded v) 0
        else do
            MV.copy v (MV.slice 0 wordsNeeded ws)
    
    return (BitMVec 0 n v)

-- |clone a vector of bits to a new unboxed vector of words.  If the bits don't completely fill the words, the last word will be zero-padded.
cloneToWords :: PrimMonad m => U.MVector (PrimState m) Bit -> m (U.MVector (PrimState m) Word)
cloneToWords v@(BitMVec s n ws)
    | aligned s =
        if aligned n
            then justClone
            else do
                lastWord <- readWord v (alignDown n)
                if isMasked (modWordSize n) lastWord
                    then justClone
                    else cloneWords v
        
    | otherwise = cloneWords v
    where
        justClone = MV.clone (MV.slice (divWordSize s) (divWordSize n) ws)

-- |Map a function over a bit vector one 'Word' at a time ('wordSize' bits at a time).  The function will be passed the bit index (which will always be 'wordSize'-aligned) and the current value of the corresponding word.  The returned word will be written back to the vector.  If there is a partial word at the end of the vector, it will be zero-padded when passed to the function and truncated when the result is written back to the array.
{-# INLINE mapMInPlaceWithIndex #-}
mapMInPlaceWithIndex ::
    PrimMonad m =>
        (Int -> Word -> m Word)
     -> U.MVector (PrimState m) Bit -> m ()
mapMInPlaceWithIndex f xs = loop 0
    where
        !n = MV.length xs
        loop !i
            | i >= n    = return ()
            | otherwise = do
                x <- readWord xs i
                writeWord xs i =<< f i x
                loop (i + wordSize)

-- |Same as 'mapMInPlaceWithIndex' but without the index.
{-# INLINE mapMInPlace #-}
mapMInPlace :: PrimMonad m => (Word -> m Word) -> U.MVector (PrimState m) Bit -> m ()
mapMInPlace f = mapMInPlaceWithIndex (const f)

{-# INLINE zipInPlace #-}
zipInPlace :: PrimMonad m => (Word -> Word -> Word) -> U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
zipInPlace f xs ys =
    mapMInPlaceWithIndex g (MV.basicUnsafeSlice 0 (min n m) xs)
    where 
        n = MV.length xs
        m =  V.length ys
        g i x = return (f x (indexWord ys i))

unionInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
unionInPlace = zipInPlace (.|.)

intersectionInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
intersectionInPlace = zipInPlace (.&.)

differenceInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
differenceInPlace = zipInPlace diff

symDiffInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> U.Vector Bit -> m ()
symDiffInPlace = zipInPlace xor

-- |Flip every bit in the given vector
invertInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> m ()
invertInPlace = mapMInPlace (return . complement)

selectBitsInPlace :: PrimMonad m => U.Vector Bit -> U.MVector (PrimState m) Bit -> m Int
selectBitsInPlace is xs = loop 0 0
    where
        !n = min (V.length is) (MV.length xs)
        loop !i !ct
            | i >= n    = return ct
            | otherwise = do
                x <- readWord xs i
                let !(nSet, x') = selectWord (masked (n - i) (indexWord is i)) x
                writeWord xs ct x'
                loop (i + wordSize) (ct + nSet)

excludeBitsInPlace :: PrimMonad m => U.Vector Bit -> U.MVector (PrimState m) Bit -> m Int
excludeBitsInPlace is xs = loop 0 0
    where
        !n = min (V.length is) (MV.length xs)
        loop !i !ct
            | i >= n    = return ct
            | otherwise = do
                x <- readWord xs i
                let !(nSet, x') = selectWord (masked (n - i) (complement (indexWord is i))) x
                writeWord xs ct x'
                loop (i + wordSize) (ct + nSet)

-- |return the number of ones in a bit vector
countBitsM :: PrimMonad m => U.MVector (PrimState m) Bit -> m Int
countBitsM v = loop 0 0
    where
        !n = alignUp (MV.length v)
        loop !s !i
            | i >= n    = return s
            | otherwise = do
                x <- readWord v i
                loop (s + popCount x) (i + wordSize)

-- | Returns 'True' if all bits in the vector are set
and :: PrimMonad m => U.MVector (PrimState m) Bit -> m Bool
and v = loop 0
    where
        !n = MV.length v
        loop !i
            | i >= n    = return True
            | otherwise = do
                y <- readWord v i
                if y == mask (n - i)
                    then loop (i + wordSize)
                    else return False

-- | Returns 'True' if any bit in the vector is set
or :: PrimMonad m => U.MVector (PrimState m) Bit -> m Bool
or v = loop 0
    where
        !n = MV.length v
        loop !i
            | i >= n    = return False
            | otherwise = do
                y <- readWord v i
                if y /= 0
                    then return True
                    else loop (i + wordSize)

all :: PrimMonad m => (Bit -> Bool) -> U.MVector (PrimState m) Bit -> m Bool
all p = case (p 0, p 1) of
    (False, False) -> return . MV.null
    (False,  True) -> allBits 1
    (True,  False) -> allBits 0
    (True,   True) -> flip seq (return True)

any :: PrimMonad m => (Bit -> Bool) -> U.MVector (PrimState m) Bit -> m Bool
any p = case (p 0, p 1) of
    (False, False) -> flip seq (return False)
    (False,  True) -> anyBits 1
    (True,  False) -> anyBits 0
    (True,   True) -> return . not . MV.null

allBits, anyBits :: PrimMonad m => Bit -> U.MVector (PrimState m) Bit -> m Bool
allBits 0 = liftM not . or
allBits 1 = and

anyBits 0 = liftM not . and
anyBits 1 = or

reverseInPlace :: PrimMonad m => U.MVector (PrimState m) Bit -> m ()
reverseInPlace xs = loop 0 (MV.length xs)
    where
        loop !i !j
            | i' <= j'  = do
                x <- readWord xs i
                y <- readWord xs j'
                
                writeWord xs i  (reverseWord y)
                writeWord xs j' (reverseWord x)
                
                loop i' j'
            | i' < j    = do
                let w = (j - i) `shiftR` 1
                    k  = j - w
                x <- readWord xs i
                y <- readWord xs k
                
                writeWord xs i (meld w (reversePartialWord w y) x)
                writeWord xs k (meld w (reversePartialWord w x) y)
                
                loop i' j'
            | i  < j    = do
                let w = j - i
                x <- readWord xs i
                writeWord xs i (meld w (reversePartialWord w x) x)
            | otherwise = return ()
            where    
                !i' = i + wordSize
                !j' = j - wordSize
