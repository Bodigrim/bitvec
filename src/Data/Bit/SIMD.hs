{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Data.Bit.SIMD
  ( ompPopcount
  , ompCom
  , ompAnd
  , ompIor
  , ompXor
  , ompAndn
  , ompIorn
  , ompNand
  , ompNior
  , ompXnor
  , reverseBitsC
  ) where

import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Primitive.ByteArray
import GHC.Exts
import System.IO.Unsafe

foreign import ccall unsafe "_hs_bitvec_popcount"
  omp_popcount :: ByteArray# -> Int# -> IO Int

-- | SIMD optimized popcount. The length is in 32 bit words.
ompPopcount :: ByteArray -> Int -> Int
ompPopcount (ByteArray arg#) (I# len#) =
  unsafeDupablePerformIO (omp_popcount arg# len#)
{-# INLINE ompPopcount #-}

foreign import ccall unsafe "_hs_bitvec_com"
  omp_com :: MutableByteArray# s -> ByteArray# -> Int# -> IO ()

-- | SIMD optimized bitwise complement. The length is in bytes
-- and the result array should have at least that many bytes.
ompCom :: MutableByteArray s -> ByteArray -> Int -> ST s ()
ompCom (MutableByteArray res#) (ByteArray arg#) (I# len#) =
  unsafeIOToST (omp_com res# arg# len#)
{-# INLINE ompCom #-}

foreign import ccall unsafe "_hs_bitvec_and"
  omp_and :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

-- | SIMD optimized bitwise AND. The length is in bytes
-- and the result array should have at least that many bytes.
ompAnd :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompAnd (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_and res# arg1# arg2# len#)
{-# INLINE ompAnd #-}

foreign import ccall unsafe "_hs_bitvec_ior"
  omp_ior :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

-- | SIMD optimized bitwise OR. The length is in bytes
-- and the result array should have at least that many bytes.
ompIor :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompIor (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_ior res# arg1# arg2# len#)
{-# INLINE ompIor #-}

foreign import ccall unsafe "_hs_bitvec_xor"
  omp_xor :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

-- | SIMD optimized bitwise XOR. The length is in bytes
-- and the result array should have at least that many bytes.
ompXor :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompXor (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_xor res# arg1# arg2# len#)
{-# INLINE ompXor #-}

foreign import ccall unsafe "_hs_bitvec_andn"
  omp_andn :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

-- | SIMD optimized bitwise AND with the second argument inverted. The length is in bytes
-- and the result array should have at least that many bytes.
ompAndn :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompAndn (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_andn res# arg1# arg2# len#)
{-# INLINE ompAndn #-}

foreign import ccall unsafe "_hs_bitvec_iorn"
  omp_iorn :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

-- | SIMD optimized bitwise OR with the second argument inverted. The length is in bytes
-- and the result array should have at least that many bytes.
ompIorn :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompIorn (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_iorn res# arg1# arg2# len#)
{-# INLINE ompIorn #-}

foreign import ccall unsafe "_hs_bitvec_nand"
  omp_nand :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

-- | SIMD optimized bitwise NAND. The length is in bytes
-- and the result array should have at least that many bytes.
ompNand :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompNand (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_nand res# arg1# arg2# len#)
{-# INLINE ompNand #-}

foreign import ccall unsafe "_hs_bitvec_nior"
  omp_nior :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

-- | SIMD optimized bitwise NOR. The length is in bytes
-- and the result array should have at least that many bytes.
ompNior :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompNior (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_nior res# arg1# arg2# len#)
{-# INLINE ompNior #-}

foreign import ccall unsafe "_hs_bitvec_xnor"
  omp_xnor :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

-- | SIMD optimized bitwise XNOR. The length is in bytes
-- and the result array should have at least that many bytes.
ompXnor :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompXnor (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_xnor res# arg1# arg2# len#)
{-# INLINE ompXnor #-}

foreign import ccall unsafe "_hs_bitvec_reverse_bits"
  reverse_bits :: MutableByteArray# s -> ByteArray# -> Int# -> IO ()

-- | The length is in 32 bit words.
reverseBitsC :: MutableByteArray s -> ByteArray -> Int -> ST s ()
reverseBitsC (MutableByteArray res#) (ByteArray arg#) (I# len#) =
  unsafeIOToST (reverse_bits res# arg# len#)
{-# INLINE reverseBitsC #-}
