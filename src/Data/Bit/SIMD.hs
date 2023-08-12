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
  , bitIndexC
  , nthBitIndexC
  , selectBitsC
  ) where

import Control.Monad.ST
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Primitive.ByteArray
import GHC.Exts

foreign import ccall unsafe "_hs_bitvec_popcount"
  omp_popcount :: ByteArray# -> Int# -> Int#

-- | SIMD optimized popcount. The length is in 32 bit words.
ompPopcount :: ByteArray -> Int -> Int
ompPopcount (ByteArray arg#) (I# len#) =
  I# (omp_popcount arg# len#)
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

-- | The length is in words.
reverseBitsC :: MutableByteArray s -> ByteArray -> Int -> ST s ()
reverseBitsC (MutableByteArray res#) (ByteArray arg#) (I# len#) =
  unsafeIOToST (reverse_bits res# arg# len#)
{-# INLINE reverseBitsC #-}

foreign import ccall unsafe "_hs_bitvec_bit_index"
  bit_index :: ByteArray# -> Int# -> Bool -> Int#

bitIndexC :: ByteArray -> Int -> Bool -> Int
bitIndexC (ByteArray arg#) (I# len#) bit =
  I# (bit_index arg# len# bit)
{-# INLINE bitIndexC #-}

foreign import ccall unsafe "_hs_bitvec_nth_bit_index"
  nth_bit_index :: ByteArray# -> Int# -> Bool -> Int# -> Int#

nthBitIndexC :: ByteArray -> Int -> Bool -> Int -> Int
nthBitIndexC (ByteArray arg#) (I# len#) bit (I# n#) =
  I# (nth_bit_index arg# len# bit n#)
{-# INLINE nthBitIndexC #-}

foreign import ccall unsafe "_hs_bitvec_select_bits"
  select_bits_c :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> Bool -> IO Int

selectBitsC :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> Bool -> ST s Int
selectBitsC (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) exclude =
  unsafeIOToST (select_bits_c res# arg1# arg2# len# exclude)
{-# INLINE selectBitsC #-}
