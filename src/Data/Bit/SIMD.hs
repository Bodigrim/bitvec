{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Data.Bit.SIMD
  ( ompCom
  , ompAnd
  , ompIor
  , ompXor
  , ompAndn
  , ompIorn
  , ompNand
  , ompNior
  , ompXnor
  ) where

import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Primitive.ByteArray
import GHC.Exts

foreign import ccall unsafe "_hs_bitvec_com"
  omp_com :: MutableByteArray# s -> ByteArray# -> Int# -> IO ()

ompCom :: MutableByteArray s -> ByteArray -> Int -> ST s ()
ompCom (MutableByteArray res#) (ByteArray arg#) (I# len#) =
  unsafeIOToST (omp_com res# arg# len#)
{-# INLINE ompCom #-}

foreign import ccall unsafe "_hs_bitvec_and"
  omp_and :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

ompAnd :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompAnd (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_and res# arg1# arg2# len#)
{-# INLINE ompAnd #-}

foreign import ccall unsafe "_hs_bitvec_ior"
  omp_ior :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

ompIor :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompIor (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_ior res# arg1# arg2# len#)
{-# INLINE ompIor #-}

foreign import ccall unsafe "_hs_bitvec_xor"
  omp_xor :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

ompXor :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompXor (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_xor res# arg1# arg2# len#)
{-# INLINE ompXor #-}

foreign import ccall unsafe "_hs_bitvec_andn"
  omp_andn :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

ompAndn :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompAndn (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_andn res# arg1# arg2# len#)
{-# INLINE ompAndn #-}

foreign import ccall unsafe "_hs_bitvec_iorn"
  omp_iorn :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

ompIorn :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompIorn (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_iorn res# arg1# arg2# len#)
{-# INLINE ompIorn #-}

foreign import ccall unsafe "_hs_bitvec_nand"
  omp_nand :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

ompNand :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompNand (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_nand res# arg1# arg2# len#)
{-# INLINE ompNand #-}

foreign import ccall unsafe "_hs_bitvec_nior"
  omp_nior :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

ompNior :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompNior (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_nior res# arg1# arg2# len#)
{-# INLINE ompNior #-}

foreign import ccall unsafe "_hs_bitvec_xnor"
  omp_xnor :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

ompXnor :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
ompXnor (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# len#) =
  unsafeIOToST (omp_xnor res# arg1# arg2# len#)
{-# INLINE ompXnor #-}
