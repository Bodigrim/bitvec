{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Data.Bit.Gmp
  ( mpnCom
  , mpnPopcount
  , mpnAndN
  , mpnIorN
  , mpnXorN
  , mpnAndnN
  , mpnIornN
  , mpnNandN
  , mpnNiorN
  , mpnXnorN
  ) where

import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Primitive.ByteArray
import GHC.Exts
import System.IO.Unsafe

foreign import ccall unsafe "__gmpn_com"
  mpn_com :: MutableByteArray# s -> ByteArray# -> Int# -> IO ()

mpnCom :: MutableByteArray s -> ByteArray -> Int -> ST s ()
mpnCom (MutableByteArray res#) (ByteArray arg#) (I# limbs#) =
  unsafeIOToST (mpn_com res# arg# limbs#)
{-# INLINE mpnCom #-}

foreign import ccall unsafe "__gmpn_popcount"
  mpn_popcount :: ByteArray# -> Int# -> IO Word

mpnPopcount :: ByteArray -> Int -> Word
mpnPopcount (ByteArray arg#) (I# limbs#) =
  unsafeDupablePerformIO (mpn_popcount arg# limbs#)
{-# INLINE mpnPopcount #-}

foreign import ccall unsafe "__gmpn_and_n"
  mpn_and_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

mpnAndN :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
mpnAndN (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# limbs#) =
  unsafeIOToST (mpn_and_n res# arg1# arg2# limbs#)
{-# INLINE mpnAndN #-}

foreign import ccall unsafe "__gmpn_ior_n"
  mpn_ior_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

mpnIorN :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
mpnIorN (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# limbs#) =
  unsafeIOToST (mpn_ior_n res# arg1# arg2# limbs#)
{-# INLINE mpnIorN #-}

foreign import ccall unsafe "__gmpn_xor_n"
  mpn_xor_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

mpnXorN :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
mpnXorN (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# limbs#) =
  unsafeIOToST (mpn_xor_n res# arg1# arg2# limbs#)
{-# INLINE mpnXorN #-}

foreign import ccall unsafe "__gmpn_andn_n"
  mpn_andn_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

mpnAndnN :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
mpnAndnN (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# limbs#) =
  unsafeIOToST (mpn_andn_n res# arg1# arg2# limbs#)
{-# INLINE mpnAndnN #-}

foreign import ccall unsafe "__gmpn_iorn_n"
  mpn_iorn_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

mpnIornN :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
mpnIornN (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# limbs#) =
  unsafeIOToST (mpn_iorn_n res# arg1# arg2# limbs#)
{-# INLINE mpnIornN #-}

foreign import ccall unsafe "__gmpn_nand_n"
  mpn_nand_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

mpnNandN :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
mpnNandN (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# limbs#) =
  unsafeIOToST (mpn_nand_n res# arg1# arg2# limbs#)
{-# INLINE mpnNandN #-}

foreign import ccall unsafe "__gmpn_nior_n"
  mpn_nior_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

mpnNiorN :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
mpnNiorN (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# limbs#) =
  unsafeIOToST (mpn_nior_n res# arg1# arg2# limbs#)
{-# INLINE mpnNiorN #-}

foreign import ccall unsafe "__gmpn_xnor_n"
  mpn_xnor_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> Int# -> IO ()

mpnXnorN :: MutableByteArray s -> ByteArray -> ByteArray -> Int -> ST s ()
mpnXnorN (MutableByteArray res#) (ByteArray arg1#) (ByteArray arg2#) (I# limbs#) =
  unsafeIOToST (mpn_xnor_n res# arg1# arg2# limbs#)
{-# INLINE mpnXnorN #-}
