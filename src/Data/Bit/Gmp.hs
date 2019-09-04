{-# LANGUAGE CPP              #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnliftedFFITypes #-}

#if UseLibGmp

module Data.Bit.Gmp
  ( mpnCom
  , mpnLshift
  , mpnRshift
  , mpnScan0
  , mpnScan1
  , mpnPopcount
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

foreign import ccall unsafe "__gmpn_lshift"
  mpn_lshift :: MutableByteArray# s -> ByteArray# -> Int# -> Word# -> IO Word

mpnLshift :: MutableByteArray s -> ByteArray -> Int -> Word -> ST s Word
mpnLshift (MutableByteArray res#) (ByteArray arg#) (I# limbs#) (W# count#) =
  unsafeIOToST (mpn_lshift res# arg# limbs# count#)
{-# INLINE mpnLshift #-}

foreign import ccall unsafe "__gmpn_rshift"
  mpn_rshift :: MutableByteArray# s -> ByteArray# -> Int# -> Word# -> IO Word

mpnRshift :: MutableByteArray s -> ByteArray -> Int -> Word -> ST s Word
mpnRshift (MutableByteArray res#) (ByteArray arg#) (I# limbs#) (W# count#) =
  unsafeIOToST (mpn_rshift res# arg# limbs# count#)
{-# INLINE mpnRshift #-}

foreign import ccall unsafe "__gmpn_scan0"
  mpn_scan0 :: ByteArray# -> Word# -> IO Word

mpnScan0 :: ByteArray -> Word -> Word
mpnScan0 (ByteArray arg#) (W# bit#) =
  unsafeDupablePerformIO (mpn_scan0 arg# bit#)
{-# INLINE mpnScan0 #-}

foreign import ccall unsafe "__gmpn_scan1"
  mpn_scan1 :: ByteArray# -> Word# -> IO Word

mpnScan1 :: ByteArray -> Word -> Word
mpnScan1 (ByteArray arg#) (W# bit#) =
  unsafeDupablePerformIO (mpn_scan1 arg# bit#)
{-# INLINE mpnScan1 #-}

foreign import ccall unsafe "__gmpn_popcount"
  mpn_popcount :: ByteArray# -> Int# -> IO Word

mpnPopcount :: ByteArray -> Int -> Word
mpnPopcount (ByteArray arg#) (I# limbs#) =
  unsafeDupablePerformIO (mpn_popcount arg# limbs#)
{-# INLINE mpnPopcount #-}

#else

module Data.Bit.Gmp where

#endif
