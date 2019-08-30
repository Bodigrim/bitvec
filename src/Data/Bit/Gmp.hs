{-# LANGUAGE CPP              #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnliftedFFITypes #-}

#if UseLibGmp

module Data.Bit.Gmp
  ( mpnCom
  ) where

import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Primitive.ByteArray
import GHC.Exts

foreign import ccall unsafe "__gmpn_com"
  c_mpn_com :: MutableByteArray# s -> ByteArray# -> Int# -> IO ()

mpnCom :: MutableByteArray s -> ByteArray -> Int -> ST s ()
mpnCom (MutableByteArray res#) (ByteArray arg#) (I# w#) =
  unsafeIOToST (c_mpn_com res# arg# w#)
{-# INLINE mpnCom #-}

#else

module Data.Bit.Gmp where

#endif
