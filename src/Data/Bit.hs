{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Bit
     ( Bit
     , fromBool
     , toBool
     ) where

import Data.Bit.Internal
import Data.Bits
import Data.Vector.Unboxed.Bit.Internal ({- instance Unbox Bit -})

instance Show Bit where
    showsPrec _ (Bit False) = showString "0"
    showsPrec _ (Bit True ) = showString "1"
instance Read Bit where
    readsPrec _ ('0':rest) = [(0, rest)]
    readsPrec _ ('1':rest) = [(1, rest)]
    readsPrec _ _ = []


liftBool2 :: (Bool -> Bool -> Bool) -> (Bit -> Bit -> Bit)
liftBool2 op x y = fromBool (toBool x `op` toBool y)
liftInt2  :: (Int -> Int -> Int) -> (Bit -> Bit -> Bit)
liftInt2  op x y = fromIntegral (fromIntegral x `op` fromIntegral y)

-- | The 'Num' instance is currently based on integers mod 2, so (+) and (-) are
-- XOR, (*) is AND, and all the unary operations are identities.  Saturating
-- operations would also be a sensible alternative.
instance Num Bit where
    fromInteger = fromBool . odd
    (+) = liftInt2 (+)
    (-) = liftInt2 (-)
    (*) = liftInt2 (*)
    abs = id
    signum = id

instance Real Bit where
    toRational (Bit False) = 0
    toRational (Bit True ) = 1

instance Integral Bit where
    quotRem _ (Bit False) = error "divide by zero"
    quotRem x (Bit True ) = (x, 0)

    divMod = quotRem
    toInteger (Bit False) = 0
    toInteger (Bit True ) = 1

instance Bits Bit where
    (.&.) = liftBool2 (&&)
    (.|.) = liftBool2 (||)
    xor = liftBool2 (/=)

    complement (Bit x) = Bit (not x)

    shift b 0 = b
    shift _ _ = 0

    rotate = const

    bit 0 = 1
    bit _ = 0

    setBit _ 0 = 1
    setBit b _ = b

    clearBit _ 0 = 0
    clearBit b _ = b

    complementBit b 0 = complement b
    complementBit b _ = b

    testBit b 0 = toBool b
    testBit _ _ = False

    bitSizeMaybe _ = Just 1
    bitSize _ = 1

    isSigned _ = False

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704

    popCount = fromEnum

#endif
