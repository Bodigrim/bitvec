{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module Tests.F2Poly
  ( f2polyTests
  ) where

import Control.Exception
import Data.Bit
import Data.Bits
import Data.Ratio
import GHC.Exts
#ifdef MIN_VERSION_ghc_bignum
import GHC.Num.Integer
#else
import GHC.Integer.Logarithms
#endif
import Test.Tasty
import Test.Tasty.QuickCheck

#ifdef MIN_VERSION_quickcheck_classes_base
import Data.Proxy
import Test.QuickCheck.Classes.Base
#endif

import Support

f2polyTests :: TestTree
f2polyTests = testGroup "F2Poly"
  [ testProperty "Addition"            prop_f2polyAdd
  , testProperty "Multiplication"      prop_f2polyMul
  , testProperty "Square"              prop_f2polySqr
  , tenTimesLess
  $ testProperty "Multiplication long" prop_f2polyMulLong
  , testProperty "Multiplication 1"    prop_f2polyMul1
  , tenTimesLess
  $ testProperty "Square long"         prop_f2polySqrLong
  , testProperty "Remainder"           prop_f2polyRem
  , testProperty "GCD"                 prop_f2polyGCD
  , testProperty "Enum" $
    \n -> let x = toEnum n in toEnum (fromEnum x) === (x :: F2Poly)
#ifdef MIN_VERSION_quickcheck_classes_base
  , tenTimesLess $ lawsToTest $
    showLaws (Proxy :: Proxy F2Poly)
  , lawsToTest $
    numLaws (Proxy :: Proxy F2Poly)
  , lawsToTest $
    integralLaws (Proxy :: Proxy F2Poly)
#endif
  , testProperty "fromNegative" prop_f2polyFromNegative
  , testProperty "divideByZero" prop_f2polyDivideByZero
  , testProperty "toRational" prop_f2polyToRational
  , testProperty "signum" $ \x -> x + signum x === (x + 1 :: F2Poly)
  ]

prop_f2polyAdd :: F2Poly -> F2Poly -> Property
prop_f2polyAdd x y = x + y === fromInteger (toInteger x `xor` toInteger y)

prop_f2polyMul :: F2Poly -> F2Poly -> Property
prop_f2polyMul x y = x * y === fromInteger (toInteger x `binMul` toInteger y)

prop_f2polySqr :: F2Poly -> Property
prop_f2polySqr x = x * x === fromInteger (toInteger x `binMul` toInteger x)

prop_f2polyMulLong :: Large F2Poly -> Large F2Poly -> Property
prop_f2polyMulLong (Large x) (Large y) = prop_f2polyMul x y

prop_f2polyMul1 :: Property
prop_f2polyMul1 = prop_f2polyMul x y
  where
    x = fromInteger (1 `shiftL` 4358)
    y = fromInteger (1 `shiftL` 4932 + 1 `shiftL` 2116)

prop_f2polySqrLong :: Large F2Poly -> Property
prop_f2polySqrLong (Large x) = prop_f2polySqr x

prop_f2polyRem :: F2Poly -> F2Poly -> Property
prop_f2polyRem x y = y /= 0 ==> x `rem` y === fromInteger (toInteger x `binRem` toInteger y)

-- For polynomials @x@ and @y@, @gcdExt@ computes their unique greatest common
-- divisor @g@ and the unique coefficient polynomial @s@ satisfying @xs + yt = g@.
--
-- Thus it is sufficient to check @gcd == fst . gcdExt@ and @xs == g (mod y)@,
-- except if @y@ divides @x@, then @gcdExt x y@ is @(y, 0)@ and @xs `rem` y@ is zero,
-- so that it is then necessary to check @xs `rem` y == g `rem` y == 0@.
prop_f2polyGCD :: F2Poly -> F2Poly -> Property
prop_f2polyGCD x y = g === x `gcd` y .&&. (y /= 0 ==> (x * s) `rem` y === g `rem` y)
  where
    (g, s) = x `gcdExt` y

binMul :: Integer -> Integer -> Integer
binMul = go 0
  where
    go :: Integer -> Integer -> Integer -> Integer
    go acc _ 0 = acc
    go acc x y = go (if odd y then acc `xor` x else acc) (x `shiftL` 1) (y `shiftR` 1)

binRem :: Integer -> Integer -> Integer
binRem x y = go x
  where
#ifdef MIN_VERSION_ghc_bignum
    binLog n = I# (word2Int# (integerLog2# n))
#else
    binLog n = I# (integerLog2# n)
#endif
    ly = binLog y

    go 0 = 0
    go z = if lz < ly then z else go (z `xor` (y `shiftL` (lz - ly)))
      where
        lz = binLog z

prop_f2polyFromNegative :: Large Int -> Property
prop_f2polyFromNegative (Large m) =
  ioProperty ((=== Left Underflow) <$> try (evaluate (fromInteger neg :: F2Poly)))
  where
    neg = negate (1 + toInteger m * toInteger m)

prop_f2polyToRational :: F2Poly -> Property
prop_f2polyToRational x = denominator y === 1 .&&. fromInteger (numerator y) === x
  where
    y = toRational x

prop_f2polyDivideByZero :: F2Poly -> Property
prop_f2polyDivideByZero x =
  ioProperty ((=== Left DivideByZero) <$> try (evaluate (x `quot` 0)))
