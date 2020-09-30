{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Data.Bit
import Data.Bits
import Data.Proxy
import qualified Data.Vector.Unboxed as U
import GHC.Exts
import GHC.Integer.Logarithms
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck

import Support
import Tests.Conc (concTests)
import Tests.MVector (mvectorTests)
import qualified Tests.MVectorTS as TS (mvectorTests)
import Tests.SetOps (setOpTests)
import qualified Tests.SetOpsTS as TS (setOpTests)
import Tests.Vector (vectorTests)

main :: IO ()
main = defaultMain $ testGroup "All"
  [ lawsTests
  , f2polyTests
  , mvectorTests
  , TS.mvectorTests
  , setOpTests
  , TS.setOpTests
  , vectorTests
  , concTests
  ]

lawsTests :: TestTree
lawsTests = adjustOption (const $ QuickCheckTests 100)
  $ testGroup "Bit"
  $ map lawsToTest
  [ bitsLaws        (Proxy :: Proxy Bit)
  , eqLaws          (Proxy :: Proxy Bit)
  , ordLaws         (Proxy :: Proxy Bit)
  , boundedEnumLaws (Proxy :: Proxy Bit)
  , showLaws        (Proxy :: Proxy Bit)
  , showReadLaws    (Proxy :: Proxy Bit)
#if MIN_VERSION_quickcheck_classes(0,6,3)
  , numLaws         (Proxy :: Proxy Bit)
#endif
  , integralLaws    (Proxy :: Proxy Bit)
  ]

f2polyTests :: TestTree
f2polyTests = testGroup "F2Poly"
  [ testProperty "Addition"       prop_f2polyAdd
  , testProperty "Multiplication" prop_f2polyMul
  , testProperty "Square" prop_f2polySqr
  , tenTimesLess $ testProperty "Multiplication long" prop_f2polyMulLong
  , tenTimesLess $ testProperty "Square long" prop_f2polySqrLong
  , testProperty "Remainder"      prop_f2polyRem
  , testProperty "GCD"            prop_f2polyGCD
  , tenTimesLess $ lawsToTest $
    showLaws (Proxy :: Proxy F2Poly)
#if MIN_VERSION_quickcheck_classes(0,6,3)
  , lawsToTest $
    numLaws (Proxy :: Proxy F2Poly)
#endif
  , lawsToTest $
    integralLaws (Proxy :: Proxy F2Poly)
  ]

prop_f2polyAdd :: F2Poly -> F2Poly -> Property
prop_f2polyAdd x y = x + y === fromInteger (toInteger x `xor` toInteger y)

prop_f2polyMul :: F2Poly -> F2Poly -> Property
prop_f2polyMul x y = x * y === fromInteger (toInteger x `binMul` toInteger y)

prop_f2polySqr :: F2Poly -> Property
prop_f2polySqr x = x * x === fromInteger (toInteger x `binMul` toInteger x)

prop_f2polyMulLong :: U.Vector Word -> U.Vector Word -> Property
prop_f2polyMulLong xs ys = x * y === fromInteger (toInteger x `binMul` toInteger y)
  where
    x = toF2Poly $ castFromWords xs
    y = toF2Poly $ castFromWords ys

prop_f2polySqrLong :: U.Vector Word -> Property
prop_f2polySqrLong xs = x * x === fromInteger (toInteger x `binMul` toInteger x)
  where
    x = toF2Poly $ castFromWords xs

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
    binLog n = I# (integerLog2# n)
    ly = binLog y

    go 0 = 0
    go z = if lz < ly then z else go (z `xor` (y `shiftL` (lz - ly)))
      where
        lz = binLog z
