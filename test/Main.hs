{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Control.Exception
import Data.Bit
import Test.Tasty
import Test.Tasty.QuickCheck

#ifdef MIN_VERSION_quickcheck_classes_base
import Data.Proxy
import Test.QuickCheck.Classes.Base
import Support
#endif

import Tests.Conc (concTests)
import Tests.F2Poly (f2polyTests)
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
#ifdef MIN_VERSION_quickcheck_classes_base
  $ map lawsToTest
  [ bitsLaws        (Proxy :: Proxy Bit)
  , eqLaws          (Proxy :: Proxy Bit)
  , ordLaws         (Proxy :: Proxy Bit)
  , boundedEnumLaws (Proxy :: Proxy Bit)
  , showLaws        (Proxy :: Proxy Bit)
  , showReadLaws    (Proxy :: Proxy Bit)
  , numLaws         (Proxy :: Proxy Bit)
  , integralLaws    (Proxy :: Proxy Bit)
  ] ++
#endif
  [ testProperty "divideByZero" prop_bitDivideByZero
  , testProperty "toRational"   prop_bitToRational
  ]

prop_bitToRational :: Bit -> Property
prop_bitToRational x = fromRational (toRational x) === x

prop_bitDivideByZero :: Bit -> Property
prop_bitDivideByZero x =
  ioProperty ((=== Left DivideByZero) <$> try (evaluate (x / 0)))
