{-# LANGUAGE CPP #-}

module Main where

import Data.Bit
import Data.Proxy
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck

import Support
import Tests.MVector (mvectorTests)
import qualified Tests.MVectorTS as TS (mvectorTests)
import Tests.SetOps (setOpTests)
import Tests.Vector (vectorTests)

main :: IO ()
main = defaultMain $ testGroup
  "All"
  [lawsTests, f2polyTests, mvectorTests, TS.mvectorTests, setOpTests, vectorTests]

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
  [ tenTimesLess $ lawsToTest $
    showLaws (Proxy :: Proxy F2Poly)
#if MIN_VERSION_quickcheck_classes(0,6,3)
  , lawsToTest $
    numLaws (Proxy :: Proxy F2Poly)
#endif
  , lawsToTest $
    integralLaws (Proxy :: Proxy F2Poly)
  ]

