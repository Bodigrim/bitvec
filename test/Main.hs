{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Data.Bit
import Data.Proxy
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck

import Support
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
