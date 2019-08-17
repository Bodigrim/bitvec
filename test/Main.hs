module Main where

import Data.Bit
import Data.Proxy
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck

import Tests.MVector (mvectorTests)
import qualified Tests.MVectorTS as TS (mvectorTests)
import Tests.SetOps (setOpTests)
import Tests.Vector (vectorTests)

main :: IO ()
main = defaultMain $ testGroup
  "All"
  [lawsTests, mvectorTests, TS.mvectorTests, setOpTests, vectorTests]

lawsTests :: TestTree
lawsTests = testGroup "Laws"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [ bitsLaws        (Proxy :: Proxy Bit)
  , eqLaws          (Proxy :: Proxy Bit)
  , ordLaws         (Proxy :: Proxy Bit)
  , boundedEnumLaws (Proxy :: Proxy Bit)
  , showLaws        (Proxy :: Proxy Bit)
  , showReadLaws    (Proxy :: Proxy Bit)
  , numLaws         (Proxy :: Proxy Bit)
  , integralLaws    (Proxy :: Proxy Bit)
  ]

