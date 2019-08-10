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
  [showReadTests, mvectorTests, TS.mvectorTests, setOpTests, vectorTests]

showReadTests :: TestTree
showReadTests =
  testGroup "Show/Read"
    $ map (uncurry testProperty)
    $ lawsProperties
    $ showReadLaws (Proxy :: Proxy Bit)
