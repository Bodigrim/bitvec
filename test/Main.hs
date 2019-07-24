#!/usr/bin/env runhaskell
module Main where

import Data.Bit
import Data.Proxy
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Classes
import Tests.MVector (mvectorTests)
import qualified Tests.MVectorTS as TS (mvectorTests)
import Tests.SetOps (setOpTests)
import Tests.Vector (vectorTests)

main :: IO ()
main = defaultMain
    [ showReadTests
    , mvectorTests
    , TS.mvectorTests
    , setOpTests
    , vectorTests
    ]

showReadTests :: Test
showReadTests
  = testGroup "Show/Read"
  $ map (uncurry testProperty)
  $ lawsProperties
  $ showReadLaws (Proxy :: Proxy Bit)
