#!/usr/bin/env runhaskell
module Main where

import Test.Framework (defaultMain)

import Tests.Bit (bitTests)
import Tests.SetOps (setOpTests)
import Tests.MVector (mvectorTests)
import Tests.Vector (vectorTests)

main :: IO ()
main = defaultMain
    [ bitTests
    , mvectorTests
    , setOpTests
    , vectorTests
    ]
