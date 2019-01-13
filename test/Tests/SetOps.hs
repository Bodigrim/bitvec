module Tests.SetOps where

import Support ()

import Data.Bit
import Data.Bits
import qualified Data.Vector.Unboxed.Bit as U
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

setOpTests :: Test
setOpTests = testGroup "Set operations"
    [ testProperty "union"          prop_union_def
    , testProperty "intersection"   prop_intersection_def
    , testProperty "difference"     prop_difference_def
    , testProperty "symDiff"        prop_symDiff_def

    , testProperty "unions"         (prop_unions_def 1000)
    , testProperty "intersections"  (prop_unions_def 1000)

    , testProperty "invert"         prop_invert_def

    , testProperty "select"         prop_select_def
    , testProperty "exclude"        prop_exclude_def

    , testProperty "selectBits"     prop_selectBits_def
    , testProperty "excludeBits"    prop_excludeBits_def

    , testProperty "countBits"      prop_countBits_def
    ]

prop_union_def :: U.Vector Bit -> U.Vector Bit -> Bool
prop_union_def xs ys
    =  U.toList (U.union xs ys)
    == zipWith (.|.) (U.toList xs) (U.toList ys)

prop_intersection_def :: U.Vector Bit -> U.Vector Bit -> Bool
prop_intersection_def xs ys
    =  U.toList (U.intersection xs ys)
    == zipWith (.&.) (U.toList xs) (U.toList ys)

prop_difference_def :: U.Vector Bit -> U.Vector Bit -> Bool
prop_difference_def xs ys
    =  U.toList (U.difference xs ys)
    == zipWith diff (U.toList xs) (U.toList ys)
    where
        diff x y = x .&. complement y

prop_symDiff_def :: U.Vector Bit -> U.Vector Bit -> Bool
prop_symDiff_def xs ys
    =  U.toList (U.symDiff xs ys)
    == zipWith xor (U.toList xs) (U.toList ys)

prop_unions_def :: Int -> Int -> [U.Vector Bit] -> Bool
prop_unions_def maxN n' xss
    =  U.unions n xss
    == U.take n (foldr U.union (U.replicate n 0) (map (U.pad n) xss))
    where n = n' `mod` maxN

prop_intersections_def :: Int -> Int -> [U.Vector Bit] -> Bool
prop_intersections_def maxN n' xss
    =  U.intersections n xss
    == U.take n (foldr U.intersection (U.replicate n 1) (map (U.padWith 1 n) xss))
    where n = n' `mod` maxN

prop_invert_def :: U.Vector Bit -> Bool
prop_invert_def xs
    =  U.toList (U.invert xs)
    == map complement (U.toList xs)

prop_select_def :: U.Vector Bit -> U.Vector Word -> Bool
prop_select_def xs ys
    =  U.select xs ys
    == [ x | (1, x) <- zip (U.toList xs) (U.toList ys)]

prop_exclude_def :: U.Vector Bit -> U.Vector Word -> Bool
prop_exclude_def xs ys
    =  U.exclude xs ys
    == [ x | (0, x) <- zip (U.toList xs) (U.toList ys)]

prop_selectBits_def :: U.Vector Bit -> U.Vector Bit -> Bool
prop_selectBits_def xs ys
    =  U.selectBits xs ys
    == U.fromList (U.select xs ys)

prop_excludeBits_def :: U.Vector Bit -> U.Vector Bit -> Bool
prop_excludeBits_def xs ys
    =  U.excludeBits xs ys
    == U.fromList (U.exclude xs ys)

prop_countBits_def :: U.Vector Bit -> Bool
prop_countBits_def xs
    =  U.countBits xs
    == U.length (U.selectBits xs xs)
