module Tests.SetOps where

import Support ()

import Data.Bit
import Data.Bits
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Bit as U
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Property, (===))
import Test.Framework.Providers.QuickCheck2 (testProperty)

setOpTests :: Test
setOpTests = testGroup "Set operations"
    [ testProperty "union"          prop_union_def
    , testProperty "intersection"   prop_intersection_def
    , testProperty "difference"     prop_difference_def
    , testProperty "symDiff"        prop_symDiff_def

    , testProperty "unions"         prop_unions_def
    , testProperty "intersections"  prop_unions_def

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

prop_unions_def :: U.Vector Bit -> [U.Vector Bit] -> Property
prop_unions_def xs xss
    =   U.unions (xs :| xss)
    === foldr U.union xs xss

prop_intersections_def :: U.Vector Bit -> [U.Vector Bit] -> Property
prop_intersections_def xs xss
    =   U.intersections (xs :| xss)
    === foldr U.intersection xs xss

prop_invert_def :: U.Vector Bit -> Bool
prop_invert_def xs
    =  U.toList (U.invert xs)
    == map complement (U.toList xs)

prop_select_def :: U.Vector Bit -> U.Vector Word -> Bool
prop_select_def xs ys
    =  U.select xs ys
    == [ x | (Bit True, x) <- zip (U.toList xs) (U.toList ys)]

prop_exclude_def :: U.Vector Bit -> U.Vector Word -> Bool
prop_exclude_def xs ys
    =  U.exclude xs ys
    == [ x | (Bit False, x) <- zip (U.toList xs) (U.toList ys)]

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
