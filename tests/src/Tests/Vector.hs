module Tests.Vector where

import Support

import Data.Bit
import qualified Data.Vector.Unboxed.Bit as U
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

vectorTests = testGroup "Data.Vector.Unboxed.Bit"
    [ testProperty "toList . fromList == id"    prop_toList_fromList
    , testProperty "fromList . toList == id"    prop_fromList_toList
    , testProperty "indexWord"                  prop_indexWord_def
    , testProperty "slice"                      prop_slice_def
    , testProperty "reverse"                    prop_reverse_def
    ]

prop_toList_fromList xs
    =  U.toList (U.fromList xs :: U.Vector Bit)
    == xs

prop_fromList_toList xs
    =  U.fromList (U.toList xs)
    == (xs :: U.Vector Bit)

prop_indexWord_def n xs 
    = not (U.null xs)
    ==> readWordL  (U.toList xs) n'
     == U.indexWord xs           n'
    where
        n' = n `mod` U.length xs

prop_slice_def :: Int -> Int -> U.Vector Bit -> Bool
prop_slice_def s n xs
    =  sliceList s' n' (U.toList xs)
    == U.toList (U.slice s' n' xs)
    where
        (s', n') = trimSlice s n (U.length xs)

prop_reverse_def xs
    =   reverse  (U.toList xs)
    ==  U.toList (U.reverse xs)
