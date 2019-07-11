module Tests.Vector where

import Support

import Prelude hiding (and, or)
import Data.Bit
import Data.List hiding (and, or)
import qualified Data.Vector.Unboxed as U hiding (reverse, and, or, any, all, findIndex)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

vectorTests :: Test
vectorTests = testGroup "Data.Vector.Unboxed.Bit"
    [ testGroup "Data.Vector.Unboxed functions"
        [ testProperty "toList . fromList == id"    prop_toList_fromList
        , testProperty "fromList . toList == id"    prop_fromList_toList
        , testProperty "slice"                      prop_slice_def
        ]
    , testProperty "cloneFromWords"             prop_cloneFromWords_def
    , testProperty "cloneToWords"               prop_cloneToWords_def
    , testProperty "reverse"                    prop_reverse_def
    , testProperty "countBits"                  prop_countBits_def
    , testProperty "listBits"                   prop_listBits_def
    , testGroup "Boolean operations"
        [ testProperty "and"                        prop_and_def
        , testProperty "or"                         prop_or_def
        ]
    , testGroup "Search operations"
        [ testProperty "first"                      prop_first_def
        ]
    ]

prop_toList_fromList :: [Bit] -> Bool
prop_toList_fromList xs =
    U.toList (U.fromList xs) == xs

prop_fromList_toList :: U.Vector Bit -> Bool
prop_fromList_toList xs =
    U.fromList (U.toList xs) == xs

prop_slice_def :: Int -> Int -> U.Vector Bit -> Bool
prop_slice_def s n xs
    =  sliceList s' n' (U.toList xs)
    == U.toList (U.slice s' n' xs)
    where
        (s', n') = trimSlice s n (U.length xs)

prop_cloneFromWords_def :: U.Vector Word -> Property
prop_cloneFromWords_def ws
    =   U.toList (castFromWords ws)
    === concatMap wordToBitList (U.toList ws)

prop_cloneToWords_def :: U.Vector Bit -> Bool
prop_cloneToWords_def xs
    =  U.toList (cloneToWords xs)
    == loop (U.toList xs)
        where
            loop [] = []
            loop bs = case packBitsToWord bs of
                (w, bs') -> w : loop bs'

prop_reverse_def :: U.Vector Bit -> Bool
prop_reverse_def xs
    =   reverse  (U.toList xs)
    ==  U.toList (U.modify reverseInPlace xs)

prop_countBits_def :: U.Vector Bit -> Bool
prop_countBits_def xs
    =  countBits xs
    == length (filter unBit (U.toList xs))

prop_listBits_def :: U.Vector Bit -> Property
prop_listBits_def xs
    =  listBits xs
    === [ i | (i,x) <- zip [0..] (U.toList xs), unBit x]

and :: U.Vector Bit -> Bool
and xs = case bitIndex (Bit False) xs of
    Nothing -> True
    Just{}  -> False

prop_and_def :: U.Vector Bit -> Bool
prop_and_def xs
    =  and xs
    == all unBit (U.toList xs)

or :: U.Vector Bit -> Bool
or xs = case bitIndex (Bit True) xs of
    Nothing -> False
    Just{}  -> True

prop_or_def :: U.Vector Bit -> Bool
prop_or_def xs
    =  or xs
    == any unBit (U.toList xs)

prop_first_def :: Bit -> U.Vector Bit -> Bool
prop_first_def b xs
    =  bitIndex b xs
    == findIndex (b ==) (U.toList xs)
