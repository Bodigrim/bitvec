module Tests.Vector where

import Support

import Data.Bit
import Data.List
import qualified Data.Vector.Unboxed as U hiding (reverse, and, or, any, all, findIndex)
import qualified Data.Vector.Unboxed.Bit as U
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
    , testProperty "fromWords"                  prop_fromWords_def
    , testProperty "toWords"                    prop_toWords_def
    , testProperty "indexWord"                  prop_indexWord_def
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

prop_fromWords_def :: U.Vector Word -> Property
prop_fromWords_def ws
    =   U.toList (U.fromWords ws)
    === concatMap wordToBitList (U.toList ws)

prop_toWords_def :: U.Vector Bit -> Bool
prop_toWords_def xs
    =  U.toList (U.toWords xs)
    == loop (U.toList xs)
        where
            loop [] = []
            loop bs = case packBitsToWord bs of
                (w, bs') -> w : loop bs'

prop_indexWord_def :: Int -> U.Vector Bit -> Property
prop_indexWord_def n xs
    = not (U.null xs)
    ==> readWordL  (U.toList xs) n'
     == U.indexWord xs           n'
    where
        n' = n `mod` U.length xs

prop_reverse_def :: U.Vector Bit -> Bool
prop_reverse_def xs
    =   reverse  (U.toList xs)
    ==  U.toList (U.reverse xs)

prop_countBits_def :: U.Vector Bit -> Bool
prop_countBits_def xs
    =  U.countBits xs
    == length (filter unBit (U.toList xs))

prop_listBits_def :: U.Vector Bit -> Bool
prop_listBits_def xs
    =  U.listBits xs
    == [ i | (i,x) <- zip [0..] (U.toList xs), unBit x]

prop_and_def :: U.Vector Bit -> Bool
prop_and_def xs
    =  U.and xs
    == all unBit (U.toList xs)

prop_or_def :: U.Vector Bit -> Bool
prop_or_def xs
    =  U.or xs
    == any unBit (U.toList xs)

prop_first_def :: Bit -> U.Vector Bit -> Bool
prop_first_def b xs
    =  U.first b xs
    == findIndex (b ==) (U.toList xs)
