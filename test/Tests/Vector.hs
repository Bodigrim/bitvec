module Tests.Vector where

import Support

import Data.Bit
import Data.Bits
import Data.List
import qualified Data.Vector.Unboxed as U hiding (reverse, and, or, any, all, findIndex)
import qualified Data.Vector.Unboxed.Bit as U
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@?=))
import Test.QuickCheck
import Test.QuickCheck.Function

vectorTests :: Test
vectorTests = testGroup "Data.Vector.Unboxed.Bit"
    [ testCase     "wordSize correct"           (U.wordSize @?= finiteBitSize (0 :: Word))
    , testGroup "Data.Vector.Unboxed functions"
        [ testProperty "toList . fromList == id"    prop_toList_fromList
        , testProperty "fromList . toList == id"    prop_fromList_toList
        , testProperty "slice"                      prop_slice_def
        ]
    , testProperty "wordLength"                 prop_wordLength_def
    , testProperty "fromWords"                  prop_fromWords_def
    , testProperty "toWords"                    prop_toWords_def
    , testProperty "indexWord"                  prop_indexWord_def
    , testProperty "zipWords"                   prop_zipWords_def
    , testProperty "reverse"                    prop_reverse_def
    , testProperty "countBits"                  prop_countBits_def
    , testProperty "listBits"                   prop_listBits_def
    , testGroup "Boolean operations"
        [ testProperty "and"                        prop_and_def
        , testProperty "or"                         prop_or_def
        ]
    , testGroup "Search operations"
        [ testProperty "any"                        prop_any_def
        , testProperty "all"                        prop_all_def
        , testProperty "anyBits"                    prop_anyBits_def
        , testProperty "allBits"                    prop_allBits_def
        , testProperty "first"                      prop_first_def
        , testProperty "findIndex"                  prop_findIndex_def
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

prop_wordLength_def :: U.Vector Bit -> Bool
prop_wordLength_def xs
    =  U.wordLength xs
    == U.length (U.toWords xs)

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

prop_zipWords_def :: Fun (Word, Word) Word -> U.Vector Bit -> U.Vector Bit -> Property
prop_zipWords_def f' xs ys
    =   U.zipWords f xs ys
    === U.take l (U.fromWords (U.zipWith f (U.toWords xs) (U.toWords ys)))
    where
        f = curry (apply f')
        l = U.length xs `min` U.length ys

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

prop_any_def :: Fun Bit Bool -> U.Vector Bit -> Bool
prop_any_def f' xs
    =  U.any f xs
    == any f (U.toList xs)
    where f = apply f'

prop_all_def :: Fun Bit Bool -> U.Vector Bit -> Bool
prop_all_def f' xs
    =  U.all f xs
    == all f (U.toList xs)
    where f = apply f'

prop_anyBits_def :: Bit -> U.Vector Bit -> Bool
prop_anyBits_def b xs
    =  U.anyBits b xs
    == U.any (b ==) xs

prop_allBits_def :: Bit -> U.Vector Bit -> Bool
prop_allBits_def b xs
    =  U.allBits b xs
    == U.all (b ==) xs

prop_first_def :: Bit -> U.Vector Bit -> Bool
prop_first_def b xs
    =  U.first b xs
    == findIndex (b ==) (U.toList xs)

prop_findIndex_def :: Fun Bit Bool -> U.Vector Bit -> Bool
prop_findIndex_def f' xs
    =  U.findIndex f xs
    == findIndex f (U.toList xs)
    where f = apply f'
