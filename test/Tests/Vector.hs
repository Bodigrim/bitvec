module Tests.Vector where

import Support

import Prelude hiding (and, or)
import Data.Bit
import Data.Bits
import Data.List hiding (and, or)
import qualified Data.Vector.Unboxed as U hiding (reverse, and, or, any, all, findIndex)
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

vectorTests :: TestTree
vectorTests = testGroup "Data.Vector.Unboxed.Bit"
  [ testGroup "Data.Vector.Unboxed functions"
    [ testProperty "toList . fromList == id" prop_toList_fromList
    , testProperty "fromList . toList == id" prop_fromList_toList
    , testProperty "slice"                   prop_slice_def
    ]
  , tenTimesLess $
    testProperty "cloneFromWords" prop_cloneFromWords_def
  , testProperty "cloneToWords"   prop_cloneToWords_def
  , tenTimesLess $
    testProperty "cloneFromWords8" prop_cloneFromWords8_def
  , testProperty "cloneToWords8"   prop_cloneToWords8_def
  , testProperty "reverse"        prop_reverse_def
  , testProperty "countBits"      prop_countBits_def
  , testProperty "listBits"       prop_listBits_def
  , testGroup "Boolean operations"
    [ testProperty "and" prop_and_def
    , testProperty "or" prop_or_def
    ]
  , testGroup "Search operations"
    [ testProperty "first" prop_first_def
    ]
  , testGroup "nthBitIndex"
    [ testCase "special case 1" case_nthBit_1
    , testProperty "matches bitIndex True"              prop_nthBit_1
    , testProperty "matches bitIndex False"             prop_nthBit_2
    , testProperty "matches sequence of bitIndex True"  prop_nthBit_3
    , testProperty "matches sequence of bitIndex False" prop_nthBit_4
    , testProperty "matches countBits"                  prop_nthBit_5
    ]
  , testGroup "Bits instance"
    [ testProperty "rotate is reversible" prop_rotate
    , testProperty "bit"                  prop_bit
    , testProperty "shiftL"               prop_shiftL
    , testProperty "shiftR"               prop_shiftR
    ]
  ]

prop_toList_fromList :: [Bit] -> Bool
prop_toList_fromList xs = U.toList (U.fromList xs) == xs

prop_fromList_toList :: U.Vector Bit -> Bool
prop_fromList_toList xs = U.fromList (U.toList xs) == xs

prop_slice_def :: Int -> Int -> U.Vector Bit -> Bool
prop_slice_def s n xs = sliceList s' n' (U.toList xs)
  == U.toList (U.slice s' n' xs)
  where (s', n') = trimSlice s n (U.length xs)

prop_cloneFromWords_def :: U.Vector Word -> Property
prop_cloneFromWords_def ws =
  U.toList (castFromWords ws) === concatMap wordToBitList (U.toList ws)

prop_cloneToWords_def :: U.Vector Bit -> Bool
prop_cloneToWords_def xs = U.toList (cloneToWords xs) == loop (U.toList xs)
 where
  loop [] = []
  loop bs = case packBitsToWord bs of
    (w, bs') -> w : loop bs'

prop_cloneFromWords8_def :: U.Vector Word8 -> Property
prop_cloneFromWords8_def ws =
  U.toList (castFromWords8 ws) === concatMap wordToBitList (U.toList ws)

prop_cloneToWords8_def :: U.Vector Bit -> Bool
prop_cloneToWords8_def xs = U.toList (cloneToWords8 xs) == loop (U.toList xs)
 where
  loop [] = []
  loop bs = case packBitsToWord bs of
    (w, bs') -> w : loop bs'

prop_reverse_def :: U.Vector Bit -> Bool
prop_reverse_def xs =
  reverse (U.toList xs) == U.toList (U.modify reverseInPlace xs)

prop_countBits_def :: U.Vector Bit -> Bool
prop_countBits_def xs = countBits xs == length (filter unBit (U.toList xs))

prop_listBits_def :: U.Vector Bit -> Property
prop_listBits_def xs =
  listBits xs === [ i | (i, x) <- zip [0 ..] (U.toList xs), unBit x ]

and :: U.Vector Bit -> Bool
and xs = case bitIndex (Bit False) xs of
  Nothing -> True
  Just{}  -> False

prop_and_def :: U.Vector Bit -> Property
prop_and_def xs = and xs === all unBit (U.toList xs)

or :: U.Vector Bit -> Bool
or xs = case bitIndex (Bit True) xs of
  Nothing -> False
  Just{}  -> True

prop_or_def :: U.Vector Bit -> Property
prop_or_def xs = or xs === any unBit (U.toList xs)

prop_first_def :: Bit -> U.Vector Bit -> Bool
prop_first_def b xs = bitIndex b xs == findIndex (b ==) (U.toList xs)

prop_nthBit_1 :: U.Vector Bit -> Property
prop_nthBit_1 xs = bitIndex (Bit True) xs === nthBitIndex (Bit True) 1 xs

prop_nthBit_2 :: U.Vector Bit -> Property
prop_nthBit_2 xs = bitIndex (Bit False) xs === nthBitIndex (Bit False) 1 xs

prop_nthBit_3 :: Positive Int -> U.Vector Bit -> Property
prop_nthBit_3 (Positive n) xs = case nthBitIndex (Bit True) (n + 1) xs of
  Nothing -> property True
  Just i  -> case bitIndex (Bit True) xs of
    Nothing -> property False
    Just j  -> case nthBitIndex (Bit True) n (U.drop (j + 1) xs) of
      Nothing -> property False
      Just k  -> i === j + k + 1

prop_nthBit_4 :: Positive Int -> U.Vector Bit -> Property
prop_nthBit_4 (Positive n) xs = case nthBitIndex (Bit False) (n + 1) xs of
  Nothing -> property True
  Just i  -> case bitIndex (Bit False) xs of
    Nothing -> property False
    Just j  -> case nthBitIndex (Bit False) n (U.drop (j + 1) xs) of
      Nothing -> property False
      Just k  -> i === j + k + 1

prop_nthBit_5 :: Positive Int -> U.Vector Bit -> Property
prop_nthBit_5 (Positive n) xs = count > 0 ==>
  case nthBitIndex (Bit True) n' xs of
    Nothing -> property False
    Just i  -> countBits (U.take (i + 1) xs) === n'
  where
    count = countBits xs
    n' = n `mod` count + 1

case_nthBit_1 :: IO ()
case_nthBit_1 =
  assertEqual "should be equal" Nothing
    $ nthBitIndex (Bit True) 1
    $ U.slice 61 4
    $ U.replicate 100 (Bit False)

prop_rotate :: Int -> U.Vector Bit -> Property
prop_rotate n v = v === (v `rotate` n) `rotate` (-n)

prop_bit :: NonNegative Int -> Property
prop_bit (NonNegative n) = testBit v n .&&. popCount v === 1 .&&. U.length v == n + 1
  where
    v :: U.Vector Bit
    v = bit n

prop_shiftL :: NonNegative Int -> U.Vector Bit -> Property
prop_shiftL (NonNegative n) v = v === u
  where
    u = (v `shiftL` n) `shiftR` n

prop_shiftR :: NonNegative Int -> U.Vector Bit -> Property
prop_shiftR (NonNegative n) v = U.drop n v === U.drop n u .&&. popCount (U.take n u) === 0
  where
    u = (v `shiftR` n) `shiftL` n
