{-# LANGUAGE CPP #-}

module Tests.Vector
  ( vectorTests
  ) where

import Support

import Prelude hiding (and, or)
import Control.Exception
import Data.Bit
import Data.Bits
import Data.List (findIndex)
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Base as UB
import Data.Word
import Test.Tasty
import Test.Tasty.QuickCheck (Property, NonNegative(..), Positive(..), testProperty, Large(..), (===), property, once, (==>), ioProperty, (.&&.), counterexample)

#include "MachDeps.h"

vectorTests :: TestTree
vectorTests = testGroup "Data.Vector.Unboxed.Bit"
  [ testGroup "Data.Vector.Unboxed functions"
    [ testProperty "toList . fromList == id" prop_toList_fromList
    , mkGroup      "fromList . toList == id" prop_fromList_toList
    , testProperty "slice"                   prop_slice_def
    ]
  , tenTimesLess $
    testProperty "cloneFromWords" prop_cloneFromWords_def
  , mkGroup      "cloneToWords"   prop_cloneToWords_def
  , tenTimesLess $
    testProperty "castToWords_1"   prop_castToWords_1
  , tenTimesLess $
    testProperty "castToWords_2"   prop_castToWords_2
  , tenTimesLess $
    testProperty "cloneFromWords8" prop_cloneFromWords8_def
  , mkGroup      "cloneToWords8"   prop_cloneToWords8_def
  , tenTimesLess $
    testProperty "castToWords8_1"  prop_castToWords8_1
  , tenTimesLess $
    testProperty "castToWords8_2"  prop_castToWords8_2
  , testProperty "cloneToByteString" prop_cloneToByteString
  , mkGroup "reverse"        prop_reverse_def
  , testGroup "countBits"
    [ testProperty "special case 1" case_countBits_1
    , mkGroup "matches definition"  prop_countBits_def
    ]
  , testGroup "listBits"
    [ testProperty "special case 1" case_listBits_1
    , testProperty "special case 2" case_listBits_2
    , mkGroup "matches definition"  prop_listBits_def
    ]
  , mkGroup "and"            prop_and_def
  , mkGroup "or"             prop_or_def
  , testGroup "bitIndex"
    [ testProperty "special case 1" case_bitIndex_1
    , testProperty "special case 2" case_bitIndex_2
    , testProperty "special case 3" case_bitIndex_3
    , testProperty "special case 4" case_bitIndex_4
    , testProperty "special case 5" case_bitIndex_5
    , testProperty "special case 6" case_bitIndex_6
    , testProperty "special case 7" case_bitIndex_7
    , mkGroup "True"               (prop_bitIndex_1 (Bit True))
    , mkGroup "False"              (prop_bitIndex_1 (Bit False))
    ]
  , testGroup "nthBitIndex"
    [ testProperty "special case 1"                     case_nthBit_1
    , testProperty "special case 2"                     case_nthBit_2
    , testProperty "special case 3"                     case_nthBit_3
    , testProperty "special case 4"                     case_nthBit_4
    , testProperty "special case 5"                     case_nthBit_5
    , testProperty "special case 6"                     case_nthBit_6
    , testProperty "special case 7"                     case_nthBit_7
    , mkGroup      "matches bitIndex True"              prop_nthBit_1
    , mkGroup      "matches bitIndex False"             prop_nthBit_2
    , testProperty "matches sequence of bitIndex True"  prop_nthBit_3
    , testProperty "matches sequence of bitIndex False" prop_nthBit_4
    , testProperty "matches countBits"                  prop_nthBit_5
    , testProperty "negative argument"                  prop_nthBit_6
    ]
  , testGroup "Bits instance"
    [ testProperty "rotate is reversible" prop_rotate
    , testProperty "bit"                  prop_bit
    , testProperty "shiftL"               prop_shiftL
    , testProperty "shiftR"               prop_shiftR
    , testProperty "zeroBits"             prop_zeroBits
    , testProperty "bitSize"              prop_bitSize
    , testProperty "isSigned"             prop_isSigned
    , testProperty "setBit"               prop_setBit
    , testProperty "clearBit"             prop_clearBit
    , testProperty "complementBit"        prop_complementBit
    ]
  ]

mkGroup :: String -> (U.Vector Bit -> Property) -> TestTree
mkGroup name prop = testGroup name
  [ testProperty "simple" prop
  , testProperty "simple_long" (prop . getLarge)
  , testProperty "middle" propMiddle
  , testProperty "middle_long" propMiddleLong
  ]
  where
    f m = let n = fromIntegral m :: Double in
      odd (truncate (exp (abs (sin n) * 10)) :: Integer)
    propMiddle (NonNegative from) (NonNegative len) (NonNegative excess) =
      prop (U.slice from len (U.generate (from + len + excess) (Bit . f)))
    propMiddleLong (NonNegative x) (NonNegative y) (NonNegative z) =
      propMiddle (NonNegative $ x * 31) (NonNegative $ y * 37) (NonNegative $ z * 29)

prop_toList_fromList :: [Bit] -> Property
prop_toList_fromList xs = U.toList (U.fromList xs) === xs

prop_fromList_toList :: U.Vector Bit -> Property
prop_fromList_toList xs = U.fromList (U.toList xs) === xs

prop_slice_def :: Int -> Int -> U.Vector Bit -> Property
prop_slice_def s n xs =
  sliceList s' n' (U.toList xs) === U.toList (U.slice s' n' xs)
  where
    (s', n') = trimSlice s n (U.length xs)

prop_cloneFromWords_def :: U.Vector Word -> Property
prop_cloneFromWords_def ws =
  U.toList (castFromWords ws) === concatMap wordToBitList (U.toList ws)

prop_cloneToWords_def :: U.Vector Bit -> Property
prop_cloneToWords_def xs = U.toList (cloneToWords xs) === loop (U.toList xs)
 where
  loop [] = []
  loop bs = case packBitsToWord bs of
    (w, bs') -> w : loop bs'

prop_castToWords_1 :: U.Vector Word -> Property
prop_castToWords_1 ws =
  Just ws === castToWords (castFromWords ws)

prop_castToWords_2 :: U.Vector Bit -> Property
prop_castToWords_2 xs = case castToWords xs of
  Nothing -> property True
  Just ws -> castFromWords ws === xs

prop_cloneFromWords8_def :: U.Vector Word8 -> Property
prop_cloneFromWords8_def ws
  = counterexample ("offset = " ++ show off ++ " len = " ++ show len)
  $ U.toList (castFromWords8 ws) === concatMap wordToBitList (U.toList ws)
  where
    UB.V_Word8 (P.Vector off len _) = ws

prop_cloneToWords8_def :: U.Vector Bit -> Property
prop_cloneToWords8_def xs@(BitVec off len _)
  = counterexample ("offset = " ++ show off ++ " len = " ++ show len)
  $ U.toList (cloneToWords8 xs) === loop (U.toList xs)
  where
    loop [] = []
    loop bs = case packBitsToWord bs of
      (w, bs') -> w : loop bs'

prop_castToWords8_1 :: U.Vector Word8 -> Property
#ifdef WORDS_BIGENDIAN
prop_castToWords8_1 ws = Nothing === castToWords8 (castFromWords8 ws)
#else
prop_castToWords8_1 ws
  = counterexample ("offset = " ++ show off ++ " len = " ++ show len)
  $ Just ws === castToWords8 (castFromWords8 ws)
  where
    UB.V_Word8 (P.Vector off len _) = ws
#endif

prop_castToWords8_2 :: U.Vector Bit -> Property
prop_castToWords8_2 xs = case castToWords8 xs of
  Nothing -> property True
  Just ws -> castFromWords8 ws === xs

prop_reverse_def :: U.Vector Bit -> Property
prop_reverse_def xs =
  reverse (U.toList xs) === U.toList (U.modify reverseInPlace xs)

prop_countBits_def :: U.Vector Bit -> Property
prop_countBits_def xs = countBits xs === length (filter unBit (U.toList xs))

case_countBits_1 :: Property
case_countBits_1 = once $
  countBits (U.drop 64 (U.replicate 128 (Bit False))) === 0

prop_listBits_def :: U.Vector Bit -> Property
prop_listBits_def xs =
  listBits xs === [ i | (i, x) <- zip [0 ..] (U.toList xs), unBit x ]

case_listBits_1 :: Property
case_listBits_1 = once $
  listBits (U.drop 24 (U.replicate 64 (Bit False))) === []

case_listBits_2 :: Property
case_listBits_2 = once $
  listBits (U.drop 24 (U.replicate 128 (Bit True))) === [0..103]

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

case_bitIndex_1 :: Property
case_bitIndex_1 = once $
  bitIndex (Bit True) (U.generate 128 (Bit . (== 64))) === Just 64

case_bitIndex_2 :: Property
case_bitIndex_2 = once $
  bitIndex (Bit False) (U.generate 128 (Bit . (/= 64))) === Just 64

case_bitIndex_3 :: Property
case_bitIndex_3 = once $
  bitIndex (Bit True) (U.drop 63 (U.generate 128 (Bit . (== 64)))) === Just 1

case_bitIndex_4 :: Property
case_bitIndex_4 = once $
  bitIndex (Bit False) (U.drop 63 (U.generate 128 (Bit . (/= 64)))) === Just 1

case_bitIndex_5 :: Property
case_bitIndex_5 = once $
  bitIndex (Bit False) (U.drop 63 (U.replicate 65 (Bit True))) === Nothing

case_bitIndex_6 :: Property
case_bitIndex_6 = once $
  bitIndex (Bit False) (U.drop 63 (U.generate 66 (Bit . (== 63)))) === Just 1

case_bitIndex_7 :: Property
case_bitIndex_7 = once $
  bitIndex (Bit False) (U.drop 1023 (U.generate 1097 (Bit . (/= 1086)))) === Just 63

prop_bitIndex_1 :: Bit -> U.Vector Bit -> Property
prop_bitIndex_1 b xs = bitIndex b xs === findIndex (b ==) (U.toList xs)

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

prop_nthBit_6 :: NonNegative Int -> U.Vector Bit -> Property
prop_nthBit_6 (NonNegative n) xs = ioProperty $ do
  ret <- try (evaluate (nthBitIndex (Bit True) (-n) xs))
  pure $ property $ case ret of
    Left ErrorCallWithLocation{} -> True
    _ -> False

case_nthBit_1 :: Property
case_nthBit_1 = once $
  nthBitIndex (Bit True) 1 (U.slice 61 4 (U.replicate 100 (Bit False))) === Nothing

case_nthBit_2 :: Property
case_nthBit_2 = once $
  nthBitIndex (Bit False) 1 (U.slice 61 4 (U.replicate 100 (Bit True))) === Nothing

case_nthBit_3 :: Property
case_nthBit_3 = once $
  nthBitIndex (Bit True) 1 (U.drop 63 (U.generate 128 (Bit . (== 64)))) === Just 1

case_nthBit_4 :: Property
case_nthBit_4 = once $
  nthBitIndex (Bit False) 1 (U.drop 63 (U.generate 128 (Bit . (/= 64)))) === Just 1

case_nthBit_5 :: Property
case_nthBit_5 = once $
  nthBitIndex (Bit False) 1 (U.drop 63 (U.replicate 65 (Bit True))) === Nothing

case_nthBit_6 :: Property
case_nthBit_6 = once $
  nthBitIndex (Bit False) 1 (U.drop 63 (U.generate 66 (Bit . (== 63)))) === Just 1

case_nthBit_7 :: Property
case_nthBit_7 = once $
  nthBitIndex (Bit False) 1 (U.drop 1023 (U.generate 1097 (Bit . (/= 1086)))) === Just 63

prop_rotate :: Int -> U.Vector Bit -> Property
prop_rotate n v = v === (v `rotate` n) `rotate` (-n)

prop_bit :: Int -> Property
prop_bit n
  | n >= 0
  = testBit v n .&&. popCount v === 1 .&&. U.length v === n + 1
  | otherwise
  = not (testBit v n) .&&. popCount v === 0 .&&. U.length v === 0
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

prop_zeroBits :: Property
prop_zeroBits = once $
  U.length (zeroBits :: U.Vector Bit) === 0

prop_bitSize :: U.Vector Bit -> Property
prop_bitSize v = bitSizeMaybe v === Nothing

prop_isSigned :: U.Vector Bit -> Property
prop_isSigned v = isSigned v === False

prop_setBit :: Int -> U.Vector Bit -> Property
prop_setBit n v = v `setBit` n === U.imap ((.|.) . Bit . (== n)) v

prop_clearBit :: Int -> U.Vector Bit -> Property
prop_clearBit n v = v `clearBit` n === U.imap ((.&.) . Bit . (/= n)) v

prop_complementBit :: Int -> U.Vector Bit -> Property
prop_complementBit n v = v `complementBit` n === U.imap (xor . Bit . (== n)) v

prop_cloneToByteString :: U.Vector Bit -> Property
prop_cloneToByteString v@(BitVec off len _)
  = counterexample ("offset = " ++ show off ++ " len = " ++ show len)
  $ cloneToByteString (cloneFromByteString bs) === bs
  where
    bs = cloneToByteString v
