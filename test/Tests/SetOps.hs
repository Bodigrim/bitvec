{-# LANGUAGE RankNTypes #-}

module Tests.SetOps where

import Support ()

import Data.Bit
import Data.Bits
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))

setOpTests :: TestTree
setOpTests = testGroup
  "Set operations"
  [ testProperty "generalize"      prop_generalize
  , testProperty "zipBits"         prop_zipBits
  , testProperty "zipInPlace"      prop_zipInPlace
  , testProperty "invertBits"      prop_invertBits
  , testProperty "invertBitsWords" prop_invertBitsWords
  , testProperty "invertInPlace"   prop_invertInPlace
  , testProperty "reverseBits"     prop_reverseBits
  , testProperty "reverseInPlace"  prop_reverseInPlace
  , testProperty "selectBits"      prop_selectBits_def
  , testProperty "excludeBits"     prop_excludeBits_def
  , testProperty "countBits"       prop_countBits_def
  ]

prop_generalize :: Fun (Bit, Bit) Bit -> Bit -> Bit -> Property
prop_generalize fun x y = curry (applyFun fun) x y === generalize (curry (applyFun fun)) x y

prop_union_def :: U.Vector Bit -> U.Vector Bit -> Property
prop_union_def xs ys =
  zipBits (.|.) xs ys === U.zipWith (.|.) xs ys

prop_intersection_def :: U.Vector Bit -> U.Vector Bit -> Property
prop_intersection_def xs ys =
  zipBits (.&.) xs ys === U.zipWith (.&.) xs ys

prop_difference_def :: U.Vector Bit -> U.Vector Bit -> Property
prop_difference_def xs ys =
  zipBits diff xs ys === U.zipWith diff xs ys
  where
    diff x y = x .&. complement y

prop_symDiff_def :: U.Vector Bit -> U.Vector Bit -> Property
prop_symDiff_def xs ys =
  zipBits xor xs ys === U.zipWith xor xs ys

prop_zipBits :: Fun (Bit, Bit) Bit -> U.Vector Bit -> U.Vector Bit -> Property
prop_zipBits fun xs ys =
  U.zipWith f xs ys === zipBits (generalize f) xs ys
  where
    f = curry $ applyFun fun

prop_zipInPlace :: Fun (Bit, Bit) Bit -> U.Vector Bit -> U.Vector Bit -> Property
prop_zipInPlace fun xs ys =
  U.zipWith f xs ys === U.take (min (U.length xs) (U.length ys)) (U.modify (zipInPlace (generalize f) xs) ys)
  where
    f = curry $ applyFun fun

prop_invertBits :: U.Vector Bit -> Property
prop_invertBits xs =
  U.map complement xs === invertBits xs

prop_invertBitsWords :: U.Vector Word -> Property
prop_invertBitsWords ws =
  U.map complement xs === invertBits xs
  where
    xs = castFromWords ws

prop_invertInPlace :: U.Vector Bit -> Property
prop_invertInPlace xs =
  U.map complement xs === U.modify invertInPlace xs

prop_reverseBits :: U.Vector Bit -> Property
prop_reverseBits xs =
  U.reverse xs === reverseBits xs

prop_reverseInPlace :: U.Vector Bit -> Property
prop_reverseInPlace xs =
  U.reverse xs === U.modify reverseInPlace xs

select :: U.Unbox a => U.Vector Bit -> U.Vector a -> U.Vector a
select mask ws = U.map snd (U.filter (unBit . fst) (U.zip mask ws))

exclude :: U.Unbox a => U.Vector Bit -> U.Vector a -> U.Vector a
exclude mask ws = U.map snd (U.filter (not . unBit . fst) (U.zip mask ws))

prop_selectBits_def :: U.Vector Bit -> U.Vector Bit -> Property
prop_selectBits_def xs ys = selectBits xs ys === select xs ys

prop_excludeBits_def :: U.Vector Bit -> U.Vector Bit -> Property
prop_excludeBits_def xs ys = excludeBits xs ys === exclude xs ys

prop_countBits_def :: U.Vector Bit -> Property
prop_countBits_def xs = countBits xs === U.length (selectBits xs xs)

-------------------------------------------------------------------------------

generalize :: (Bit -> Bit -> Bit) -> (forall a. Bits a => a -> a -> a)
generalize f = case (f (Bit False) (Bit False), f (Bit False) (Bit True), f (Bit True) (Bit False), f (Bit True) (Bit True)) of
  (Bit False, Bit False, Bit False, Bit False) -> \_ _ -> zeroBits
  (Bit False, Bit False, Bit False, Bit True)  -> \x y -> x .&. y
  (Bit False, Bit False, Bit True,  Bit False) -> \x y -> x .&. complement y
  (Bit False, Bit False, Bit True,  Bit True)  -> \x _ -> x

  (Bit False, Bit True,  Bit False, Bit False) -> \x y -> complement x .&. y
  (Bit False, Bit True,  Bit False, Bit True)  -> \_ y -> y
  (Bit False, Bit True,  Bit True,  Bit False) -> \x y -> x `xor` y
  (Bit False, Bit True,  Bit True,  Bit True)  -> \x y -> x .|. y

  (Bit True,  Bit False, Bit False, Bit False) -> \x y -> complement (x .|. y)
  (Bit True,  Bit False, Bit False, Bit True)  -> \x y -> complement (x `xor` y)
  (Bit True,  Bit False, Bit True,  Bit False) -> \_ y -> complement y
  (Bit True,  Bit False, Bit True,  Bit True)  -> \x y -> x .|. complement y

  (Bit True,  Bit True,  Bit False, Bit False) -> \x _ -> complement x
  (Bit True,  Bit True,  Bit False, Bit True)  -> \x y -> complement x .|. y
  (Bit True,  Bit True,  Bit True,  Bit False) -> \x y -> complement (x .&. y)
  (Bit True,  Bit True,  Bit True,  Bit True)  -> \_ _ -> complement zeroBits
