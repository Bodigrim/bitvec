{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

#ifndef BITVEC_THREADSAFE
module Tests.SetOps (setOpTests) where
#else
module Tests.SetOpsTS (setOpTests) where
#endif

import Support (twoTimesMore)

import Control.Monad
import Control.Monad.ST
import Data.Bit
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))

setOpTests :: TestTree
setOpTests = testGroup "Set operations"
  [ testProperty "generalize1"              prop_generalize1
  , testProperty "generalize2"              prop_generalize2
  , twoTimesMore
  $ testProperty "zipBits"                  prop_zipBits
  , testProperty "zipInPlace"               prop_zipInPlace

  , testProperty "mapBits"                  prop_mapBits
  , testProperty "mapInPlace"               prop_mapInPlace

  , testProperty "union"                    prop_union_def
  , testProperty "intersection"             prop_intersection_def
  , testProperty "difference"               prop_difference_def
  , testProperty "symDiff"                  prop_symDiff_def

  , mkGroup "invertBits" prop_invertBits

  , testProperty "invertInPlace"            prop_invertInPlace
  , testProperty "invertInPlaceWords"       prop_invertInPlaceWords
  , testProperty "invertInPlace middle"     prop_invertInPlace_middle
  , testProperty "invertInPlaceLong middle" prop_invertInPlaceLong_middle

  , mkGroup "reverseBits" prop_reverseBits

  , testProperty "reverseInPlace"            prop_reverseInPlace
  , testProperty "reverseInPlaceWords"       prop_reverseInPlaceWords
  , testProperty "reverseInPlace middle"     prop_reverseInPlace_middle
  , testProperty "reverseInPlaceLong middle" prop_reverseInPlaceLong_middle

  , mkGroup2 "selectBits"  prop_selectBits_def
  , mkGroup2 "excludeBits" prop_excludeBits_def

  , mkGroup "countBits" prop_countBits_def
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

mkGroup2 :: String -> (U.Vector Bit -> U.Vector Bit -> Property) -> TestTree
mkGroup2 name prop = testGroup name
  [ testProperty "simple" prop
  , testProperty "simple_long" (\(Large xs) (Large ys) -> prop xs ys)
  , testProperty "middle" propMiddle
  , testProperty "middle_long" propMiddleLong
  ]
  where
    f m = let n = fromIntegral m :: Double in
      odd (truncate (exp (abs (sin n) * 10)) :: Integer)
    propMiddle (NonNegative from1) (NonNegative len1) (NonNegative excess1) (NonNegative from2) (NonNegative len2) (NonNegative excess2) =
      prop (U.slice from1 len1 (U.generate (from1 + len1 + excess1) (Bit . f))) (U.slice from2 len2 (U.generate (from2 + len2 + excess2) (Bit . f)))
    propMiddleLong (NonNegative x1) (NonNegative y1) (NonNegative z1) (NonNegative x2) (NonNegative y2) (NonNegative z2) =
      propMiddle (NonNegative $ x1 * 31) (NonNegative $ y1 * 37) (NonNegative $ z1 * 29) (NonNegative $ x2 * 31) (NonNegative $ y2 * 37) (NonNegative $ z2 * 29)

prop_generalize1 :: Fun Bit Bit -> Bit -> Property
prop_generalize1 fun x =
  applyFun fun x === generalize1 (applyFun fun) x

prop_generalize2 :: Fun (Bit, Bit) Bit -> Bit -> Bit -> Property
prop_generalize2 fun x y =
  curry (applyFun fun) x y === generalize2 (curry (applyFun fun)) x y

prop_union_def :: U.Vector Bit -> U.Vector Bit -> Property
prop_union_def xs ys =
  xs .|. ys === U.zipWith (.|.) xs ys

prop_intersection_def :: U.Vector Bit -> U.Vector Bit -> Property
prop_intersection_def xs ys =
  xs .&. ys === U.zipWith (.&.) xs ys

prop_difference_def :: U.Vector Bit -> U.Vector Bit -> Property
prop_difference_def xs ys =
  zipBits diff xs ys === U.zipWith diff xs ys
  where
    diff x y = x .&. complement y

prop_symDiff_def :: U.Vector Bit -> U.Vector Bit -> Property
prop_symDiff_def xs ys =
  xs `xor` ys === U.zipWith xor xs ys

prop_zipBits :: Fun (Bit, Bit) Bit -> U.Vector Bit -> U.Vector Bit -> Property
prop_zipBits fun xs ys =
  U.zipWith f xs ys === zipBits (generalize2 f) xs ys
  where
    f = curry $ applyFun fun

prop_zipInPlace :: Fun (Bit, Bit) Bit -> U.Vector Bit -> U.Vector Bit -> Property
prop_zipInPlace fun xs ys =
  U.zipWith f xs ys === U.take (min (U.length xs) (U.length ys)) (U.modify (zipInPlace (generalize2 f) xs) ys)
  where
    f = curry $ applyFun fun

prop_mapBits :: Fun Bit Bit -> U.Vector Bit -> Property
prop_mapBits fun xs =
  U.map (applyFun fun) xs === mapBits (generalize1 (applyFun fun)) xs

prop_mapInPlace :: Fun Bit Bit -> U.Vector Bit -> Property
prop_mapInPlace fun xs =
  U.map (applyFun fun) xs === U.modify (mapInPlace (generalize1 (applyFun fun))) xs

prop_invertBits :: U.Vector Bit -> Property
prop_invertBits xs =
  U.map complement xs === complement xs

prop_invertInPlace :: U.Vector Bit -> Property
prop_invertInPlace xs =
  U.map complement xs === U.modify invertInPlace xs

prop_invertInPlaceWords :: Large (U.Vector Bit) -> Property
prop_invertInPlaceWords = prop_invertInPlace . getLarge

prop_invertInPlace_middle :: NonNegative Int -> NonNegative Int -> NonNegative Int -> Property
prop_invertInPlace_middle (NonNegative from) (NonNegative len) (NonNegative excess) = runST $ do
  let totalLen = from + len + excess
  vec <- MU.new totalLen
  forM_ [0 .. totalLen - 1] $ \i ->
    MU.write vec i (Bit (odd i))
  ref <- U.freeze vec

  let middle = MU.slice from len vec
  invertInPlace middle
  wec <- U.unsafeFreeze vec

  let refLeft   = U.take from ref
      wecLeft   = U.take from wec
      refRight  = U.drop (from + len) ref
      wecRight  = U.drop (from + len) wec
      refMiddle = U.map complement (U.take len (U.drop from ref))
      wecMiddle = U.take len (U.drop from wec)
  pure $ refLeft === wecLeft .&&. refRight === wecRight .&&. refMiddle === wecMiddle

prop_invertInPlaceLong_middle :: NonNegative Int -> NonNegative Int -> NonNegative Int -> Property
prop_invertInPlaceLong_middle (NonNegative x) (NonNegative y) (NonNegative z) =
  prop_invertInPlace_middle (NonNegative $ x * 31) (NonNegative $ y * 37) (NonNegative $ z * 29)

prop_reverseBits :: U.Vector Bit -> Property
prop_reverseBits xs =
  U.reverse xs === reverseBits xs

prop_reverseInPlace :: U.Vector Bit -> Property
prop_reverseInPlace xs =
  U.reverse xs === U.modify reverseInPlace xs

prop_reverseInPlaceWords :: Large (U.Vector Bit) -> Property
prop_reverseInPlaceWords = prop_reverseInPlace . getLarge

prop_reverseInPlace_middle :: NonNegative Int -> NonNegative Int -> NonNegative Int -> Property
prop_reverseInPlace_middle (NonNegative from) (NonNegative len) (NonNegative excess) = runST $ do
  let totalLen = from + len + excess
  vec <- MU.new totalLen
  forM_ [0 .. totalLen - 1] $ \i ->
    MU.write vec i (Bit (odd i))
  ref <- U.freeze vec

  let middle = MU.slice from len vec
  reverseInPlace middle
  wec <- U.unsafeFreeze vec

  let refLeft   = U.take from ref
      wecLeft   = U.take from wec
      refRight  = U.drop (from + len) ref
      wecRight  = U.drop (from + len) wec
      refMiddle = U.reverse (U.take len (U.drop from ref))
      wecMiddle = U.take len (U.drop from wec)
  pure $ refLeft === wecLeft .&&. refRight === wecRight .&&. refMiddle === wecMiddle

prop_reverseInPlaceLong_middle :: NonNegative Int -> NonNegative Int -> NonNegative Int -> Property
prop_reverseInPlaceLong_middle (NonNegative x) (NonNegative y) (NonNegative z) =
  prop_reverseInPlace_middle (NonNegative $ x * 31) (NonNegative $ y * 37) (NonNegative $ z * 29)

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

generalize1 :: (Bit -> Bit) -> (forall a. Bits a => a -> a)
generalize1 f = case (f (Bit False), f (Bit True)) of
  (Bit False, Bit False) -> const zeroBits
  (Bit False, Bit True)  -> id
  (Bit True,  Bit False) -> complement
  (Bit True,  Bit True)  -> const $ complement zeroBits

generalize2 :: (Bit -> Bit -> Bit) -> (forall a. Bits a => a -> a -> a)
generalize2 f = case (f (Bit False) (Bit False), f (Bit False) (Bit True), f (Bit True) (Bit False), f (Bit True) (Bit True)) of
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
