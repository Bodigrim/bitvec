module Tests.SetOps where

import Support ()

import Data.Bit
import Data.Bits
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))

setOpTests :: TestTree
setOpTests = testGroup
  "Set operations"
  [ testProperty "union"         prop_union_def
  , testProperty "intersection"  prop_intersection_def
  , testProperty "difference"    prop_difference_def
  , testProperty "symDiff"       prop_symDiff_def
  , testProperty "unions"        prop_unions_def
  , testProperty "intersections" prop_unions_def
  , testProperty "invert"        prop_invert_def
  , testProperty "select"        prop_select_def
  , testProperty "exclude"       prop_exclude_def
  , testProperty "selectBits"    prop_selectBits_def
  , testProperty "excludeBits"   prop_excludeBits_def
  , testProperty "countBits"     prop_countBits_def
  ]

union :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
union = zipBits (.|.)

prop_union_def :: U.Vector Bit -> U.Vector Bit -> Property
prop_union_def xs ys =
  U.toList (union xs ys) === zipWith (.|.) (U.toList xs) (U.toList ys)

intersection :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
intersection = zipBits (.&.)

prop_intersection_def :: U.Vector Bit -> U.Vector Bit -> Property
prop_intersection_def xs ys =
  U.toList (intersection xs ys) === zipWith (.&.) (U.toList xs) (U.toList ys)

difference :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
difference = zipBits (\a b -> a .&. complement b)

prop_difference_def :: U.Vector Bit -> U.Vector Bit -> Property
prop_difference_def xs ys = U.toList (difference xs ys)
  === zipWith diff (U.toList xs) (U.toList ys)
  where diff x y = x .&. complement y

symDiff :: U.Vector Bit -> U.Vector Bit -> U.Vector Bit
symDiff = zipBits xor

prop_symDiff_def :: U.Vector Bit -> U.Vector Bit -> Property
prop_symDiff_def xs ys =
  U.toList (symDiff xs ys) === zipWith xor (U.toList xs) (U.toList ys)

unions :: NonEmpty (U.Vector Bit) -> U.Vector Bit
unions (x :| xs) = U.slice 0 l $ U.modify (go xs) x
 where
  l = minimum $ fmap U.length (x :| xs)
  go []       _   = pure ()
  go (y : ys) acc = do
    zipInPlace (.|.) y acc
    go ys acc

prop_unions_def :: U.Vector Bit -> [U.Vector Bit] -> Property
prop_unions_def xs xss = unions (xs :| xss) === foldr union xs xss

intersections :: NonEmpty (U.Vector Bit) -> U.Vector Bit
intersections (x :| xs) = U.slice 0 l $ U.modify (go xs) x
 where
  l = minimum $ fmap U.length (x :| xs)
  go []       _   = pure ()
  go (y : ys) acc = do
    zipInPlace (.&.) y acc
    go ys acc

prop_intersections_def :: U.Vector Bit -> [U.Vector Bit] -> Property
prop_intersections_def xs xss =
  intersections (xs :| xss) === foldr intersection xs xss

prop_invert_def :: U.Vector Bit -> Bool
prop_invert_def xs =
  U.toList (U.modify invertInPlace xs) == map complement (U.toList xs)

select :: U.Unbox a => U.Vector Bit -> U.Vector a -> [a]
select mask ws = U.toList (U.map snd (U.filter (unBit . fst) (U.zip mask ws)))

prop_select_def :: U.Vector Bit -> U.Vector Word -> Bool
prop_select_def xs ys =
  select xs ys == [ x | (Bit True, x) <- zip (U.toList xs) (U.toList ys) ]

exclude :: U.Unbox a => U.Vector Bit -> U.Vector a -> [a]
exclude mask ws =
  U.toList (U.map snd (U.filter (not . unBit . fst) (U.zip mask ws)))

prop_exclude_def :: U.Vector Bit -> U.Vector Word -> Bool
prop_exclude_def xs ys =
  exclude xs ys == [ x | (Bit False, x) <- zip (U.toList xs) (U.toList ys) ]

prop_selectBits_def :: U.Vector Bit -> U.Vector Bit -> Bool
prop_selectBits_def xs ys = selectBits xs ys == U.fromList (select xs ys)

prop_excludeBits_def :: U.Vector Bit -> U.Vector Bit -> Bool
prop_excludeBits_def xs ys = excludeBits xs ys == U.fromList (exclude xs ys)

prop_countBits_def :: U.Vector Bit -> Bool
prop_countBits_def xs = countBits xs == U.length (selectBits xs xs)
