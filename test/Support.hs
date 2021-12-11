{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Support where

import Control.Monad.ST
import Data.Bit
import qualified Data.Bit.ThreadSafe as TS
import Data.Bits
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic.New as N
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Test.Tasty.QuickCheck

#ifdef MIN_VERSION_quickcheck_classes_base
import Test.QuickCheck.Classes.Base
#endif
#ifdef MIN_VERSION_quickcheck_classes
import qualified Test.QuickCheck.Classes as QCC
#endif

instance Arbitrary Bit where
  arbitrary = Bit <$> arbitrary
  shrink    = fmap Bit . shrink . unBit

instance CoArbitrary Bit where
  coarbitrary = coarbitrary . unBit

instance Function Bit where
  function f = functionMap unBit Bit f

instance Arbitrary TS.Bit where
  arbitrary = TS.Bit <$> arbitrary
  shrink    = fmap TS.Bit . shrink . TS.unBit

instance CoArbitrary TS.Bit where
  coarbitrary = coarbitrary . TS.unBit

instance Function TS.Bit where
  function f = functionMap TS.unBit TS.Bit f

instance (Arbitrary a, U.Unbox a) => Arbitrary (U.Vector a) where
  arbitrary = frequency
    [ (10, U.fromList <$> arbitrary)
    , (2 , U.drop <$> arbitrary <*> arbitrary)
    , (2 , U.take <$> arbitrary <*> arbitrary)
    , (2 , slice <$> arbitrary <*> arbitrary <*> arbitrary)
    ]
   where
    slice s n v = let (s', n') = trimSlice s n (U.length v) in U.slice s' n' v
  shrink v = let len = U.length v in
    [ U.take (len - s) v | s <- [1 .. len] ] ++
    [ U.drop s         v | s <- [1 .. len] ] ++
    [ v U.// [(i, x)] | i <- [0 .. len - 1], x <- shrink (v U.! i) ]

instance {-# OVERLAPPING #-} Arbitrary (Large (U.Vector Bit)) where
  arbitrary = Large . castFromWords <$> arbitrary
  shrink (Large v) = Large <$> shrink v

instance {-# OVERLAPPING #-} Arbitrary (Large (U.Vector TS.Bit)) where
  arbitrary = Large . TS.castFromWords <$> arbitrary
  shrink (Large v) = Large <$> shrink v

instance Arbitrary F2Poly where
  arbitrary = toF2Poly <$> arbitrary
  shrink v = toF2Poly <$> shrink (unF2Poly v)

instance {-# OVERLAPPING #-} Arbitrary (Large F2Poly) where
  arbitrary = Large . toF2Poly . castFromWords <$> arbitrary
  shrink (Large v) = Large . toF2Poly <$> shrink (unF2Poly v)

instance (Show (v a), V.Vector v a) => Show (N.New v a) where
  showsPrec p = showsPrec p . V.new

newFromList :: forall a v . V.Vector v a => [a] -> N.New v a
newFromList xs = N.create (V.thaw (V.fromList xs :: v a))

-- this instance is designed to make sure that the arbitrary vectors we work with are not all nicely aligned; we need to deal with cases where the vector is a weird slice of some other vector.
instance (V.Vector v a, Arbitrary a) => Arbitrary (N.New v a) where
  arbitrary = frequency
    [ (10, newFromList <$> arbitrary)
    , (2 , N.drop <$> arbitrary <*> arbitrary)
    , (2 , N.take <$> arbitrary <*> arbitrary)
    , (2 , slice <$> arbitrary <*> arbitrary <*> arbitrary)
    ]
   where
    slice s n = N.apply
      $ \v -> let (s', n') = trimSlice s n (M.length v) in M.slice s' n' v
  shrink v =
    [ N.take s v | s <- [0 .. len - 1] ] ++
    [ N.drop s v | s <- [1 .. len] ]
    where len = runST (M.length <$> N.run v)

trimSlice :: Integral a => a -> a -> a -> (a, a)
trimSlice s n l = (s', n')
 where
  s' | l == 0    = 0
     | otherwise = s `mod` l
  n' | s' == 0   = 0
     | otherwise = n `mod` (l - s')

sliceList :: Int -> Int -> [a] -> [a]
sliceList s n = take n . drop s

wordSize :: Int
wordSize = finiteBitSize (0 :: Word)

packBitsToWord :: FiniteBits a => [Bit] -> (a, [Bit])
packBitsToWord = loop 0 zeroBits
 where
  loop _ w [] = (w, [])
  loop i w (x : xs)
    | i >= finiteBitSize w = (w, x : xs)
    | otherwise            = loop (i + 1) (if unBit x then setBit w i else w) xs

readWordL :: [Bit] -> Int -> Word
readWordL xs 0 = fst (packBitsToWord xs)
readWordL xs n = readWordL (drop n xs) 0

wordToBitList :: FiniteBits a => a -> [Bit]
wordToBitList w = [ Bit (testBit w i) | i <- [0 .. finiteBitSize w - 1] ]

writeWordL :: [Bit] -> Int -> Word -> [Bit]
writeWordL xs 0 w = zipWith const (wordToBitList w) xs ++ drop wordSize xs
writeWordL xs n w = pre ++ writeWordL post 0 w
  where (pre, post) = splitAt n xs

prop_writeWordL_preserves_length :: [Bit] -> NonNegative Int -> Word -> Property
prop_writeWordL_preserves_length xs (NonNegative n) w =
  length (writeWordL xs n w) === length xs

prop_writeWordL_preserves_prefix :: [Bit] -> NonNegative Int -> Word -> Property
prop_writeWordL_preserves_prefix xs (NonNegative n) w =
  take n (writeWordL xs n w) === take n xs

prop_writeWordL_preserves_suffix :: [Bit] -> NonNegative Int -> Word -> Property
prop_writeWordL_preserves_suffix xs (NonNegative n) w =
  drop (n + wordSize) (writeWordL xs n w) === drop (n + wordSize) xs

prop_writeWordL_readWordL :: [Bit] -> Int -> Property
prop_writeWordL_readWordL xs n = writeWordL xs n (readWordL xs n) === xs

withNonEmptyMVec
  :: (Eq t, Show t)
  => (U.Vector Bit -> t)
  -> (forall s . U.MVector s Bit -> ST s t)
  -> Property
withNonEmptyMVec f g = forAll arbitrary $ \xs ->
  let xs' = V.new xs in not (U.null xs') ==> f xs' === runST (N.run xs >>= g)

tenTimesLess :: TestTree -> TestTree
tenTimesLess = adjustOption $
  \(QuickCheckTests n) -> QuickCheckTests (max 100 (n `div` 10))

twoTimesMore :: TestTree -> TestTree
twoTimesMore = adjustOption $
  \(QuickCheckTests n) -> QuickCheckTests (n * 2)

#ifdef MIN_VERSION_quickcheck_classes_base
lawsToTest :: Laws -> TestTree
lawsToTest (Laws name props) =
  testGroup name $ map (uncurry testProperty) props
#endif

#ifdef MIN_VERSION_quickcheck_classes
lawsToTest' :: QCC.Laws -> TestTree
lawsToTest' (QCC.Laws name props) =
  testGroup name $ map (uncurry testProperty) props
#endif
