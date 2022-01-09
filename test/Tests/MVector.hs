{-# LANGUAGE CPP #-}

#ifndef BITVEC_THREADSAFE
module Tests.MVector (mvectorTests) where
#else
module Tests.MVectorTS (mvectorTests) where
#endif

import Support

import Control.Exception
import Control.Monad.ST
#ifndef BITVEC_THREADSAFE
import Data.Bit
#else
import Data.Bit.ThreadSafe
#endif
import Data.Bits
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Generic.New as N
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Test.Tasty
import Test.Tasty.QuickCheck

#ifdef MIN_VERSION_quickcheck_classes
import Data.Proxy
import Test.QuickCheck.Classes
#endif

mvectorTests :: TestTree
mvectorTests = testGroup "Data.Vector.Unboxed.Mutable.Bit"
  [ testGroup "Data.Vector.Unboxed.Mutable functions"
    [ tenTimesLess $
      testProperty "slice" prop_slice_def
    , testProperty "grow"  prop_grow_def
    ]
  , testGroup "Read/write Words"
    [ tenTimesLess $
      testProperty "castFromWords"  prop_castFromWords_def
    , testProperty "cloneToWords"   prop_cloneToWords_def
    , tenTimesLess $
      testProperty "castToWords_1"  prop_castToWords_1
    , tenTimesLess $
      testProperty "castToWords_2"  prop_castToWords_2
    ]
#ifdef MIN_VERSION_quickcheck_classes
  , lawsToTest' $ muvectorLaws (Proxy :: Proxy Bit)
#endif
  , testProperty "basicInitialize 1" case_write_init_read1
  , testProperty "basicInitialize 2" case_write_init_read2
  , testProperty "basicInitialize 3" case_write_init_read3
  , testProperty "basicInitialize 4" case_write_init_read4
  , testProperty "basicSet 1"        case_write_set_read1
  , testProperty "basicSet 2"        case_write_set_read2
  , testProperty "basicSet 3"        case_write_set_read3
  , testProperty "basicSet 4"        case_write_set_read4
  , testProperty "basicSet 5"        case_set_read1
  , testProperty "basicSet 6"        case_set_read2
  , testProperty "basicSet 7"        case_set_read3
  , testProperty "basicSet 8"        case_set_read4
  , testProperty "basicUnsafeCopy1"  case_write_copy_read1
  , testProperty "basicUnsafeCopy2"  case_write_copy_read2
  , testProperty "basicUnsafeCopy3"  case_write_copy_read3
  , testProperty "basicUnsafeCopy4"  case_write_copy_read4
  , testProperty "basicUnsafeCopy5"  case_write_copy_read5
  , tenTimesLess $
    testProperty "flipBit" prop_flipBit
  , testProperty "new negative"       prop_new_neg
  , testProperty "replicate negative" prop_replicate_neg
  ]

prop_flipBit :: U.Vector Bit -> NonNegative Int -> Property
prop_flipBit xs (NonNegative k) = U.length xs > 0 ==> ys === ys'
  where
    k'  = k `mod` U.length xs
    ys  = U.modify (\v -> M.modify v complement k') xs
    ys' = U.modify (\v -> flipBit v k') xs

case_write_init_read1 :: Property
case_write_init_read1 = (=== Bit True) $ runST $ do
  arr <- M.new 2
  M.write arr 0 (Bit True)
  MG.basicInitialize (M.slice 1 1 arr)
  M.read arr 0

case_write_init_read2 :: Property
case_write_init_read2 = (=== Bit True) $ runST $ do
  arr <- M.new 2
  M.write arr 1 (Bit True)
  MG.basicInitialize (M.slice 0 1 arr)
  M.read arr 1

case_write_init_read3 :: Property
case_write_init_read3 =
  (=== (Bit True, Bit True)) $ runST $ do
    arr <- M.new 2
    M.write arr 0 (Bit True)
    M.write arr 1 (Bit True)
    MG.basicInitialize (M.slice 1 0 arr)
    (,) <$> M.read arr 0 <*> M.read arr 1

case_write_init_read4 :: Property
case_write_init_read4 =
  (=== (Bit True, Bit True)) $ runST $ do
    arr <- M.new 3
    M.write arr 0 (Bit True)
    M.write arr 2 (Bit True)
    MG.basicInitialize (M.slice 1 1 arr)
    (,) <$> M.read arr 0 <*> M.read arr 2

case_write_set_read1 :: Property
case_write_set_read1 = (=== Bit True) $ runST $ do
  arr <- M.new 2
  M.write arr 0 (Bit True)
  MG.basicSet (M.slice 1 1 arr) (Bit False)
  M.read arr 0

case_write_set_read2 :: Property
case_write_set_read2 = (=== Bit True) $ runST $ do
  arr <- M.new 2
  M.write arr 1 (Bit True)
  MG.basicSet (M.slice 0 1 arr) (Bit False)
  M.read arr 1

case_write_set_read3 :: Property
case_write_set_read3 =
  (=== (Bit True, Bit True)) $ runST $ do
    arr <- M.new 2
    M.write arr 0 (Bit True)
    M.write arr 1 (Bit True)
    MG.basicSet (M.slice 1 0 arr) (Bit False)
    (,) <$> M.read arr 0 <*> M.read arr 1

case_write_set_read4 :: Property
case_write_set_read4 =
  (=== (Bit True, Bit True)) $ runST $ do
    arr <- M.new 3
    M.write arr 0 (Bit True)
    M.write arr 2 (Bit True)
    MG.basicSet (M.slice 1 1 arr) (Bit False)
    (,) <$> M.read arr 0 <*> M.read arr 2

case_set_read1 :: Property
case_set_read1 = (=== Bit True) $ runST $ do
  arr <- M.new 1
  MG.basicSet arr (Bit True)
  M.read arr 0

case_set_read2 :: Property
case_set_read2 = (=== Bit True) $ runST $ do
  arr <- M.new 2
  MG.basicSet (M.slice 1 1 arr) (Bit True)
  M.read arr 1

case_set_read3 :: Property
case_set_read3 = (=== Bit True) $ runST $ do
  arr <- M.new 192
  MG.basicSet (M.slice 71 121 arr) (Bit True)
  M.read arr 145

case_set_read4 :: Property
case_set_read4 = (=== Bit True) $ runST $ do
  arr <- M.slice 27 38 <$> M.new 65
  MG.basicSet arr (Bit True)
  M.read arr 21

case_write_copy_read1 :: Property
case_write_copy_read1 = (=== Bit True) $ runST $ do
  src <- M.slice 37 28 <$> M.new 65
  M.write src 27 (Bit True)
  dst <- M.slice 37 28 <$> M.new 65
  M.copy dst src
  M.read dst 27

case_write_copy_read2 :: Property
case_write_copy_read2 = (=== Bit True) $ runST $ do
  src <- M.slice 32 33 <$> M.new 65
  M.write src 0 (Bit True)
  dst <- M.slice 32 33 <$> M.new 65
  M.copy dst src
  M.read dst 0

case_write_copy_read3 :: Property
case_write_copy_read3 = (=== Bit True) $ runST $ do
  src <- M.slice 1 1 <$> M.new 2
  M.write src 0 (Bit True)
  dst <- M.slice 1 1 <$> M.new 2
  M.copy dst src
  M.read dst 0

case_write_copy_read4 :: Property
case_write_copy_read4 = (=== Bit True) $ runST $ do
  src <- M.slice 12 52 <$> M.new 64
  M.write src 22 (Bit True)
  dst <- M.slice 12 52 <$> M.new 64
  M.copy dst src
  M.read dst 22

case_write_copy_read5 :: Property
case_write_copy_read5 = (=== Bit True) $ runST $ do
  src <- M.slice 48 80 <$> M.new 128
  M.write src 46 (Bit True)
  dst <- M.slice 48 80 <$> M.new 128
  M.copy dst src
  M.read dst 46

prop_slice_def
  :: NonNegative Int
  -> NonNegative Int
  -> N.New U.Vector Bit
  -> Property
prop_slice_def (NonNegative s) (NonNegative n) xs =
  l > 0 ==> runST $ do
    let xs' = V.new xs
    xs1 <- N.run xs
    xs2 <- V.unsafeFreeze (M.slice s' n' xs1)
    return (U.toList xs2 === sliceList s' n' (U.toList xs'))
  where
    l = V.length (V.new xs)
    s' = s `mod` l
    n' = n `mod` (l - s')

prop_grow_def :: U.Vector Bit -> NonNegative Int -> Bool
prop_grow_def xs (NonNegative m) = runST $ do
  let n = U.length xs
  v0  <- U.thaw xs
  v1  <- M.grow v0 m
  fv0 <- U.freeze v0
  fv1 <- U.freeze v1
  return (fv0 == U.take n fv1)

prop_castFromWords_def :: N.New U.Vector Word -> Property
prop_castFromWords_def ws =
  runST (N.run ws >>= pure . castFromWordsM >>= V.unsafeFreeze)
    === castFromWords (V.new ws)

prop_cloneToWords_def :: N.New U.Vector Bit -> Property
prop_cloneToWords_def xs =
  runST (N.run xs >>= cloneToWordsM >>= V.unsafeFreeze)
    === cloneToWords (V.new xs)

prop_castToWords_1 :: N.New U.Vector Word -> Property
prop_castToWords_1 xs = runST $ do
  vs <- N.run xs
  vs' <- cloneToWordsM (castFromWordsM vs)
  case castToWordsM (castFromWordsM vs) of
    Nothing -> pure $ property False
    Just vs'' -> do
      ws'  <- V.unsafeFreeze vs'
      ws'' <- V.unsafeFreeze vs''
      pure $ ws' === ws''

prop_castToWords_2 :: N.New U.Vector Bit -> Property
prop_castToWords_2 xs = runST $ do
  vs <- N.run xs
  case castToWordsM vs of
    Nothing  -> pure $ property True
    Just ws -> do
      ws' <- V.unsafeFreeze (castFromWordsM ws)
      ws'' <- V.unsafeFreeze vs
      pure $ ws' === ws''

prop_replicate_neg :: Positive Int -> Bit -> Property
prop_replicate_neg (Positive n) x = ioProperty $ do
  ret <- try (evaluate (runST $ MG.basicUnsafeReplicate (-n) x >>= U.unsafeFreeze))
  pure $ property $ case ret of
    Left ErrorCallWithLocation{} -> True
    _ -> False

prop_new_neg :: Positive Int -> Property
prop_new_neg (Positive n) = ioProperty $ do
  ret <- try (evaluate (runST $ MG.basicUnsafeNew (-n) >>= U.unsafeFreeze :: U.Vector Bit))
  pure $ property $ case ret of
    Left ErrorCallWithLocation{} -> True
    _ -> False
