{-# LANGUAGE CPP #-}

#ifndef BITVEC_THREADSAFE
module Tests.MVector (mvectorTests) where
#else
module Tests.MVectorTS (mvectorTests) where
#endif

import Support

import Control.Monad.ST
#ifndef BITVEC_THREADSAFE
import Data.Bit
#else
import Data.Bit.ThreadSafe
#endif
import Data.Bits
import Data.Proxy
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M (basicInitialize, basicSet)
import qualified Data.Vector.Generic.New as N
import qualified Data.Vector.Unboxed as B
import qualified Data.Vector.Unboxed.Mutable as M
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

mvectorTests :: TestTree
mvectorTests = testGroup "Data.Vector.Unboxed.Mutable.Bit"
  [ testGroup "Data.Vector.Unboxed.Mutable functions"
    [ tenTimesLess $
      testProperty "slice" prop_slice_def
    , testProperty "grow"  prop_grow_def
    ]
  , testGroup "Read/write Words"
    [ tenTimesLess $
      testProperty "castFromWords" prop_castFromWords_def
    , testProperty "cloneToWords"   prop_cloneToWords_def
    , tenTimesLess $
      testProperty "castToWords"    prop_castToWords_def
    ]
  , lawsToTest $ muvectorLaws (Proxy :: Proxy Bit)
  , testCase "basicInitialize 1" case_write_init_read1
  , testCase "basicInitialize 2" case_write_init_read2
  , testCase "basicInitialize 3" case_write_init_read3
  , testCase "basicInitialize 4" case_write_init_read4
  , testCase "basicSet 1"        case_write_set_read1
  , testCase "basicSet 2"        case_write_set_read2
  , testCase "basicSet 3"        case_write_set_read3
  , testCase "basicSet 4"        case_write_set_read4
  , testCase "basicSet 5"        case_set_read1
  , testCase "basicSet 6"        case_set_read2
  , testCase "basicSet 7"        case_set_read3
  , testCase "basicSet 8"        case_set_read4
  , testCase "basicUnsafeCopy1"  case_write_copy_read1
  , testCase "basicUnsafeCopy2"  case_write_copy_read2
  , testCase "basicUnsafeCopy3"  case_write_copy_read3
  , testCase "basicUnsafeCopy4"  case_write_copy_read4
  , testCase "basicUnsafeCopy5"  case_write_copy_read5
  , tenTimesLess $
    testProperty "flipBit" prop_flipBit
  ]

prop_flipBit :: B.Vector Bit -> NonNegative Int -> Property
prop_flipBit xs (NonNegative k) = B.length xs > 0 ==> ys === ys'
  where
    k'  = k `mod` B.length xs
    ys  = B.modify (\v -> M.modify v complement k') xs
    ys' = B.modify (\v -> flipBit v k') xs

case_write_init_read1 :: IO ()
case_write_init_read1 = assertEqual "should be equal" (Bit True) $ runST $ do
  arr <- M.new 2
  M.write arr 0 (Bit True)
  M.basicInitialize (M.slice 1 1 arr)
  M.read arr 0

case_write_init_read2 :: IO ()
case_write_init_read2 = assertEqual "should be equal" (Bit True) $ runST $ do
  arr <- M.new 2
  M.write arr 1 (Bit True)
  M.basicInitialize (M.slice 0 1 arr)
  M.read arr 1

case_write_init_read3 :: IO ()
case_write_init_read3 =
  assertEqual "should be equal" (Bit True, Bit True) $ runST $ do
    arr <- M.new 2
    M.write arr 0 (Bit True)
    M.write arr 1 (Bit True)
    M.basicInitialize (M.slice 1 0 arr)
    (,) <$> M.read arr 0 <*> M.read arr 1

case_write_init_read4 :: IO ()
case_write_init_read4 =
  assertEqual "should be equal" (Bit True, Bit True) $ runST $ do
    arr <- M.new 3
    M.write arr 0 (Bit True)
    M.write arr 2 (Bit True)
    M.basicInitialize (M.slice 1 1 arr)
    (,) <$> M.read arr 0 <*> M.read arr 2

case_write_set_read1 :: IO ()
case_write_set_read1 = assertEqual "should be equal" (Bit True) $ runST $ do
  arr <- M.new 2
  M.write arr 0 (Bit True)
  M.basicSet (M.slice 1 1 arr) (Bit False)
  M.read arr 0

case_write_set_read2 :: IO ()
case_write_set_read2 = assertEqual "should be equal" (Bit True) $ runST $ do
  arr <- M.new 2
  M.write arr 1 (Bit True)
  M.basicSet (M.slice 0 1 arr) (Bit False)
  M.read arr 1

case_write_set_read3 :: IO ()
case_write_set_read3 =
  assertEqual "should be equal" (Bit True, Bit True) $ runST $ do
    arr <- M.new 2
    M.write arr 0 (Bit True)
    M.write arr 1 (Bit True)
    M.basicSet (M.slice 1 0 arr) (Bit False)
    (,) <$> M.read arr 0 <*> M.read arr 1

case_write_set_read4 :: IO ()
case_write_set_read4 =
  assertEqual "should be equal" (Bit True, Bit True) $ runST $ do
    arr <- M.new 3
    M.write arr 0 (Bit True)
    M.write arr 2 (Bit True)
    M.basicSet (M.slice 1 1 arr) (Bit False)
    (,) <$> M.read arr 0 <*> M.read arr 2

case_set_read1 :: IO ()
case_set_read1 = assertEqual "should be equal" (Bit True) $ runST $ do
  arr <- M.new 1
  M.basicSet arr (Bit True)
  M.read arr 0

case_set_read2 :: IO ()
case_set_read2 = assertEqual "should be equal" (Bit True) $ runST $ do
  arr <- M.new 2
  M.basicSet (M.slice 1 1 arr) (Bit True)
  M.read arr 1

case_set_read3 :: IO ()
case_set_read3 = assertEqual "should be equal" (Bit True) $ runST $ do
  arr <- M.new 192
  M.basicSet (M.slice 71 121 arr) (Bit True)
  M.read arr 145

case_set_read4 :: IO ()
case_set_read4 = assertEqual "should be equal" (Bit True) $ runST $ do
  arr <- M.slice 27 38 <$> M.new 65
  M.basicSet arr (Bit True)
  M.read arr 21

case_write_copy_read1 :: IO ()
case_write_copy_read1 = assertEqual "should be equal" (Bit True) $ runST $ do
  src <- M.slice 37 28 <$> M.new 65
  M.write src 27 (Bit True)
  dst <- M.slice 37 28 <$> M.new 65
  M.copy dst src
  M.read dst 27

case_write_copy_read2 :: IO ()
case_write_copy_read2 = assertEqual "should be equal" (Bit True) $ runST $ do
  src <- M.slice 32 33 <$> M.new 65
  M.write src 0 (Bit True)
  dst <- M.slice 32 33 <$> M.new 65
  M.copy dst src
  M.read dst 0

case_write_copy_read3 :: IO ()
case_write_copy_read3 = assertEqual "should be equal" (Bit True) $ runST $ do
  src <- M.slice 1 1 <$> M.new 2
  M.write src 0 (Bit True)
  dst <- M.slice 1 1 <$> M.new 2
  M.copy dst src
  M.read dst 0

case_write_copy_read4 :: IO ()
case_write_copy_read4 = assertEqual "should be equal" (Bit True) $ runST $ do
  src <- M.slice 12 52 <$> M.new 64
  M.write src 22 (Bit True)
  dst <- M.slice 12 52 <$> M.new 64
  M.copy dst src
  M.read dst 22

case_write_copy_read5 :: IO ()
case_write_copy_read5 = assertEqual "should be equal" (Bit True) $ runST $ do
  src <- M.slice 48 80 <$> M.new 128
  M.write src 46 (Bit True)
  dst <- M.slice 48 80 <$> M.new 128
  M.copy dst src
  M.read dst 46

prop_slice_def
  :: NonNegative Int
  -> NonNegative Int
  -> N.New B.Vector Bit
  -> Property
prop_slice_def (NonNegative s) (NonNegative n) xs =
  l > 0 ==> runST $ do
    let xs' = V.new xs
    xs1 <- N.run xs
    xs2 <- V.unsafeFreeze (M.slice s' n' xs1)
    return (B.toList xs2 === sliceList s' n' (B.toList xs'))
  where
    l = V.length (V.new xs)
    s' = s `mod` l
    n' = n `mod` (l - s')

prop_grow_def :: B.Vector Bit -> NonNegative Int -> Bool
prop_grow_def xs (NonNegative m) = runST $ do
  let n = B.length xs
  v0  <- B.thaw xs
  v1  <- M.grow v0 m
  fv0 <- B.freeze v0
  fv1 <- B.freeze v1
  return (fv0 == B.take n fv1)

prop_castFromWords_def :: N.New B.Vector Word -> Property
prop_castFromWords_def ws =
  runST (N.run ws >>= pure . castFromWordsM >>= V.unsafeFreeze)
    === castFromWords (V.new ws)

prop_cloneToWords_def :: N.New B.Vector Bit -> Property
prop_cloneToWords_def xs =
  runST (N.run xs >>= cloneToWordsM >>= V.unsafeFreeze)
    === cloneToWords (V.new xs)

prop_castToWords_def :: N.New B.Vector Word -> Property
prop_castToWords_def xs = runST $ do
  vs <- N.run xs
  vs' <- cloneToWordsM (castFromWordsM vs)
  case castToWordsM (castFromWordsM vs) of
    Nothing -> pure $ property False
    Just vs'' -> do
      ws'  <- V.unsafeFreeze vs'
      ws'' <- V.unsafeFreeze vs''
      pure $ ws' === ws''
