{-# LANGUAGE CPP #-}

#ifndef BITVEC_THREADSAFE
module Tests.MVector where
#else
module Tests.MVectorTS where
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
import qualified Data.Vector.Generic             as V
import qualified Data.Vector.Generic.Mutable     as M (basicInitialize, basicSet)
import qualified Data.Vector.Generic.New         as N
import qualified Data.Vector.Unboxed             as B
import qualified Data.Vector.Unboxed.Mutable     as M
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertEqual)
import Test.QuickCheck
import Test.QuickCheck.Classes

mvectorTests :: Test
mvectorTests = testGroup "Data.Vector.Unboxed.Mutable.Bit"
    [ testGroup "Data.Vector.Unboxed.Mutable functions"
        [ testProperty "slice"          prop_slice_def
        , testProperty "grow"           prop_grow_def
        ]
    , testGroup "Read/write Words"
        [ testProperty "cloneFromWords" prop_cloneFromWords_def
        , testProperty "cloneToWords"   prop_cloneToWords_def
        ]
    , testProperty "reverseInPlace" prop_reverseInPlace_def
    , testGroup "MVector laws" $ map (uncurry testProperty) $ lawsProperties $ muvectorLaws (Proxy :: Proxy Bit)
    , testCase "basicInitialize 1" case_write_init_read1
    , testCase "basicInitialize 2" case_write_init_read2
    , testCase "basicInitialize 3" case_write_init_read3
    , testCase "basicInitialize 4" case_write_init_read4
    , testCase "basicSet 1" case_write_set_read1
    , testCase "basicSet 2" case_write_set_read2
    , testCase "basicSet 3" case_write_set_read3
    , testCase "basicSet 4" case_write_set_read4
    , testCase "basicSet 5" case_set_read1
    , testCase "basicSet 6" case_set_read2
    , testCase "basicSet 7" case_set_read3
    , testCase "basicUnsafeCopy1" case_write_copy_read1
    , testCase "basicUnsafeCopy2" case_write_copy_read2
    , testCase "basicUnsafeCopy3" case_write_copy_read3
    , testCase "basicUnsafeCopy4" case_write_copy_read4
    , testCase "basicUnsafeCopy5" case_write_copy_read5

    , testProperty "flipBit" prop_flipBit
    ]

prop_flipBit :: B.Vector Bit -> NonNegative Int -> Property
prop_flipBit xs (NonNegative k) = k < B.length xs ==> ys === ys'
    where
        ys  = B.modify (\v -> M.modify v complement k) xs
        ys' = B.modify (\v -> flipBit v k) xs

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
case_write_init_read3 = assertEqual "should be equal" (Bit True, Bit True) $ runST $ do
    arr <- M.new 2
    M.write arr 0 (Bit True)
    M.write arr 1 (Bit True)
    M.basicInitialize (M.slice 1 0 arr)
    (,) <$> M.read arr 0 <*> M.read arr 1

case_write_init_read4 :: IO ()
case_write_init_read4 = assertEqual "should be equal" (Bit True, Bit True) $ runST $ do
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
case_write_set_read3 = assertEqual "should be equal" (Bit True, Bit True) $ runST $ do
    arr <- M.new 2
    M.write arr 0 (Bit True)
    M.write arr 1 (Bit True)
    M.basicSet (M.slice 1 0 arr) (Bit False)
    (,) <$> M.read arr 0 <*> M.read arr 1

case_write_set_read4 :: IO ()
case_write_set_read4 = assertEqual "should be equal" (Bit True, Bit True) $ runST $ do
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

prop_slice_def :: Int -> Int -> N.New B.Vector Bit -> Bool
prop_slice_def s n xs = runST $ do
    let xs' = V.new xs
        (s', n') = trimSlice s n (V.length xs')
    xs1 <- N.run xs
    xs2 <- V.unsafeFreeze (M.slice s' n' xs1)

    return (B.toList xs2 == sliceList s' n' (B.toList xs'))

prop_grow_def :: B.Vector Bit -> NonNegative Int -> Bool
prop_grow_def xs (NonNegative m) = runST $ do
    let n = B.length xs
    v0 <- B.thaw xs
    v1 <- M.grow v0 m
    fv0 <- B.freeze v0
    fv1 <- B.freeze v1
    return (fv0 == B.take n fv1)

prop_cloneFromWords_def :: N.New B.Vector Word -> Bool
prop_cloneFromWords_def ws
    =  runST (N.run ws >>= pure . castFromWordsM >>= V.unsafeFreeze)
    == castFromWords (V.new ws)

prop_cloneToWords_def :: N.New B.Vector Bit -> Bool
prop_cloneToWords_def xs
    =  runST (N.run xs >>= cloneToWordsM >>= V.unsafeFreeze)
    == cloneToWords (V.new xs)

prop_reverseInPlace_def :: N.New B.Vector Bit -> Bool
prop_reverseInPlace_def xs
    =  runST (N.run xs >>= \v -> reverseInPlace v >> V.unsafeFreeze v)
    == B.reverse (V.new xs)

