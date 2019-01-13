module Tests.MVector where

import Support

import Control.Monad
import Control.Monad.ST
import Data.Bit
import Data.STRef
import qualified Data.Vector.Generic             as V
import qualified Data.Vector.Generic.New         as N
import qualified Data.Vector.Unboxed.Bit         as B
import qualified Data.Vector.Unboxed.Mutable.Bit as U
import qualified Data.Vector.Unboxed.Mutable     as M
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

mvectorTests :: Test
mvectorTests = testGroup "Data.Vector.Unboxed.Mutable.Bit"
    [ testGroup "Data.Vector.Unboxed.Mutable functions"
        [ testProperty "slice"          prop_slice_def
        , testProperty "grow"           prop_grow_def
        ]
    , testProperty "wordLength"     prop_wordLength_def
    , testGroup "Read/write Words"
        [ testProperty "readWord"       prop_readWord_def
        , testProperty "writeWord"      prop_writeWord_def
        , testProperty "cloneFromWords" (prop_cloneFromWords_def 10000)
        , testProperty "cloneToWords"   prop_cloneToWords_def
        ]
    , testGroup "mapMInPlaceWithIndex"
        [ testProperty "maps left to right" prop_mapMInPlaceWithIndex_leftToRight
        , testProperty "wordSize-aligned"   prop_mapMInPlaceWithIndex_aligned
        ]
    , testProperty "countBits"      prop_countBits_def
    , testProperty "listBits"       prop_listBits_def
    , testProperty "reverseInPlace" prop_reverseInPlace_def
    ]

prop_slice_def :: Int -> Int -> N.New U.Vector Bit -> Bool
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

prop_readWord_def :: Int -> Property
prop_readWord_def n = withNonEmptyMVec
    (\xs ->   readWordL (B.toList xs) (n `mod` V.length xs))
    (\xs -> U.readWord            xs  (n `mod` M.length xs))

prop_writeWord_def :: Int -> Word -> Property
prop_writeWord_def n w = withNonEmptyMVec
    (\xs -> B.fromList
               $ writeWordL (B.toList xs) (n `mod` V.length xs) w)
    (\xs -> do U.writeWord            xs  (n `mod` M.length xs) w
               V.unsafeFreeze xs)

prop_wordLength_def :: N.New U.Vector Bit -> Bool
prop_wordLength_def xs
    =  runST (fmap U.wordLength (N.run xs))
    == runST (fmap U.length (N.run xs >>= U.cloneToWords))

prop_cloneFromWords_def :: Int -> Int -> N.New U.Vector Word -> Bool
prop_cloneFromWords_def maxN n' ws
    =  runST (N.run ws >>= U.cloneFromWords n >>= V.unsafeFreeze)
    == B.fromWords n (V.new ws)
    where n = n' `mod` maxN

prop_cloneToWords_def :: N.New U.Vector Bit -> Bool
prop_cloneToWords_def xs
    =  runST (N.run xs >>= U.cloneToWords >>= V.unsafeFreeze)
    == B.toWords (V.new xs)

prop_mapMInPlaceWithIndex_leftToRight :: N.New U.Vector Bit -> Bool
prop_mapMInPlaceWithIndex_leftToRight xs
    = runST $ do
        x <- newSTRef (-1)
        xs1 <- N.run xs
        let f i _ = do
                j <- readSTRef x
                writeSTRef x i
                return (if i > j then maxBound else 0)
        U.mapMInPlaceWithIndex f xs1
        xs2 <- V.unsafeFreeze xs1
        return (all toBool (B.toList xs2))

prop_mapMInPlaceWithIndex_aligned :: N.New U.Vector Bit -> Bool
prop_mapMInPlaceWithIndex_aligned xs = runST $ do
    ok <- newSTRef True
    xs1 <- N.run xs
    let aligned i   = i `mod` U.wordSize == 0
        f i x = do
            when (not (aligned i)) (writeSTRef ok False)
            return x
    U.mapMInPlaceWithIndex f xs1
    readSTRef ok

prop_countBits_def :: N.New U.Vector Bit -> Bool
prop_countBits_def xs
    =  runST (N.run xs >>= U.countBits)
    == B.countBits (V.new xs)

prop_listBits_def :: N.New U.Vector Bit -> Bool
prop_listBits_def xs
    =  runST (N.run xs >>= U.listBits)
    == B.listBits (V.new xs)

prop_reverseInPlace_def :: N.New U.Vector Bit -> Bool
prop_reverseInPlace_def xs
    =  runST (N.run xs >>= \v -> U.reverseInPlace v >> V.unsafeFreeze v)
    == B.reverse (V.new xs)

