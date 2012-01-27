module Tests.MVector where

import Support

import Control.Monad.ST
import Data.Bit
import Data.STRef
import qualified Data.Vector.Generic             as V
import qualified Data.Vector.Generic.New         as N
import qualified Data.Vector.Unboxed.Bit         as U (unsafeFreeze, reverse, fromWords, toWords)
import qualified Data.Vector.Unboxed.Mutable.Bit as U
import qualified Data.Vector.Unboxed.Mutable     as M
import Data.Word
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

mvectorTests = testGroup "Data.Vector.Unboxed.Mutable.Bit"
    [ testGroup "Data.Vector.Unboxed.Mutable functions"
        [ testProperty "slice"          prop_slice_def
        ]
    , testGroup "Read/write Words"
        [ testProperty "readWord"       prop_readWord_def
        , testProperty "writeWord"      prop_writeWord_def
        , testProperty "cloneFromWords" (prop_cloneFromWords_def 10000)
        , testProperty "cloneToWords"   prop_cloneToWords_def
        ]
    , testGroup "mapMInPlaceWithIndex"
        [ testProperty "maps left to right" prop_mapMInPlaceWithIndex_leftToRight
        ]
    , testProperty "reverseInPlace" prop_reverseInPlace_def
    ]

prop_slice_def :: Int -> Int -> N.New U.Vector Bit -> Bool
prop_slice_def s n xs
    =  sliceList s' n' (U.toList xs')
    == U.toList (runST (do
        xs <- N.run xs
        U.unsafeFreeze (M.slice s' n' xs)))
    where
        xs' = V.new xs
        (s', n') = trimSlice s n (U.length xs')

prop_readWord_def n = withNonEmptyMVec
    (\xs ->   readWordL (U.toList xs) (n `mod` U.length xs))
    (\xs -> U.readWord            xs  (n `mod` M.length xs))

prop_writeWord_def n w = withNonEmptyMVec
    (\xs -> U.fromList
               $ writeWordL (U.toList xs) (n `mod` U.length xs) w)
    (\xs -> do U.writeWord            xs  (n `mod` M.length xs) w
               U.unsafeFreeze xs)

prop_cloneFromWords_def :: Int -> Int -> N.New U.Vector Word -> Bool
prop_cloneFromWords_def maxN n' ws 
    =  runST (N.run ws >>= U.cloneFromWords n >>= U.unsafeFreeze)
    == U.fromWords n (V.new ws)
    where n = n' `mod` maxN

prop_cloneToWords_def :: N.New U.Vector Bit -> Bool
prop_cloneToWords_def xs
    =  runST (N.run xs >>= U.cloneToWords >>= U.unsafeFreeze)
    == U.toWords (V.new xs)

prop_mapMInPlaceWithIndex_leftToRight :: N.New U.Vector Bit -> Bool
prop_mapMInPlaceWithIndex_leftToRight xs 
    = runST $ do
        x <- newSTRef (-1)
        xs <- N.run xs
        let f i _ = do
                j <- readSTRef x
                writeSTRef x i
                return (if i > j then maxBound else 0)
        U.mapMInPlaceWithIndex f xs
        xs <- U.unsafeFreeze xs
        return (all toBool (U.toList xs))

prop_reverseInPlace_def xs
    =  U.reverse (V.new xs)
    == runST (do
        xs <- N.run xs
        U.reverseInPlace xs
        U.unsafeFreeze xs)

