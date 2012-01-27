module Tests.MVector where

import Support

import Control.Monad.ST
import Data.Bit
import qualified Data.Vector.Generic             as V
import qualified Data.Vector.Generic.New         as N
import qualified Data.Vector.Unboxed.Bit         as U
import qualified Data.Vector.Unboxed.Mutable.Bit as U
import qualified Data.Vector.Unboxed.Mutable     as M
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

mvectorTests = testGroup "Data.Vector.Unboxed.Mutable.Bit"
    [ testProperty "readWord"       prop_readWord_def
    , testProperty "writeWord"      prop_writeWord_def
    , testProperty "slice"          prop_slice_def
    , testProperty "reverseInPlace" prop_reverseInPlace_def
    ]

prop_readWord_def n = withNonEmptyMVec
    (\xs ->   readWordL (U.toList xs) (n `mod` U.length xs))
    (\xs -> U.readWord            xs  (n `mod` M.length xs))

prop_writeWord_def n w = withNonEmptyMVec
    (\xs -> U.fromList
               $ writeWordL (U.toList xs) (n `mod` U.length xs) w)
    (\xs -> do U.writeWord            xs  (n `mod` M.length xs) w
               U.unsafeFreeze xs)

prop_slice_def :: Int -> Int -> N.New U.Vector Bit -> Bool
prop_slice_def s n xs
    =  sliceList s' n' (U.toList xs')
    == U.toList (runST (do
        xs <- N.run xs
        U.unsafeFreeze (M.slice s' n' xs)))
    where
        xs' = V.new xs
        (s', n') = trimSlice s n (U.length xs')

prop_reverseInPlace_def xs
    =  U.reverse (V.new xs)
    == runST (do
        xs <- N.run xs
        U.reverseInPlace xs
        U.unsafeFreeze xs)

