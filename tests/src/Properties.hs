module Properties where

import Support

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Bit
import Data.Bits
import qualified Data.Vector.Generic             as V
import qualified Data.Vector.Generic.New         as N
import qualified Data.Vector.Unboxed.Bit         as U
import qualified Data.Vector.Unboxed.Mutable.Bit as U
import qualified Data.Vector.Unboxed.Mutable     as M
import Test.QuickCheck

prop_toList_fromList xs
    =  U.toList (U.fromList xs :: U.Vector Bit)
    == xs

prop_fromList_toList xs
    =  U.fromList (U.toList xs)
    == (xs :: U.Vector Bit)

prop_indexWord_def n xs 
    = not (U.null xs)
    ==> readWordL  (U.toList xs) n'
     == U.indexWord xs           n'
    where
        n' = n `mod` U.length xs

prop_readWord_def n = withNonEmptyMVec
    (\xs ->   readWordL (U.toList xs) (n `mod` U.length xs))
    (\xs -> U.readWord            xs  (n `mod` M.length xs))

prop_writeWord_def n w = withNonEmptyMVec
    (\xs -> U.fromList
               $ writeWordL (U.toList xs) (n `mod` U.length xs) w)
    (\xs -> do U.writeWord            xs  (n `mod` M.length xs) w
               U.unsafeFreeze xs)

prop_slice_def :: Int -> Int -> U.Vector Bit -> Bool
prop_slice_def s n xs
    =  sliceList s' n' (U.toList xs)
    == U.toList (U.slice s' n' xs)
    where
        (s', n') = trimSlice s n (U.length xs)

prop_sliceM_def :: Int -> Int -> N.New U.Vector Bit -> Bool
prop_sliceM_def s n xs
    =  sliceList s' n' (U.toList xs')
    == U.toList (runST (do
        xs <- N.run xs
        U.unsafeFreeze (M.slice s' n' xs)))
    where
        xs' = V.new xs
        (s', n') = trimSlice s n (U.length xs')


prop_reverse_def xs
    =   reverse  (U.toList xs)
    ==  U.toList (U.reverse xs)

prop_reverseInPlace_def xs
    =  U.reverse (V.new xs)
    == runST (do
        xs <- N.run xs
        U.reverseInPlace xs
        U.unsafeFreeze xs)

prop_reverseInPlace_def' xs
    =  U.reverse xs
    == runST (do
        xs <- U.thaw xs
        U.reverseInPlace xs
        U.unsafeFreeze xs)
