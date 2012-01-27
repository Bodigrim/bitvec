module Tests.Bit where

import Support

import Data.Bit
import Data.Bits
import Test.HUnit
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

b0 = 0 :: Bit
b1 = 1 :: Bit

testOp opName op rOp =
    [ testCase (unwords [opName, show x])
        (op x @?= rOp x)
    | x <- [0, 1 :: Bit]
    ]

testBinop opName op rOp =
    [ testCase (unwords [show x, opName, show y])
        (op x y @?= rOp x y)
    | x <- [0, 1 :: Bit]
    , y <- [0, 1 :: Bit]
    ]

bitTests = testGroup "Data.Bit"
    [ testGroup "basic assertions"
        [ testCase "toBool 0"       (toBool 0       @?= False)
        , testCase "toBool 1"       (toBool 1       @?= True)
        , testCase "fromBool False" (fromBool False @?= 0)
        , testCase "fromBool True"  (fromBool True  @?= 1)
        , testCase "fromInteger 0"  (fromInteger 0  @?= (0 :: Bit))
        , testCase "fromInteger 1"  (fromInteger 1  @?= (1 :: Bit))
        ]
    , testGroup "Num instance forms ℤ/2" $ concat
        [ [ testProperty "fromInteger == odd" prop_fromInteger ]
        , testBinop "+" (+) xor
        , testBinop "*" (*) (.&.)
        , testBinop "-" (+) xor
        , testOp "negate" negate id
        , testOp "abs"    abs    id
        , testOp "signum" signum id
        ]
    ]

prop_fromInteger x = fromInteger x == fromBool (odd x)