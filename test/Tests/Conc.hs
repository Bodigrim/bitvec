module Tests.Conc
  ( concTests
  ) where

import Control.Concurrent
import Control.Monad
import Data.Bit.ThreadSafe
import Data.Bits
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Test.Tasty.QuickCheck

concTests :: TestTree
concTests = testGroup "Concurrency"
  [ testProperty "invertInPlace"  case_conc_invert
  , testProperty "reverseInPlace" case_conc_reverse
  , testProperty "zipInPlace"     case_conc_zip
  ]

runConcurrently :: IO () -> IO () -> IO ()
runConcurrently action1 action2 = do
  m <- newEmptyMVar
  _ <- forkIO $ do
    action1
    putMVar m ()
  action2
  takeMVar m

case_conc_invert :: Property
case_conc_invert = ioProperty $ replicateM_ 1000 $ do
  let len  = 64
      len' = 37
  vec <- M.replicate len (Bit True)
  ref <- V.freeze vec :: IO (U.Vector Bit)
  runConcurrently
    (replicateM_ 1000 $ invertInPlace (M.take len' vec))
    (replicateM_ 1000 $ invertInPlace (M.drop len' vec))
  wec <- V.unsafeFreeze vec
  pure $ ref === wec

case_conc_reverse :: Property
case_conc_reverse = ioProperty $ replicateM_ 1000 $ do
  let len  = 128
      len' = 66
  vec <- M.new len
  forM_ [0 .. len - 1] $ \i -> M.write vec i (Bit $ odd i)
  ref <- V.freeze vec :: IO (U.Vector Bit)
  runConcurrently
    (replicateM_ 1000 $ reverseInPlace (M.take len' vec))
    (replicateM_ 1000 $ reverseInPlace (M.drop len' vec))
  wec <- V.unsafeFreeze vec
  pure $ ref === wec

case_conc_zip :: Property
case_conc_zip = ioProperty $ replicateM_ 1000 $ do
  let len  = 128
      len' = 37
  vec <- M.replicate len (Bit True)
  let ref = V.replicate len (Bit False)
  runConcurrently
    (replicateM_ 1001 $ zipInPlace (const complement) ref (M.take len' vec))
    (replicateM_ 1001 $ zipInPlace (const complement) ref (M.drop len' vec))
  wec <- V.unsafeFreeze vec
  pure $ ref === wec
