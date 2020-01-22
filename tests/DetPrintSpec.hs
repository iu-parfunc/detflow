{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |

module DetPrintSpec where

import Control.Monad.DetIO
import Control.Monad.DetIO.Perms
import Prelude hiding (putStrLn)
-- import qualified Prelude as P
import System.IO.Silently
-- import Control.Exception
import Control.Concurrent.Async as A
import qualified Control.Concurrent as C

import Test.Hspec hiding (example)
-- import           Test.Hspec.QuickCheck (prop)
-- import           Test.HUnit
-- import           Test.QuickCheck (Arbitrary(..), Gen, generate)

--------------------------------------------------------------------------------


-- startingPerms = Node noPerm 0
--                 (M.fromList [ ("input",  Has (R 1) )
--                             , ("output", Has (RW 1)) ])
perms0 :: Perms
Right perms0 = fromPP (mkPermR "/input") `addPerms'`
               fromPP (mkPermRW "/output")

fromPP :: PathPerm -> Perms
fromPP x = let IP y = fromPathPerm x in y


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Passing, valid tests: " $ do
--    it "t1: trivial" (runIO t1 >>= (`shouldBe` 3))
    it "can_return" (t1 `shouldReturn` 3)
    it "can_new" (t2 `shouldReturn` 3)
    it "can_put" (t3 `shouldReturn` 3)
    it "can_get" (t4 `shouldReturn` 'a')

    it "can_print_on_main_thread" (t5 `shouldPrint` "hi")
    it "can_sequentially_print"   (t6 `shouldPrint` "hi there")
    it "can_parallel_print"       (t7 `shouldPrint` "there")
    it "can_force_child_print"    (t8 `shouldPrint` "a b c")

    it "can_print_from_nested_child"
       -- (t9 `shouldPrint` "a1 b1 b2 c1 a2")
       -- Accepting this policy for now:
       (t9 `shouldPrint` "a1 b1 c1 b2 a2")

    it "1st_class_ivars"    (t10 `shouldPrint` "a1 b1 b2 a2 c1 a3")
    it "unjoined_child_thread"  (t11 `shouldPrint` "a1 b1 b2 a2")

    it "DIVERGING_child"  (t12 `shouldDiverge` "a1 b1 b2 a2")

-- FIXME: get this passing
--    it "should error" (t13 `shouldThrow` \(_ :: SomeException) -> True)

    it "extra_child->parent_sync_edge"  (t14 `shouldPrint` "a1 a2 b1 a3 b2 a4")
    it "extra_parent->child_sync_edge"  (t15 `shouldPrint` "a1 a2 a3 b1 b2 a4")

    it "nested_diamond_of_IVar_edges"  (t16 `shouldPrint` "a1 a2 b1 a3 a4 b2 b3 a5")
    it "flip_of_prev"                  (t17 `shouldPrint` "a1 a2 a3 b1 b2 a4 b3 a5")


--------------------------------------------------------------------------------

shouldPrint :: IO () -> String -> Expectation
shouldPrint act str = (do
  (output,()) <- capture act
  return (words output))
  `shouldReturn` (words str)

shouldDiverge :: IO () -> String -> Expectation
shouldDiverge act str = (do
  a <- A.async act
  C.threadDelay (200*1000)
  p <- A.poll a
  case p of
    Nothing         -> return () -- good, hasn't terminated.
    Just (Left e)   -> error $ "Supposedly-diverging thread threw exception: "++show e
    Just (Right _x) -> error $ "Supposedly-diverging thread returned successfully."
  cancel a
  ) `shouldPrint` str

--------------------------------------------------------------------------------

t1 :: IO Integer
t1 = runDetIO (return 3)

t2 :: IO Integer
t2 = runDetIO (do _ <- new; return 3)

t3 :: IO Integer
t3 = runDetIO (do v <- new; put v 'a'; return 3)

t4 :: IO Char
t4 = runDetIO (do v <- new; put v 'a'; get v)


t5 :: IO ()
t5 = runDetIO (putStrLn "hi")

t6 :: IO ()
t6 = runDetIO (do putStrLn "hi"; putStrLn "there")

t7 :: IO ()
t7 = runDetIO (do _ <- forkIO (putStrLn "hi"); putStrLn "there")

t8 :: IO ()
t8 = runDetIO (do v <- forkIO (putStrLn "b")
                  putStrLn "a"
                  joinThread v
                  putStrLn "c")

t9 :: IO ()
t9 = runDetIO (do v1 <- forkIO (do putStrLn "b1"
                                   v2 <- forkIO (putStrLn "c1")
                                   putStrLn "b2"
                                   joinThread v2)
                  putStrLn "a1"
                  joinThread v1
                  putStrLn "a2")

-- | First-class use of IVars:
t10 :: IO ()
t10 = runDetIO (do v1 <- forkIO (do putStrLn "b1"
                                    v2 <- forkIO (putStrLn "c1")
                                    putStrLn "b2"
                                    return v2)
                   putStrLn "a1"
                   v2 <- joinThread v1
                   putStrLn "a2"
                   joinThread v2
                   putStrLn "a3")

-- | Thread never forced produces no output currently.
t11 :: IO ()
t11 = runDetIO (do v1 <- forkIO (do putStrLn "b1"
                                    v2 <- forkIO (putStrLn "c1")
                                    putStrLn "b2"
                                    return v2)
                   putStrLn "a1"
                   _v2 <- joinThread v1
                   -- get v2
                   putStrLn "a2")

-- | Threads not joinThreaded still produce DIVERGENCE:
t12 :: IO ()
t12 = runDetIO (do v1 <- forkIO (do putStrLn "b1"
                                    v2 <- forkIO (let go = do _ <- new; go
                                                  in go)
                                    putStrLn "b2"
                                    return v2)
                   putStrLn "a1"
                   _v2 <- joinThread v1
                   -- get v2
                   putStrLn "a2")

-- FIXME: test errors also after switching over to Async.
-- This should NOT block forever waiting for an IVar to be written.
t13 :: IO ()
t13 = runDetIO (do v1 <- forkIO (do putStrLn "b1"
                                    v2 <- forkIO (let go = return () >> error "hmm"
                                                  in go)
                                    putStrLn "b2"
                                    return v2)
                   putStrLn "a1"
                   _v2 <- joinThread v1
                   -- get v2
                   putStrLn "a2")

t14 :: IO ()
t14 = runDetIO (do iv1 <- new
                   putStrLn "a1"
                   th1 <- forkIO (do putStrLn "b1"
                                     put iv1 ("hi"::String)
                                     putStrLn "b2"
                                     return ())
                   putStrLn "a2"
                   "hi" <- get iv1
                   putStrLn "a3"
                   joinThread th1
                   putStrLn "a4")

t15 :: IO ()
t15 = runDetIO (do iv1 <- new
                   putStrLn "a1"
                   th1 <- forkIO (do putStrLn "b1"
                                     "hi" <- get iv1
                                     putStrLn "b2"
                                     return ())
                   putStrLn "a2"
                   put iv1 ("hi"::String)
                   putStrLn "a3"
                   joinThread th1
                   putStrLn "a4")

-- | Respect happens before of a nested diamond made of IVar edges.
t16 :: IO ()
t16 = runDetIO (do iv1 <- new
                   iv2 <- new
                   putStrLn "a1"
                   th1 <- forkIO (do putStrLn "b1"
                                     put iv1 ("hi"::String)
                                     putStrLn "b2"
                                     "there" <- get iv2
                                     putStrLn "b3"
                                     return ())
                   putStrLn "a2"
                   "hi" <- get iv1
                   putStrLn "a3"
                   put iv2 ("there"::String)
                   putStrLn "a4"
                   joinThread th1 -- Sync the child thread
                   putStrLn "a5")

-- | Flip the direction of t16
t17 :: IO ()
t17 = runDetIO (do iv1 <- new
                   iv2 <- new
                   putStrLn "a1"
                   th1 <- forkIO (do putStrLn "b1"
                                     "hi" <- get iv1
                                     putStrLn "b2"
                                     put iv2 ("there"::String)
                                     putStrLn "b3"
                                     return ())
                   putStrLn "a2"
                   put iv1 ("hi"::String)
                   putStrLn "a3"
                   "there" <- get iv2
                   putStrLn "a4"
                   joinThread th1 -- Sync the child thread
                   putStrLn "a5")
