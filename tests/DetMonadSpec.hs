{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DetMonadSpec
    ( main, spec
    , t1, t2, t3
    ) where

import           Data.ByteString.Char8
import           Control.Exception
import           Control.Monad.DetIO as Det
import           Control.Monad.DetIO.Perms

import           Test.Hspec
import           System.IO.Temp
import           System.IO
import           System.FilePath

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Reading/Writing without permission" $ do
    it "read should fail"  $ t1 `shouldThrow` \(_ :: SomeException) -> True
    it "write should fail" $ t2 `shouldThrow` \(_ :: SomeException) -> True

  describe "Writing with permission" $
    it "should work" $ t3 `shouldReturn` "hello"

  describe "Illegal uses of readProcess" $ do
    it "& should fail" $
      runDetIO (readProcess "blah" ["&"] "")
        `shouldThrow` \(_ :: SomeException) -> True
    it "| should fail" $
      runDetIO (readProcess "blah" ["|", "blargh"] "")
        `shouldThrow` \(_ :: SomeException) -> True

t1 :: IO ByteString
t1 = runDetIO (Det.readFile "donthaveme")

t2 :: IO ()
t2 = runDetIOWith Nothing (fromPathPerm (mkPermRW "/tmp/a"))
                  (do Det.writeFile "/tmp/b" "this should not be here"
                      Det.putStrLn "!!!!!!! Should not reach here... !!!!!")

t3 :: IO ByteString
t3 = withSystemTempFile "ok" $ \tmpfile hndl -> do
      hClose hndl
      (runDetIOWith Nothing
          -- (fromPathPerm (mkPermRW tmpfile)) -- Just the file.
          (fromPathPerm (mkPermRW (takeDirectory tmpfile))) -- Parent dir.
                   (do Det.writeFile tmpfile "hello"
                       Det.readFile tmpfile
                       ))
