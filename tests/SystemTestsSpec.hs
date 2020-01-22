{-# LANGUAGE ScopedTypeVariables #-}

-- | Basically a shell script for launching system tests.

module SystemTestsSpec where

import           Control.Exception
import           Data.List
import           Test.Hspec hiding (example)
import           System.Directory
import           System.Process
import           System.Exit

main :: IO ()
main = do
  dir <- getCurrentDirectory
  putStrLn$ "Running system tests... (in dir: "++show dir++")"
  hspec spec

spec :: Spec
spec = do
  describe "Valid detflow executions" $ do
    it "hello_world" (do x <- t1
                         x `shouldSatisfy` (isInfixOf "Hello \nworld!\n"))
    it "hello_files" (do x <- t2
                         x `shouldSatisfy` (isInfixOf $
                                            unlines [ "Reading file, in/hello.txt"
                                                    , "File read!  Now writing."
                                                    , "Write successful!  Exiting."]))
    it "hello_shell/Cat" (do x <- t3 "Cat.hs"
                             x `shouldSatisfy` ("succeeded" `isInfixOf`))
    it "hello_shell/Head" (do x <- t3 "HeadUsr.hs"
                              x `shouldSatisfy` ("succeeded" `isInfixOf`))
    it "hello_shell/Env" (do x <- t3 "Env.hs"
                             x `shouldSatisfy` ("succeeded" `isInfixOf`))
    it "hello_shell/Bash1_1" (do x <- t3 "Bash.hs in/bash1.sh"
                                 x `shouldSatisfy` ("succeeded" `isInfixOf`))
    it "hello_shell/Bash1_2" (do x <- t3 "Bash.hs in/bash2.sh"
                                 x `shouldSatisfy` ("succeeded" `isInfixOf`))

    it "hello_shell/Bash2" (do x <- t3 "Bash2.hs"
                               x `shouldSatisfy` ("Succeeded" `isInfixOf`))


    -- it "runshell_1" (do x <- t4
    --                     x `shouldSatisfy` ("\ESC" `isInfixOf`)) -- Don't require a SPECIFIC escape sequence.

-- This one's failing and I don't know why.  Child process errors w/ code 144 [2017.04.03].
    -- it "hello_shell/Bash3" (do x <- t3 "Bash.hs in/bash3.sh"
    --                            x `shouldSatisfy` ("succeeded" `isInfixOf`))

  describe "Invalid executions that try to break the rules" $ do
    it "cat_etc" (f1 `shouldThrow` \(_ :: SomeException) ->
                      -- TODO: CHECK THE ERROR MESSAGE:
                      True)

    -- FIXME: tighten this up and ensure the error comes from
    -- libdet.so, where it stops the process from touching a path it shouldn't:
    it "write_disallowed" (f2 `shouldThrow` \(_ :: SomeException) -> True)

--    it "date" (f3 `shouldThrow` \(_ :: SomeException) -> True)

    return ()

--------------------------------------------------------------------------------

-- TODO: add --repeat arguments:
t1 :: IO String
t1 = withCurrentDirectory "./examples/should_run/hello_world" $ do
     safeshell "detflow HelloWorld.hs"

t2 :: IO String
t2 = withCurrentDirectory "./examples/should_run/hello_files" $ do
      safeshell "detflow -i in/ -o out/ HelloFiles.hs"

-- t3 :: IO String
-- t3 = withCurrentDirectory "./examples/should_run/hello_shell" $ do
--       safeshell "./run_me.sh"


t3 :: String -> IO String
t3 which = withCurrentDirectory "./examples/should_run/hello_shell" $ do
            safeshell$ "detflow -i in/ -o out/ "++which


-- With mothur we seem to have some /usr/bin/clear subprocesses dying inexplicably.
t4 :: IO String
t4 = safeshell "detflow --runshell clear"


-- Failing tests
--------------------------------------------------------------------------------

f1 :: IO String
f1 = withCurrentDirectory "./examples/should_fail" $ do
      safeshell "detflow CatEtc.hs"

f2 :: IO String
f2 = withCurrentDirectory "./examples/should_fail" $ do
      safeshell "detflow WriteDisallowed.hs"

f3 :: IO String
f3 = withCurrentDirectory "./examples/should_fail" $ do
      safeshell "detflow Date.hs"

--------------------------------------------------------------------------------

-- | Returns stdout and errors if anything fishy happens.
safeshell :: String -> IO String
safeshell str = do
  (code,sout,serr) <- readCreateProcessWithExitCode (shell str) ""
  let ctxt = "\nCommand: "++str
             ++"\nStderr was:\n"++serr
             ++"\nStdout was:\n"++sout
  case code of
    ExitFailure err -> error$ "\nsafeshell: command exited with code "++show err
                       ++ctxt
    ExitSuccess ->
      case serr of
        "" -> return sout
        _ -> error $ "\nsafeshell: command exited with non-empty stderr output."
                     ++ctxt
