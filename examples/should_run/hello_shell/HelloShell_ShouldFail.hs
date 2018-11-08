{-# LANGUAGE OverloadedStrings #-}
module HelloShell_ShouldFail (main) where

import Control.Monad.DetIO
import Control.Monad.DetIO.System
import Data.Text
import Data.Monoid
import Prelude hiding (putStrLn, readFile, writeFile)
import System.Exit

main :: DetIO ()
main = do putStrLn "Shelling out"
          (ExitSuccess,sout,serr) <- readProcess "cat" ["/etc/issue"] ""
          putStrLn "Shell out succeeded:"
          putStrLn $ "Standard out: " <> pack sout
          putStrLn $ "Standard err: " <> pack serr

          (ret,sout,serr) <- readProcess "./foo.sh" [] ""
          putStrLn $ "Ret: " <> pack (show ret)
          putStrLn $ "Standard out: " <> pack sout
          putStrLn $ "Standard err: " <> pack serr
