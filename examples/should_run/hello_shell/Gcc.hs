{-# LANGUAGE OverloadedStrings #-}
module Gcc (main) where

import Control.Monad.DetIO
import Prelude hiding (putStrLn, readFile, writeFile)
-- import System.IO.Process
    
main :: DetIO ()
main = do putStrLn "Shelling out"
--           sout <- readShell "gcc -print-prog-name=cc1" "" -- Not working yet.
          runEcho "gcc" ["-print-prog-name=cc1"]
          runEcho "gcc" ["--version"] 
          runEcho "which" ["gcc"] 

--          runEcho "gcc" ["-B/usr/lib/gcc/x86_64-linux-gnu/6/", "-print-prog-name=cc1"]

runEcho :: String -> [String] -> DetIO ()
runEcho cmd args = do
  putStrLn$ "Running: "++ cmd++" "++unwords args
  putStrLn =<< readCreateProcess (proc cmd args) ""
-- [2017.04.05] PROBLEMS:
--  putStrLn =<< readCreateProcess (shell (cmd++" "++unwords args)) ""

-- [2017.04.04] Cannot run strace under libdet:
--           sout <- readProcess "strace" ["gcc", "-print-prog-name=cc1"] ""

