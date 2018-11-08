{-# LANGUAGE OverloadedStrings #-}

-- | A strange sort of bash file interpreter.
module Bash2 (main) where

import Control.Monad.DetIO
import Prelude hiding (putStrLn, readFile, writeFile)

main :: DetIO ()
main = do putStrLn $ "Creating a child process running bash -c:"
          putStrLn =<< readProcess "/bin/bash" ["-c", "/usr/bin/env"] ""
                     
          putStrLn $ "Creating a child process with a redirection:" 
          putStrLn =<< readProcess "/bin/bash" ["-c", "cat in/hello.txt > out/hello1.txt"] ""
          putStrLn =<< readShell "cat in/hello.txt > out/hello2.txt" ""
                                   
          putStrLn $ "Succeeded.  Exiting."
