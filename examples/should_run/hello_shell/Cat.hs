{-# LANGUAGE OverloadedStrings #-}
module Cat (main) where

import Control.Monad.DetIO
import Prelude hiding (putStrLn, readFile, writeFile)

main :: DetIO ()
main = do putStrLn "Shelling out"
          sout <- readProcess "cat" ["in/hello.txt"] ""
--          sout <- readCreateProcess (shell "cat in/hello.txt") ""
          putStrLn "Running cat succeeded, with stdout:"
          putStrLn sout
