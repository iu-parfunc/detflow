{-# LANGUAGE OverloadedStrings #-}
module Env (main) where

import Control.Monad.DetIO
import Prelude hiding (putStrLn, readFile, writeFile)

main :: DetIO ()
main = do putStrLn "Shelling out"
          sout <- readProcess "/usr/bin/env" [] ""
--          sout <- readProcess "/bin/bash" ["-c", "/usr/bin/env"] ""
-- [2017.04.05] Child process dies:
--          sout <- readCreateProcess (shell "/usr/bin/env") ""
          putStrLn "Running /usr/bin/env succeeded, with stdout:"
          putStrLn (sout)
