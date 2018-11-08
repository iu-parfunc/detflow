{-# LANGUAGE OverloadedStrings #-}
module Touch (main) where

import Control.Monad.DetIO
import Control.Monad.DetIO.System
import Data.Text (pack)
import Prelude hiding (putStrLn, readFile, writeFile)

main :: DetIO ()
main = do putStrLn "Shelling out"
          sout2 <- readProcess "touch" ["out/blah.txt"] ""
          putStrLn "Running touch succeeded, with stdout:"
          putStrLn (pack sout2)
