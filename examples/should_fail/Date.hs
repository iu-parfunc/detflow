{-# LANGUAGE OverloadedStrings #-}

-- | Date should deterministically print something boring.
module Date (main) where

import Control.Monad.DetIO
import Control.Monad.DetIO.System
import Data.Text (pack)
import Prelude hiding (putStrLn, readFile, writeFile)

main :: DetIO ()
main = do putStrLn "Shelling out"
          sout2 <- readProcess "date" [] ""
          putStrLn "Running touch succeeded, with stdout:"
          putStrLn (pack sout2)
