{-# LANGUAGE OverloadedStrings #-}

-- | A strange sort of bash file interpreter.
module Bash (main) where

import Control.Monad.DetIO
import Control.Monad.DetIO.System
import Data.Text (pack)
import Data.Monoid
import Prelude hiding (putStrLn, readFile, writeFile, getArgs)

main :: DetIO ()
main = do 
          args <- getArgs
          case args of
            [one] -> do putStrLn $ "Shelling out to a bash script: " ++ one
                        sout <- readProcess one [] ""
                        putStrLn $ "bash script call succeeded, producing stdout:" ++ sout
                        putStrLn sout
            oth -> error$ "Expected a name of a bash script as argument, got: "++show oth
