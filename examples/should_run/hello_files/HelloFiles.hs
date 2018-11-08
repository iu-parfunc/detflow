{-# LANGUAGE OverloadedStrings #-}
module HelloFiles (main) where

import Control.Monad.DetIO
import Prelude hiding (putStrLn, readFile, writeFile)

main :: DetIO ()
main = do putStrLn "Reading file, in/hello.txt"
          str <- readFile "in/hello.txt"
          putStrLn "File read!  Now writing."
          writeFile "out/hello.txt" str
          putStrLn "Write successful!  Exiting."
