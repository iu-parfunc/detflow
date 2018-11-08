{-# LANGUAGE OverloadedStrings #-}
module CatEtc (main) where
import Control.Monad
import Control.Monad.DetIO
import Control.Monad.DetIO.System
import Data.Text
import Data.Monoid
import Prelude hiding (putStrLn, readFile, writeFile)
import System.Exit

main :: DetIO ()
main = do putStrLn "Shelling in a bad way"
-- Actually temporarily allowing etc/passwd [2017.04.03]:
--          sout <- readProcess "wc" ["/etc/passwd"] ""
          sout <- readProcess "wc" ["/etc/issue"] ""
          putStrLn$ "<stdout>\n"<>pack sout<>"</stdout>"
          putStrLn$ "We should NOT get here!!"
