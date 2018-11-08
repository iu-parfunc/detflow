{-# LANGUAGE OverloadedStrings #-}
module WriteDisallowed (main) where
import Control.Monad
import Control.Monad.DetIO
import Control.Monad.DetIO.System
import Data.Text
import Data.Monoid
import Prelude hiding (putStrLn, readFile, writeFile)
import System.Exit

main :: DetIO ()
main = do putStrLn "Shelling in a bad way"
          sout <- readProcess "touch" ["/naughty/files"] ""
          putStrLn$ "<stdout>\n"<>pack sout<>"</stdout>"
          putStrLn$ "We should NOT get here!!"
