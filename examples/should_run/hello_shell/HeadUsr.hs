{-# LANGUAGE OverloadedStrings #-}
module HeadUsr (main) where

import Control.Monad.DetIO
import Control.Monad.DetIO.System
import Data.Text (pack)
import Prelude hiding (putStrLn, readFile, writeFile)

-- | Since we allow read access to /usr, this should work.
main :: DetIO ()
main = do putStrLn "Shelling out"
          sout <- readProcess "head" ["/usr/include/stdio.h"] ""
          putStrLn "Running head succeeded, with stdout:"
          putStrLn sout
