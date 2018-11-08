{-# LANGUAGE OverloadedStrings #-}
module Makefile where

import qualified Data.ByteString.Char8 as B
import           Data.Makefile
import           Data.Monoid

makefile :: Makefile
makefile = Makefile
  [ Assignment "CC" "gcc"
  , Rule (Target "all")
         [Dependency "main"]
         []
  , Rule (Target "main")
         (map (\n -> Dependency $ "dep" <> B.pack (show n) <> ".o") [1..8::Int])
         [Command "$(CC) $^ -Wall -o $@"]
  --, Rule (Target
  , Rule (Target "clean")
         []
         [Command "-rm -f *.o", Command "-rm -f main"]
  ]
