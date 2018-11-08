{-# LANGUAGE OverloadedStrings #-}
module HelloWorld (main) where

import Control.Monad.DetIO
import Prelude hiding (putStrLn)

main :: DetIO ()
main = do t <- forkIO $ putStrLn "world!"
          putStrLn "Hello "
          joinThread t
