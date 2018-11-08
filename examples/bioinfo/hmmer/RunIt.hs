-- | Run "bwa mem" in parallel on several files.

module RunIt where

import Control.Monad
import Control.Monad.DetIO as D
import Control.Monad.DetIO.Unsafe

import Prelude hiding (putStrLn, putStr, readFile, writeFile)
import System.FilePath
main :: DetIO ()
main = do
  -- This is lame, but just copy the input to the output:
  -- callCommand "cp in/* out/"
  -- Then we can operate in-place on the output dir.

  putStrLn "Listing input dirs..."
  ins0 <- listDirectory "in"
  clargs <- getArgs
  let ins = case clargs of
              []  -> ins0
              [n] -> take (read n) ins0
              _   -> error "expects 0 or 1 command line args: number of files to run"

  thrds <- forM ins $ \file -> do
    inDir  <- makeAbsolute ("in" </> file)
    outDir <- makeAbsolute "out"
    let inFile  = inDir </> file <.> "fa"
        outFile = outDir </> file 
    forkWithPerms [ mkPermR inDir, mkPermRW outFile ] $ do
      putStr (" "++file)
      let exec = "./bin/hmmbuild"
          args = [ "--cpu","1", outFile, inFile ]
      putStrLn$ "running command: "++exec ++" "++ unwords args
      _ <- readProcess exec args ""
      return ()

  putStrLn "Main thread now waiting on forked children..."
  D.joinThreads thrds -- Wait for everything to finish.
  putStrLn "\nAll threads joined."

