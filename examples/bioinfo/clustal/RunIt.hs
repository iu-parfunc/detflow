-- | Run in parallel on several input files.

module RunIt where

import Control.Monad
import Control.Monad.DetIO as D
import Prelude hiding (putStrLn, putStr, readFile, writeFile)
import System.FilePath

main :: DetIO ()
main = do
  putStrLn "Listing input dirs..."
  ins0   <- listDirectory "in"
  clargs <- getArgs
  let ins = case clargs of
              []  -> ins0
              [n] -> take (read n) ins0
              _   -> error "expects 0 or 1 command line args: number of files to run"

  thrds <- forM ins $ \file -> do
    inDir  <- makeAbsolute ("in" </> file)
    outDir <- makeAbsolute "out"
    let inFile  = inDir </> file <.> "fa"
        outFile = outDir </> file <.> "aln"
    forkWithPerms [ mkPermR inFile, mkPermRW outFile ] $ do
      putStr (" "++file)
      let exec = "./bin/clustalw2"
          args = [ "-ALIGN", "-INFILE="++inFile,"-OUTFILE="++outFile ]
      -- putStrLn$ "running command: "++exec ++" "++ unwords args
      _ <- readProcess exec args ""
      return ()

  putStrLn "Main thread now waiting on forked children..."
  _ <- D.joinThreads thrds -- Wait for everything to finish.
  putStrLn "\nAll threads joined."
