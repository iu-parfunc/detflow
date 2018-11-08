-- | 

module RunIt where

import Control.Monad
import Control.Monad.DetIO as D
import Data.Text (pack)
import Prelude hiding (putStrLn, putStr, readFile, writeFile)
import System.FilePath
import  System.Process as Proc


main :: DetIO ()
main = do  
  putStrLn "Listing input dirs..."
  ins0 <- listDirectory "in"
  clargs <- getArgs
  let ins = case clargs of
              []  -> ins0
              [n] -> take (read n) ins0
              _   -> error "expects 0 or 1 command line args: number of files to run"
  putStr "Populating output dirs..."
  forM_ ins $ \nm -> createDirectoryIfMissing False ("./out" </> nm)
  putStrLn " done."

  -- This strange program MUTATES the input directory.
  thrds <- forM ins $ \file -> do
    inDir  <- makeAbsolute ("in" </> file)
    let inFile = inDir </> file <.> "fa"
    outDir <- makeAbsolute ("out" </> file)

    forkWithPerms [ mkPermRW inDir, mkPermRW outDir ] $ do
      putStr (" "++file)
      let exec = "./bin/raxml"
          args = words ("-m GTRGAMMA -p 12345 "++
                        " -s "++ inFile ++ -- in/"++file++"/"++file++".fa "++
                        " -w "++outDir++" -n "++file)
          _cmd = exec ++" "++ unwords args
      -- putStrLn _cmd
      -- _ <- readProcess exec args ""
      -- callProcess exec args
      _ <- D.readCreateProcess (Proc.proc exec args)
                               { std_out= NoStream
                               , std_err= NoStream } ""
      return ()

  putStrLn "Main thread now waiting on forked children..."
  D.joinThreads thrds -- Wait for everything to finish.
  putStrLn "\nAll threads joined."
                   
--          sout <- detsystem "cat" ["in/hello.txt"] ""
--          putStrLn "Running cat succeeded, with stdout:"
--          putStrLn sout


