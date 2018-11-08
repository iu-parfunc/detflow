-- | Run Mothur in parallel on several files.

module RunIt where

import Control.Monad
import Control.Monad.DetIO as D
import Data.Text (pack)
import Prelude hiding (putStrLn, putStr, readFile, writeFile)
import System.FilePath
import qualified Data.ByteString.Char8 as B
    
main :: DetIO ()
main = do
  -- This is lame, but just copy the input to the output:
  -- callCommand "cp in/* out/"
  -- Then we can operate in-place on the output dir.
              
  putStrLn "Getting list of input files:"   
  files <- readFile "in/stability.files"

  let indivs0 = B.lines files
  clargs <- getArgs
  let (indivs,innerJobs) =
          case clargs of
            []    -> (indivs0, 1)
            [n]   -> (take (read n) indivs0, 1)
            [n,t] -> (take (read n) indivs0, read t)
            _ -> error "expects 0-2 command line args: [num-files-to-run] [inner-parallelism]"

  putStrLn $ show (length indivs) ++ " separate files/jobs."
  putStrLn $ "Inner parallelism (threads): "++show innerJobs
               
  -- Split up the input file list:             
  forM_ (zip [1..] indivs) $ \(ix,line) -> do 
    writeFile ("out/stability_"++show ix++".files") line
    createDirectory $ "out" </> show ix

  putStrLn "Output directories created, now calling mothur."  
  rootD  <- makeAbsolute "."
  inDir  <- makeAbsolute "in"
  thrds <- forM (zip [1..] indivs) $ \(ix,_) -> do
   outDir <- makeAbsolute ("out" </> show ix)
   forkWithPerms [ mkPermR inDir, mkPermRW outDir ] $ do
    putStrLn ("Processing file "++show ix++"\n----------------------------------------")
    let cmd = -- "mothur" -- system-wide
              (rootD </> "bin/mothur") -- binary distribution
              -- (rootD </> "mothur-1.39.5/mothur") -- built from source
        args = [ "#set.logfile(name=logfile_"++show ix++"_.txt); "++
                "make.contigs(file=../stability_"++show ix
                ++".files, processors="++show innerJobs
                ++", inputdir=../../in/, outputdir=./"++show ix++")" ]
        prc = (proc cmd args) { cwd = Just outDir }
    
    -- callCreateProcess prc
    _sout <- readCreateProcess prc ""
    putStrLn "Child process finished." 
    -- putStrLn sout
    putStrLn "===== End output from child process." 
    return ()

  -- Example:
  -- mothur "#make.contigs(file=out/stability_1.files, processors=1, inputdir=in/, outputdir=out/1)"

  putStrLn "Main thread now waiting on forked children..."
  _ <- D.joinThreads thrds -- Wait for everything to finish.
  putStrLn "\nAll threads joined."
