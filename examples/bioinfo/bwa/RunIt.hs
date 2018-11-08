-- | Run "bwa mem" in parallel on several files.

module RunIt where

import Control.Monad
import Control.Monad.DetIO as D
import Control.Monad.DetIO.Unsafe

import Prelude hiding (putStrLn, putStr, readFile, writeFile)
import System.FilePath

internalParallelism :: String
internalParallelism = "-t 1"

main :: DetIO ()
main = do
  -- This is lame, but just copy the input to the output:
  -- callCommand "cp in/* out/"
  -- Then we can operate in-place on the output dir.
              
  putStrLn "Building a list of input files:" 
  let files0 = [ ( "GSF1344N-F10-145_S62_L001_R1_001.part-"++ix++".fastq.gz"
                 , "GSF1344N-F10-145_S62_L001_R1_001.part-"++ix++".fastq.gz")
               | ix <- map (zeroPad 2 . show) [1..32] ]
  clargs <- getArgs
  let files = case clargs of
                []  -> files0
                [n] -> take (read n) files0
                _   -> error "expects 0 or 1 command line args: number of files to run"
  putStrLn $ show (length files) ++ " separate files/jobs."

  inDir  <- makeAbsolute "in"
  thrds <- forM (zip [1..] files) $ \(ixN,(file1,file2)) -> do
   let ix = zeroPad 2 (show ixN)
   outP <- makeAbsolute ("out" </> ix <.> "sam")
   forkWithPerms [ mkPermR inDir, mkPermRW outP
                 -- , mkPermR "/dev/shm/bwactl"
                 ] $ do
    putStrLn $ "Processing file "++ix -- ++"\n----------------------------------------"
    let prc = (shell$ -- "../bwa-0.7.15/bwa" ++
                      -- "../bwa-0.7.9/bwa" ++
                      "../bin/bwa"++
                      " mem "++internalParallelism++
                      " wMel.fa "++file1++" "++file2++" > ../out/"++ix++".sam"
              -- [ "", ".fa", file1, file2 ]
              )
              { cwd = Just inDir }
    _sout <- readCreateProcess prc ""
    -- putStrLn sout
    return ()

  putStrLn "Main thread now waiting on forked children..."
  D.joinThreads thrds -- Wait for everything to finish.
  putStrLn "\nAll threads joined."

zeroPad :: Int -> [Char] -> [Char]
zeroPad n ls | length ls < n = replicate (n - length ls) '0' ++ ls
             | otherwise     = ls 
