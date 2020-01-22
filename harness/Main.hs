module Main (main) where

import           Control.Concurrent.Async
import           Control.Monad (when, forM, unless)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.ByteString.Char8 as BS
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process
import qualified Data.List as L

import           Options.Applicative

import           Prelude ()
import           Prelude.Compat

import           System.Clock
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.IO.Temp
import           System.Directory

data Args = Args
  { repeat       :: Maybe Word -- FIXME - use this.
  , keepTmpFiles :: Bool
  , compile      :: Bool
  , compileArgs  :: String
  , rtsArgs      :: String
  , time         :: Bool
  , inputDir     :: [FilePath]
  , outputDir    :: Maybe FilePath
  , runShellCmd  :: Bool
  , detMode      :: DetMode
  , jobs         :: Maybe Word
  , thingToRun   :: FilePath -- For now, this is a file. TODO: Require it be a library
  , exeArgs      :: [String]
  } deriving Show

-- | How to invoke the job at runtime, what kind of determinism enforcement and how much?
data DetMode = PreloadLibDet { _useASLR :: Bool }
               -- ^ The default.
             | NonDet   -- ^ Run with no enforcement, should be fastest.
             | RR_Record -- ^ Run with Mozilla rr as a point of comparison, record trace.
             | RR_Chaos  -- ^ Run with rr, record trace, and use chaos mode.
             | RR_Replay -- ^ Replay with rr' default settings.
  deriving (Eq,Ord,Show,Read)

parseStringList :: Monad m => String -> m [String]
parseStringList = return . words

multiString :: Mod OptionFields [String] -> Parser [String]
multiString desc = concat <$> many single
  where single = option (str >>= parseStringList) desc

argsParser :: Parser Args
argsParser
  = Args
  <$> option (Just <$> auto)
      (  long "repeat"
      <> value Nothing
      <> metavar "N"
      <> help "Repeat execution N times for testing purposes.")
  <*> switch
      (  long "keep-tmp-files"
      <> help "Don't delete temporary files after finishing")
  <*> switch
      (  long "compile"
      <> short 'c'
      <> help "Compile the target file (instead of using runghc)")
  <*> strOption
      (  metavar "ARGS"
      <> value "-O2 -threaded"
      <> long "compile-args"
      <> help "A string of extra arguments to pass to GHC iff in --compile mode")
  <*> strOption
      (  metavar "ARGS"
      <> value "+RTS -s -RTS"
      <> long  "rts-args"
      <> help ("A string of extra arguments to pass to the deterministic"
              ++" program at runtime (only in --compile mode)"))
  <*> switch
      (  long "time"
      <> short 't'
      <> help "Time the execution of running the program")
  -- <*> fmap catMaybes
  --     (many (option (Just <$> str)
  --           (  metavar "INPUT_DIR"
  --           <> value Nothing
  --           <> short 'i'
  --           <> long "input"
  --           <> help "Input directory")))

  <*> (multiString
      (short 'i'
            <> long "input"
            <> help "Input directory"))

  <*> option (Just <$> str)
      (  metavar "OUTPUT_DIR"
      <> value Nothing
      <> short 'o'
      <> long "output"
      <> help "Output directory")
  <*> switch
      (  long "runshell"
      <> short 'r'
      <> help "Run a shell command directly instead of a Haskell file")

  <*> (flag' (PreloadLibDet True) (long "unsafe-ASLR" <>
                                   help "Use address space randomization, which is a source of nondetermiism.") <|>
       flag' NonDet (long "nondet" <>
                     help "Maximum performance, but no determinism enforcement at all.") <|>
       flag' RR_Record (long "rr-record" <>
                        help "Run the workload under Mozilla's 'rr record'.  Records to the default trace dir.") <|>
       flag' RR_Record (long "rr-chaos" <>
                        help "Use 'rr record -- chaos' to help find bugs.") <|>
       flag' RR_Replay (long "rr-replay" <>
                        help "After running with --rr-record, this will run with 'rr replay -a'") <|>
       flag (PreloadLibDet False) (PreloadLibDet False)
            (long "det" <>
             help "Run with determinism enforcement via libdet (default)")
      )

  <*> option (Just <$> auto)
      (  long "jobs"
      <> short 'j'
      <> value Nothing
      <> metavar "N"
      <> help "Allow N jobs at once" )
  <*> strArgument (metavar "<File.hs>")
  <*> (many . strArgument)
      (metavar "exe-args...")

main :: IO ()
main = do
    args <- execParser opts
    detflowDriver args
  where
    opts = info (helper <*> argsParser)
      ( fullDesc
      <> header "detflow" )

detflowDriver :: Args -> IO ()
detflowDriver args = do

  inDs <- forM (inputDir args) $ \x ->
             do createDirectoryIfMissing True x
                x' <- fmap dropTrailingPathSeparator $ makeAbsolute x
                xc <- canonicalizePath x
                when (xc /= x') $
                   error $ "\nInput directory must contain symlinks!  Absolute and canonicalized paths differ."
                          ++"\nRequested directory:     "++x'
                          ++"\nCanonicalized directory: "++xc
                return x'

  outD' <- case outputDir args of
            Just x -> do createDirectoryIfMissing True x
                         x' <- fmap dropTrailingPathSeparator $ makeAbsolute x
                         xc <- canonicalizePath x
                         when (xc /= x') $
                           error $ "\nOutput directory must contain symlinks!  Absolute and canonicalized paths differ."
                                   ++"\nRequested directory:     "++x'
                                   ++"\nCanonicalized directory: "++xc

                         return (Just x')
            Nothing -> return Nothing
  -- TODO: assert that inD outD are directories for now... and that they exist.
  createAndRunWrapper args inDs outD'


createAndRunWrapper :: Args -> [FilePath] -> Maybe FilePath -> IO a
createAndRunWrapper args inDs outD' =
  let keepTmps = keepTmpFiles args
      file     = thingToRun args
      fileDir       = takeDirectory file
      ioMainModName = takeBaseName file
  in
  withSystemTempDirectory "detflow_tmprun" $ \ tmpdir ->
  -- Put the generated script in the current directory so that it can import the target script:
   withTempFile' keepTmps fileDir "DetMainXXX.hs" $ \fp h -> do
    unless (detMode args == RR_Replay) $
      hPutStrLn h $ detMainFile (detMode args) (jobs args) inDs outD'
                    (if runShellCmd args
                     then Right (file++" "++unwords (exeArgs args))
                     else Left ioMainModName)
    hClose h
    let detMainModName = takeBaseName fp

        ghcCmd = unwords $ [ if compile args
                             then "ghc "++compileArgs args
                             else "runghc"
                           , "-main-is" , ghcArg args "DetMain.detMain"
                           , "-i" ++ fileDir
                           , fp
                           ] ++ if compile args then []
                                else exeArgs args ++ [rtsArgs args]
        runCmd | compile args = unwords $ ["./" ++ detMainModName]
                                      ++ exeArgs args ++ [rtsArgs args]
               | otherwise    = ghcCmd
        cleanupCmd = "rm -f " ++ detMainModName ++ ".o "
                              ++ detMainModName ++ ".hi "
                              ++ detMainModName
        fullCmd | detMode args == RR_Replay = maybeRR ghcCmd -- NO Compile!
                | compile args              =  "( " ++ ghcCmd ++ " ; " ++ maybeRR runCmd ++ " )"
                | otherwise                 = maybeRR ghcCmd

        maybeRR cmd = case detMode args of
                        RR_Record -> "rr record "++cmd
                        RR_Chaos  -> "rr record --chaos "++cmd
                        RR_Replay -> "rr replay -a" -- FIXME: SPECIFY TRACE DIR
                                     -- This would be trivial to run by hand, we keep it here just to
                                     -- reuse the timing/logging setup.
                        _         -> cmd

        runCmdProc = shell fullCmd
        teeStdoutFile = tmpdir </> "stdout"
        teeStderrFile = tmpdir </> "stderr"
        sink = CL.mapM_ $ liftIO . BS.putStr
    withFile   teeStdoutFile AppendMode $ \teeOut ->
      withFile teeStderrFile AppendMode $ \teeErr -> do
        when (compile args && detMode args /= RR_Replay) $ do
          (_,_,_,phGhcCmd) <- createProcess (shell ghcCmd)
          ec <- waitForProcess phGhcCmd
          case ec of
            ExitSuccess -> return ()
            ExitFailure n -> error $ "Compiling failed with " ++ show n

        startTime <- getTime Monotonic
        (ClosedStream, out, err, procH) <- streamingProcess runCmdProc
        ec <- runConcurrently $
                  Concurrently (runResourceT $ runConduit
                                             $ out .| conduitHandle teeOut .| sink)
               *> Concurrently (runResourceT $ runConduit
                                             $ err .| conduitHandle teeErr .| sink)
               *> Concurrently (waitForStreamingProcess procH)
        endTime <- getTime Monotonic

        -- let StreamingProcessHandle _ tmv _ = procH
        -- cde <- getProcessExitCode procH
        -- cde <- atomically $ readTMVar tmv
        -- cde <- waitForStreamingProcess procH
        case ec of
          ExitSuccess -> return ()
          (ExitFailure n) -> error $ " detflow: job exited with code "++show n

        -- In seconds
        let duration :: Double
            duration = (fromIntegral $ toNanoSecs endTime - toNanoSecs startTime) /
                       (10 ^ (9 :: Int))
        when (time args) $ do
          putStrLn $ "COMPILE_FLAGS: "++compileArgs args
          putStrLn $ "RUNTIME_FLAGS: "++rtsArgs args
          putStrLn $ "SELFTIMED: " ++ show duration
          ghcVer <- readProcess "ghc" ["--version"] ""
          putStrLn $ "COMPILER: "++L.head (L.lines ghcVer)

        when (compile args && not (keepTmpFiles args)) $
          callCommand cleanupCmd
        exitSuccess
        -- exitWith ec -- Exit the whole process.

    -- TODO: Before they are deleted, hash the captured stdout/stderr

withTempFile' :: Bool -> FilePath -> String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile' keepTmps fp s f
  | keepTmps  = do (fp', h) <- openTempFile fp s
                   f fp' h
  | otherwise = withTempFile fp s f

ghcArg :: Args -> String -> String
ghcArg args
  | compile args = id
  | otherwise    = ("--ghc-arg=" ++)

-- | Generate a wrapper file that imports the users file.
detMainFile :: DetMode-> Maybe Word -> [FilePath] -> Maybe FilePath -> (Either String String) -> String
detMainFile detmode mjobs inDs moutD eithstr = unlines
  [ "module DetMain (detMain) where"
  , case eithstr of
      Left ioMainModName -> "import " ++ ioMainModName ++ " (main)"
      Right _ -> ""
  , "import Control.Monad.DetIO "
  , "import Control.Monad.DetIO.Perms "
  , "import Control.Monad.DetIO.Unsafe (allowASLR, fullNonDet) "
--  , "import Algebra.Lattice "
  , "detMain :: IO ()"
  , "detMain = do"
--  , "  perms <- getInitPermsFromHarness" -- TODO: Write this

 -- TODO: Sanitize and convert to absolute directories!!

  , "  runDetIOWith ("++show mjobs++") ("++inP++" \\/ "++outP++") "
    ++ maybeChangeMode
       (case eithstr of
         Left _    -> "main"
         Right cmd -> "(callCommand "++show cmd++")")
  ]
 where
   maybeChangeMode act =
       case detmode of
         PreloadLibDet True  -> "(allowASLR >> "++act++")"
         PreloadLibDet False -> act
         NonDet              -> "(fullNonDet >> "++act++")"
         RR_Record           -> -- act -- Nothing to do here inside the Haskell code. rr is outside.
                                "(allowASLR >> "++act++")" -- On second thought, if we're nondet anyway, why mess around
                                                           -- with the extra setarch wrapper...
         RR_Chaos            -> "(allowASLR >> "++act++")" -- disabling ASLR seems to conflict with chaos mode.
         RR_Replay           -> "error_should_not_actually_use_this_code"

   -- Optional input and output permissions.  Currently restricted to one each:
   inP = case inDs of
           [] -> "IP emptyPerms"
           _  -> concat $ L.intersperse " \\/ "
                 [ "fromPathPerm (mkPermR "++show x++")" | x <- inDs ]
   outP = case moutD of
            Just x  -> "fromPathPerm (mkPermRW "++show x++")"
            Nothing -> "IP emptyPerms"

