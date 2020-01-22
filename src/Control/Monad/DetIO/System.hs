{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
module Control.Monad.DetIO.System
    ( -- * Drop-in replacements to System.Process
      readProcess
    , callProcess
    , callCommand
    , system
    , rawSystem
    , readCreateProcess

     -- * Reexports
    , Proc.proc
    , Proc.shell
    , Proc.CreateProcess(..)

     -- * Extra entrypoints /not/ in System.Process
    , readShell, callCreateProcess

     -- * Internal
    , getWhitelist
    )
 where

import Control.Monad.DetIO.Logging (glog,dbgLvl)
import Control.Monad.DetIO.Perms
import Control.Monad.DetIO.Unsafe as U
import qualified Control.Monad.Fail as Fail
import Control.Monad.State as S
import Control.Concurrent.Chan
import Control.Exception as E

import Data.List as L
import Data.List.Extra (trim, unsnoc)
import Data.Maybe
import qualified Data.Map as M

import Paths_detmonad (getDataDir, getDataFileName)

import qualified System.Directory as D
import System.IO.Temp
import System.Environment hiding (setEnv, unsetEnv)
import System.Exit
import System.FilePath ((</>))
import System.IO
import qualified System.Process as Proc
import Prelude as P

import qualified System.Log.TSLogger as L
--------------------------------------------------------------------------------

getWhitelist :: IO [FilePath]
getWhitelist = do
  iow <- ioWhitelist
  pure $ whitelist ++ iow

-- | Paths that the subprocess may always read.
whitelist :: [FilePath]
whitelist = [ "/usr"
            , "/bin"
            , "/lib"
            , "/dev/urandom" -- We replace this inside the container.
            , "/dev/random"  -- Same here.
            , "/dev/tty"
            , "/dev/stdin"
            , "/dev/stdout"
            , "/dev/stderr" -- Except we may decide non-empty stderr is an error.
            , "/etc/passwd" -- UGH, bash reads this.. should remove.
                            -- normally deterministic under docker, but stack --docker
                            -- screws it up.
            , "/etc/terminfo" -- for /usr/bin/clear

            , "/proc/self/maps" -- Should remove.  Though is this deterministic without ASLR?
            , "/proc/filesystems" -- used by /bin/ls
            ]

ioWhitelist :: IO [FilePath]
ioWhitelist = do
  dataDir  <- getDataDir
  cDataDir <- D.canonicalizePath dataDir
  pure [ cDataDir
       , cDataDir </> "cbits"
       ]

withLease :: Maybe (Chan Word) -> IO a -> IO a
withLease Nothing act = act
withLease (Just chan) act = do
  E.bracket
       (do x <- readChan chan
           L.logStrLn glog 5 $ " acquired lease to execute shell-out: "++show x
           return x)
       (\x ->
        do L.logStrLn glog 5 $ " returning lease to execute shell-out: "++show x
           writeChan chan x)
    (\_ -> act)


-- | A deterministic version of "System.Process.readProcess".
--
-- Run a subprocess deterministically.  This is the key ability
-- granted by the 'DetIO' monad, but it carries some significant
-- restrictions.  The child process may not exit with an error, and
-- the child process may not produce output on stderr (unless in debug
-- mode, in which case the stderr output should contain debug chatter
-- that is passed onward to the stderr of the current process).
--
-- Detsystem takes an executable and its arguments, followed by a
-- string to feed to the standard input of the child process.
--
-- The return value of readProcess is the full contents of the standard
-- output of the child process.  `readProcess` is synchronous, so it
-- does not return until the child process is finished.
--
-- The
readProcess :: FilePath -> [String] -> String -> DetIO String
readProcess f ls = readCreateProcess (Proc.proc f ls)

-- | Like readProcess, except executes an arbitrary shell command.
readShell :: String -> String -> DetIO String
readShell str sin = do
  readCreateProcess (Proc.shell str) sin

-- | Analogous to System.Process.callProcess
callProcess :: String -> [String] -> DetIO ()
callProcess f ls = callCreateProcess (Proc.proc f ls)

-- | Run a program but ignore its stdout and stderr.  They go into the void.
_voidProcess :: String -> [String] -> DetIO ()
_voidProcess _f _ls =
  undefined
--   readCreateProcess (Proc.proc f ls) { }
  -- TS{perms,myped,shellLeases,execMode} <- DetIO S.get -- TODO use myped to name tmpdir!!

  -- case execMode of
  --   -- Just run natively:
  --   NonDet -> lio $ withLease shellLeases $
  --             Proc.readCreateProcess cp0 input


  --   undefined

-- | Analogous to System.Process.callProcess
callCommand :: String -> DetIO ()
callCommand str = callCreateProcess (Proc.shell str)

callCreateProcess :: Proc.CreateProcess -> DetIO ()
callCreateProcess cp =
  if dbgLvl >= printImmediatelyDebugLvl
  then do "" <- readCreateProcess cp{ Proc.std_out = Proc.Inherit } ""
          return ()
  else do so <- readCreateProcess cp ""
          U.putStrLn so


-- | Always returns ExitSuccess (or throws an error).  The type here
-- is for compatibility with System.Process.system.
system :: String -> DetIO ExitCode
system s = callCommand s >> return ExitSuccess

-- | Always returns ExitSuccess (or throws an error).  The type here
-- is for compatibility with System.Process.system.
rawSystem :: String -> [String] -> DetIO ExitCode
rawSystem f ls = callProcess f ls >> return ExitSuccess

-- | ReadCreateProcess that sets up the environment and executes the
--   subprocess.  Here we apply some modifications to the environment
--   that the user has requested for the subprocess.  We also apply
--   sanitization checks, and throw an exception if they fail.
readCreateProcess :: Proc.CreateProcess -> String -> DetIO String
readCreateProcess cp0 input = do
  TS{perms,shellLeases,execMode} <- DetIO S.get -- TODO use myped to name tmpdir!!

  case execMode of
    -- Just run natively:
    NonDet -> lio $ withLease shellLeases $
              Proc.readCreateProcess cp0 input
    _ -> do
     cp1 <- sanitizeCP cp0
     let cp2 = case execMode of
                 Det       -> wrapSetArch cp1
                 AllowASLR -> cp1
                 NonDet    -> error "System.hs/readCreateProcess: internal error"
     let cp = if L.dbgLvl >= printImmediatelyDebugLvl
              then cp2 { Proc.std_err = Proc.Inherit } -- Echo immediately.
              else cp2

     let (rs, rws) = partitionPerms perms
         colonify  = intercalate ":"

     parentEnv <- fmap M.fromList $
                  lio$ getEnvironment

   --  D.putStrLn $ "\nPassing through parent env ("++show(length parentEnv)++"): "++unlines (L.map show parentEnv)

     -- TODO: use pedigree ONLY instead of system random names:
     liftIOToDet $
       withLease shellLeases $
       withSystemTempDirectory "pedigree_here" $ \ tmpdir -> do
           libdet <- getDataFileName "cbits/libdet.so"
           wl     <- getWhitelist

           -- HACKY HACK:
           b <- D.doesFileExist "/etc/pregen_random"
           let randsource = if b then "/etc/pregen_random" else "/dev/urandom"

           -- TODO: may need extra standard things in here.
           let env' = M.toList $ M.fromList
                      [ -- Here we make R include the RW perms for convenience.  Only one list to check.
                        ("DETIO_R_PERMS_LIST", colonify (tmpdir:rs++rws++wl))
                      , ("DETIO_W_PERMS_LIST", colonify (tmpdir:rws))
                      -- , ("DETIO_URAND_PATH", "/etc/pregen_random") -- No special file here yet...
                      , ("DETIO_URAND_PATH", randsource) -- Just use the normal path for now.
                       -- TODO: set TMPDIR to a private RW space for this job:
                      , ("TMPDIR",tmpdir)
                      , ("LD_PRELOAD", libdet)
                      , ("TERM","xterm") -- Choice: let this be a source of nondeterminism? No. Constant.

                      ----------------- Option 1: MINIMAL fraction of parent env ----------------------
                      -- , ("PATH", parentEnv M.! "PATH")
                      ]
                      ----------------- Option 2: inherit ALL of parent env ------------------------
                      -- `M.union` parentEnv
                      ----------------- Option 3: let the user control the env ----------------------
                      -- Here we DIFFER from the Proc.readCreateProcess policy.  We don't pass through
                      -- the whole environment by DEFAULT.  Rather, a minimal environment.  If you want
                      -- more you have to ask for it explicitly.
                      `M.union` (M.fromList (fromMaybe ([ ("PATH", (parentEnv M.! "PATH")) ]
                                                        ++ maybEnv "DEBUG" parentEnv
                                                        -- ++ maybEnv "TERM"  parentEnv
                                                       )
                                             (Proc.env cp)))
           L.logStrLn glog 4 $ "Running command: "++show (Proc.cmdspec cp)
           when (dbgLvl >= 5) $ do
              dir <- D.getCurrentDirectory
              L.logStrLn glog 5 $ "In directory: "++(fromMaybe dir (Proc.cwd cp))
           L.logStrLn glog 5 "subprocess: allowing read permissions (in addition to tmpdir,whitelist): "
           mapM_ (L.logStrLn glog 4 . show) rs
           L.logStrLn glog 5 "subprocess: allowing read/write permissions (in addition to tmpdir): "
           mapM_ (L.logStrLn glog 4 . show) rws

           (code,sout,serr) <-
               if L.dbgLvl >= printImmediatelyDebugLvl
                    -- This version echos stderr immediately:
               then do sout <- Proc.readCreateProcess (cp{Proc.env= Just env'}) input
                       return (ExitSuccess,sout,"")
               else Proc.readCreateProcessWithExitCode (cp{Proc.env= Just env'}) input

           let ctxt = "\nCommand: "++(show (Proc.cmdspec cp))
                      ++"\nStderr was:\n"++serr
                      ++"\nStdout was:\n"++sout
           case code of
             ExitFailure c -> error $  "\nsubprocess: ERROR - command exited with code "++show c++ctxt
                                    ++ "\nMost recently run command:\n"
                                    ++ unwords (map (\(var,val) -> var ++ "=" ++ val) env') ++ " "
                                    ++ ppCmdSpec (Proc.cmdspec cp)
             ExitSuccess -> do
   -- TODO: enable `detflow --debug` mode which allows stderr chatter:
   -- FIXME: UNCONDITIONALLY ALLOWING IT FOR NOW:
               -- case serr of
               --   "" -> return sout
               --   _  -> error $ "\nreadProcess: command exited with non-empty stderr output."++ctxt

               -- Deterministically put stderr AFTER stdout.  Coarse grained!
               return (sout++serr)

                -- if L.dbgLvl > 1
                -- then L.logStrLn glog 2 serr -- Get standard error sooner
                -- else hPutStrLn stderr serr
                -- FIXME.  Even when in this forwarding mode, we should
                -- use createProcess to directly forward stderr to our stderr
                -- handle, not echo it post-facto after buffering it in memory...

maybEnv :: Ord a => a -> M.Map a b -> [(a, b)]
maybEnv var parentEnv =
    case M.lookup var parentEnv of
      Nothing -> []
      Just d  -> [(var,d)]

ppCmdSpec :: Proc.CmdSpec -> String
ppCmdSpec (Proc.ShellCommand s) = s
ppCmdSpec (Proc.RawCommand fp args) = fp ++ " " ++ unwords args

-- | Sanitization. Currently, we disallow the following characters altogether.
--
-- This is a crude attempt to prevent piping or launching background
-- processes.  It's completely optional, because the subprocess will
-- be constrained by the dynamic harness to be serialized.
sanitize :: Fail.MonadFail m => String -> m ()
sanitize str = do
  {-
  when ('|' `elem` str) $
    fail $ "Process cannot contain | (in " ++ str ++ ")"
  -}
  case trim str of
    (unsnoc -> Just (_, '&')) ->
      fail $ "Background processes disallowed (in " ++ str ++ ")"
    _ -> return ()

sanitizeCP :: Fail.MonadFail m => Proc.CreateProcess -> m Proc.CreateProcess
sanitizeCP cp = do
 -- These are ignored anyway by readCreateProcess:
 -- if Proc.std_in cp /= Proc.Inherit
 -- then fail $ "sanitize: CreateProcess cannot use stdin = " ++ show (Proc.std_in cp)
 -- else if Proc.std_out cp /= Proc.Inherit
 -- then fail $ "sanitize: CreateProcess cannot use stdout = " ++ show (Proc.std_out cp) else
 -- if Proc.std_err cp /= Proc.Inherit
 -- then fail $ "sanitize: CreateProcess cannot use stdout = " ++ show (Proc.std_err cp)
 -- else
 do
   case Proc.cmdspec cp of
     Proc.ShellCommand s -> sanitize s
     Proc.RawCommand{}  -> return ()
   return cp

-- | Disable ASLR in the command we run.
wrapSetArch :: Proc.CreateProcess -> Proc.CreateProcess
wrapSetArch cp = cp { Proc.cmdspec = newspec }
 where
   newspec = case Proc.cmdspec cp of
               Proc.ShellCommand str  -> Proc.RawCommand "/usr/bin/setarch" ["x86_64","-R","/bin/bash","-c",str]
               Proc.RawCommand f args -> Proc.RawCommand "/usr/bin/setarch" $ ["x86_64","-R",f]++args
