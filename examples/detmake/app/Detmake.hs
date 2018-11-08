{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Attoparsec.ByteString as Atto
import Control.Monad hiding (join)
import Control.Monad.DetIO as D
import qualified Control.Monad.DetIO.Unsafe as D
import Control.Monad.DetIO.Logging as D
import Control.Monad.Extra hiding (join)

import qualified Data.ByteString.Char8 as BS
import Data.Foldable (forM_)
import Data.Makefile
import Data.Makefile.Parse (makefile)
import Data.Makefile.Parse.Internal (unescape)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M

import DetmakeLib

import Options.Applicative
import Prelude hiding (putStrLn, readFile, writeFile)
import qualified Prelude as P

-- import System.Process (system)
import System.Directory as Dir hiding (doesPathExist, findFile)
import Text.Show.Pretty (ppShow)

type EagerPrinting = Bool
type ASLR          = Bool
type NonDet        = Bool

data Args = Args
  { target        :: Maybe Target
  , jobs          :: Maybe Word
  , eagerPrinting :: EagerPrinting
  , unsafeASLR    :: ASLR
  , nondet        :: NonDet
  } deriving Show

argsParser :: Parser Args
argsParser
  = Args
  <$> argument (Just . Target . BS.pack <$> str)
      (  metavar "TARGET"
      <> value Nothing
      <> help "Run this target. If one isn't applied, run the first target.")
  <*> option (Just <$> auto)
      (  long "jobs"
      <> short 'j'
      <> value Nothing
      <> metavar "N"
      <> help "Allow N jobs at once" )
  <*> switch
      (  long "eager-printing"
      <> short 'e'
      <> help "Print logs eagerly (and nondeterministically)")
  <*> switch
      (  long "unsafe-ASLR"
      <> help "Use address space randomization, which is a source of nondetermiism.")
  <*> switch
      (  long "nondet"
      <> help "Fully nondeterministic execution with no restrictions at all.")

-- | Should putStrLn be eager, and nondeterministic?
putStrLn' :: EagerPrinting -> String -> DetIO ()
putStrLn' True  = D.unsafePutStrLn
putStrLn' False = putStrLn

main :: IO ()
main = do d <- Dir.getCurrentDirectory
          when (dbgLvl>=2) $ P.putStrLn $ " [detmake] Set current directory to: "++show d
          inD  <- Dir.makeAbsolute "."
--          outD <- Dir.canonicalizePath "./out"
--          Dir.createDirectoryIfMissing False "./out"

          let opts = info (argsParser <**> helper)
                     ( fullDesc
                     <> progDesc "Deterministic implementation of make"
                     <> header "")
          args <- execParser opts -- TODO: use pure version.
          when (dbgLvl>=3) $ P.putStrLn$ " [detmake] Args: "++show args
          when (dbgLvl>=2) $ P.putStrLn$ " [detmake] Granting RW permission to:\n  "++inD++"\n  "-- ++outD
          runDetIOWith (jobs args) (initPerms [] [inD]) -- outD
                       (detmain args)

detmain :: Args -> DetIO ()
detmain args = do
    let ep = eagerPrinting args
    when (dbgLvl>=1) $ putStrLn' ep $ " [detmake] Starting deterministic make, target: "++show(target args)
    upperMakefile <- findFile ["."] "Makefile"
    lowerMakefile <- findFile ["."] "makefile"
    let mfl = upperMakefile <|> lowerMakefile
    when (isNothing mfl) $
      error "detmake: *** No Makefile. Stop."
    mflStr <- readFile $ fromJust mfl
    when (dbgLvl>=2) $ putStrLn' ep " [detmake] Makefile read.  Parsing..."
    let Right !parsedMF = Atto.parseOnly makefile $ unescape mflStr
    when (dbgLvl>=2) $ putStrLn' ep " [detmake] Parse complete."

    let isRule (Rule (Target n) _ _)
                 | Just (t,_) <- BS.uncons n, t /= '.'
                 = True
        isRule _ = False
        firstTgtName = case L.find isRule (entries parsedMF) of
                         Just (Rule (Target tgt) _ _)
                           -> Just $ Target $ trim tgt
                         _ -> Nothing
    if isNothing (target args) && isNothing firstTgtName
       then error "detmake: *** No targets.  Stop."
       else let (regexMap, subMF1) = subMakefile False -- Don't expand suffix rules here
                                   $ trimMakefile parsedMF
                subMF2             = handleSuffixRules regexMap
                                   $ trimMakefile subMF1
                postProcessedEntries = entries
                                     $ splitPostFacto
                                     $ combineTargets
                                     $ subMF2
                subFirstTgtName = subTarget regexMap <$> firstTgtName
                tgt = fromJust (target args <|> subFirstTgtName)
            in invokeTarget (unsafeASLR args) (nondet args) ep tgt
                            {-(jobs args)-} postProcessedEntries
    -- putStrLn' ep $ " [detmake] Parsed: " <> T.pack(show parsedMF)

traceMe :: Show a => String -> a -> a
-- traceMe x y = trace ("RGS " ++ x ++ ": " ++ show y) y
traceMe _ x = x

fromDep :: Dependency -> BS.ByteString
fromDep (Dependency bs) = bs

logDbg :: EagerPrinting -> Int -> String -> DetIO ()
logDbg ep prio msg =
  when (dbgLvl>=prio) $
    putStrLn' ep msg

invokeTarget :: ASLR -> NonDet -> EagerPrinting -> Target -> [Entry] -> DetIO ()
invokeTarget aslr nondet' ep (Target tgt) entrs = do
    when aslr D.allowASLR
    when nondet' D.fullNonDet
    logDbg ep 4 $ " [detmake] Starting invokeTarget."
    logDbg ep 4 $ " [detmake] length of entrs: "         ++ show (length entrs)
    logDbg ep 4 $ " [detmake] AllDeps: "         ++ ppShow deps
    logDbg ep 4 $ " [detmake] Partitioning targets.."
    let (!reachableTargets, !reachableFiles) = L.partition isTarget reachable

    logDbg ep 3 $ " [detmake] All mentioned targets: "  ++ show (M.keys deps)
    logDbg ep 2 $ " [detmake] ReachableTargets: "++ show reachableTargets
    logDbg ep 2 $ " [detmake] ReachableFiles: "  ++ show reachableFiles

    logDbg ep 4 $ " [detmake] Checking that paths exist.."
    forM_ reachableFiles $ \pth -> do
      unlessM (doesPathExist $ BS.unpack pth) $
        error$ "Dependency on file "++show pth++", with no rule to build it."

    logDbg ep 4 $ " [detmake] All files exist or have a build rule.  Creating "
                  ++show (length reachableTargets)++" IVars... "
    ivars <- mapM (\_ -> new) reachableTargets
    let ivmap = M.fromList (zip reachableTargets ivars)

    thrds <- forM reachableTargets $ \ this -> do
     absThis <- D.makeAbsolute (BS.unpack this)
     absIn   <- D.makeAbsolute "." -- HACK FIXME
     forkWithPerms [ mkPermR "/dev/urandom", mkPermR absIn
                   , mkPermRW absThis ] $ do
      -- block on ivars for dependencies
      let (cmds, mydeps) = deps # this
      logDbg ep 2 (" [detmake] Inside thread for target: "++show this++", waiting on "++show(length mydeps)
                  ++" deps: "++unwords (L.map BS.unpack (S.toList mydeps)))
      forM_ mydeps $ \d ->
          case M.lookup d ivmap of
            Just iv -> do logDbg ep 4 (" [detmake] Dep has an IVar, blocking: "++ show d)
                          get iv
            Nothing -> logDbg ep 4 (" [detmake] Dep must be just a file: "++ show d)
      -- return () -- Must not be a "target".

      logDbg ep 2 (" [detmake] Target "++show this++" done blocking on "
                   ++show (length mydeps)++" dependencies, now exec "
                   ++show (length cmds)++" commands..")

      -- run command:
      forM_ cmds $ \(Command cmd) -> do
        let c = BS.head cmd
        -- Respect the special character @ in make, which suppresses echoing
        unless (c == '@') $ logDbg ep 0 (BS.unpack cmd)
        -- putStrLn $ T.pack $ "Running: " ++ BS.unpack cmd

        -- HACK HACK HACK:
        -- let (rator:rands) = words (BS.unpack cmd)
        -- sout <- readProcess rator rands ""

        let cmd' = sanitize cmd
        sout <- readShell (BS.unpack cmd') ""
--         sout <- readProcess "/bin/bash" ["--verbose","-c", BS.unpack cmd'] ""
--         sout <- readProcess "/bin/echo" ["--verbose","-c", BS.unpack cmd'] ""

        logDbg ep 4 (" [detmake] Target "++show this++", command completed: "++BS.unpack cmd)
        forM_ (lines sout) (putTextLn . T.pack)

      logDbg ep 2 (" [detmake] Target "++show this++", commands finished, marking ivar.")
      -- write ivar to show that OUR build outputs are available:
      put (ivmap # this) ()

    when (dbgLvl>=1) $ putStrLn " [detmake] Main thread now waiting on forked children..."
    _ <- joinThreads thrds -- Wait for everything to finish.
    when (dbgLvl>=1) $ putStrLn " [detmake] All threads joined."

  where
    isTarget d = M.member d deps

    deps :: M.Map BS.ByteString ([Command], S.Set BS.ByteString)
    deps = M.fromList [ (tgt, (cmds, S.fromList (L.map fromDep deps)))
                      | Rule (Target tgt) deps cmds <- entrs ]

    -- All the targets that will (transitively) be built:
    reachable :: [BS.ByteString]
    reachable = S.toList $ go S.empty [tgt]
    go acc [] = acc
    go acc (x:xs) =
      case M.lookup x deps of
        Nothing -> go (S.insert x acc) xs -- We'll deal with this later.
        Just (_,alldeps) -> let fresh = alldeps `S.difference` acc
                            in go (S.insert x (acc `S.union` fresh)) (S.toList fresh ++ xs)

    -- Make has a special - prefix that you can put in front of commands to have
    -- their exit codes be ignored. This is unacceptable for us, so we simply
    -- remove occurrences of -.
    --
    -- Also remove @, whose behavior is noted above.
    sanitize str
        -- Man I wish we had or-patterns...
      | Just ('-', rest) <- BS.uncons str = rest
      | Just ('@', rest) <- BS.uncons str = rest
      | otherwise                         = str

(#) :: (Show k, Ord k) => M.Map k v -> k -> v
m # k =
 case M.lookup k m of
   Nothing -> error ("Map does not contain key: "++show k++"\nKeys present: "++show (M.keys m))
   Just x -> x

{-
    findTarget :: [Entry] -> IO (Maybe [String])
    findTarget []                = pure Nothing
    findTarget (Assignment{}:xs) = findTarget xs
    findTarget (Rule (Target tgt') deps cmds:xs)
      | tgt == tgt' = case mbJobs of
                        Nothing -> sequentialDeps deps cmds
                        Just n  -> withPool (fromIntegral n) $ \pool ->
                                     parallelDeps pool deps cmds
      | otherwise   = findTarget xs

--     invokeCommands :: [Command] -> IO ()

    sequentialDeps :: [Dependency] -> [Command] -> IO (Maybe [String])
    sequentialDeps deps cmds = do
        forM_ deps $ \(Dependency dep) -> invokeTarget (Target dep) mbJobs entrs
        forM_ cmds $ \(Command cmd) -> do
            BS.putStrLn cmd
            ec <- system $ BS.unpack cmd
            case ec of
              ExitSuccess   -> pure ()
              ExitFailure{} -> throw ec
        pure Nothing
      `catch`
        \(se :: SomeException) -> throw se

    parallelDeps :: Pool -> [Dependency] -> [Command] -> IO (Maybe [String])
    parallelDeps pool deps cmds = do
        depCmds <- parallel pool $ map (\(Dependency dep) -> invokeTarget (Target dep) mbJobs entrs) deps
        forM_ depCmds $ maybe (pure ()) (mapM_ P.putStrLn)
        forM_ cmds $ \(Command cmd) -> do
            ec <- system $ BS.unpack cmd
            case ec of
              ExitSuccess   -> pure ()
              ExitFailure{} -> throw ec
        pure $ Just $ map (BS.unpack . unCommand) cmds
      `catch`
        \(se :: SomeException) -> throw se
-}


unDependency :: Dependency -> BS.ByteString
unDependency (Dependency dep) = dep

unCommand :: Command -> BS.ByteString
unCommand (Command cmd) = cmd
