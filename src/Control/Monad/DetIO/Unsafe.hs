{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Underlying implementation of the DetIO abstraction, with
-- everything revealed, including 'liftIOToDet'.

module Control.Monad.DetIO.Unsafe where

import           Control.Applicative
import qualified Control.Concurrent as C
import           Control.Concurrent.Async as A
import qualified Control.Exception as E
import           Control.Monad hiding (join)
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.DetIO.Logging (glog, logStrLn, dbgLvl)
import           Control.Monad.DetIO.Perms
import           Control.Monad.State (liftIO, lift)
import qualified Control.Monad.State.Strict as S
import           Data.Foldable
import           Data.IORef
import           Data.List as L
import           Data.Map  as M
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.Sequence as Seq
import           Data.Text as T
import qualified Data.Text.IO as T
import           Data.Word
import qualified Prelude as P
import           Prelude hiding (log,id,putStrLn,putStr)
import           Text.Show.Pretty (ppShow)
import           Text.Printf (printf)
-- FIXME: we currently have potentially exponential behavior in the
-- log graph generation... how to fix that?

--------------------------------------------------------------------------------


-- import Debug.Trace
trace :: a -> b -> b
trace _ x = x

-- TODO: replace with efficient encoding.. bitlist or Integer:
data PedStep = Parent | Child
  deriving (Eq, Ord, Show, Read)

type PedPath = [PedStep] -- root is empty list.  Each forkIO
                         -- introduces an intermediate node in the
                         -- tree and makes paths deeper.

-- Counters reset when we go to a new segment (edge) in the tree:
consParent :: Pedigree -> Pedigree
consParent (Pedigree _c ls) = Pedigree 0 (Parent : ls)

consChild :: Pedigree -> Pedigree
consChild (Pedigree _c ls)  = Pedigree 0 (Child : ls)

----------------------------------------

-- | Location in the fork tree, IGNORING IVar synchronization
-- operations.
data Pedigree = Pedigree { cntr    :: Word -- ^ count events along one thread.
                         , treeInd :: PedPath
                         -- Integer
                           -- ^ fork-tree index of current thread.  1
                           -- is child, 0 is parent.  Root starts at significant bit.
                         }
  deriving (Eq,Show)

-- | When filled, carries the pedigree of the writer thread.
data IVar a = IVar IVarID (C.MVar (Pedigree, Log, a))

-- | A handle on a Thread, much like a future, it returns a value when
-- it is joined.  However, joining a thread *transfers permissions*,
-- and thus it can only be joined *once*.
data Thread a = Thread IVarID (IORef (ThreadVal a))
 -- As our internal protocol, we replace the (Left x) inside the MVar
 -- with (Right y) to signify that the thread has already been joined.

-- | [Internal] return value of a thread.  We don't lose the return
-- value after joining.
data ThreadVal a = Unjoined (A.Async (Pedigree, Log, Perms, a))
                 | Joined a

-- threadval :: ThreadVal t -> t
-- threadval (Unjoined a) = a
-- threadval (Joined a) = a


-- newtype SomeIVar = SomeIVar (forall a . IVar a)
data SomeSyncable = forall a . SomeIVar   (IVar a)
                  | forall a . SomeThread (Thread a)

getID :: SomeSyncable -> IVarID
getID (SomeIVar (IVar id _)) = id
getID (SomeThread (Thread id _)) = id


type Mark = IORef Bool

type IVarID = Word64
type Log    = [Msg]

-- | A message is marked when it has already been printed.
data Msg = Msg { msg :: Text, ped :: Pedigree, mark :: Mark }
         | Link { ped :: Pedigree, msglog :: Log }
           -- ^ a placeholder indicating a synchronization: that the
           -- designated pedigree happened BEFORE this point on the
           -- current thread.
  deriving Eq


instance Show Msg where
  show Link{ped,msglog}    = "(Link "++show (ped,msglog)++")"
  show Msg{msg,ped} = "(Msg "++show msg++" "++show ped++")"

-- | The order in the fork tree IF no IVar synchronizations existed.
--   LESS means "before" in this ordering.
--   This corresponds to the order in a hypothetical sequential
--   execution of the program and is used only for symmetry breaking.
instance Ord Pedigree where
  Pedigree c1 l1 <= Pedigree c2 l2 =
      if   l1 == l2
      then c1 <= c2
      else go (L.reverse l1) (L.reverse l2)
    where
     go [] [] = True
     go [] _  = True -- The computation BEFORE the fork happens before
                     -- either the left or right branch of the fork.
     go _  [] = False
     go (Child:_) (Parent:_) = True -- Child before parent in this ordering.
     go (Parent:_) (Child:_) = False
     go (_:a) (_:b)          = go a b -- Match, keep going.

-- | Ordered by pedigree.
instance Ord Msg where
  a <= b = ped a <= ped b

-- TODO: Add PTRACE as an option here (#17)
-- | Three execution modes going from more enforcement to less enforcement
data ExecMode = Det       -- ^ The default, full determinism enforcement.
              | AllowASLR -- ^ Use all determinism enforcement EXCEPT disabling ASLR in subprocesses.
              | NonDet    -- ^ Run subprocesses natively, no determinism enforcement.
                          -- Also don't buffer printed output.
  deriving (Read,Show,Eq,Ord)

data ThreadState =
    TS { -- Thread private state:
         myped  :: !Pedigree
       , log    :: !Log -- ^ reverse chronological message log.
       , isMain :: !Bool -- ^ FIXME - MERGE WITH LOG.  isMain => log=[]
       , perms  :: !Perms

       -- Global state shared between threads:
       , execMode :: ExecMode -- ^ How much runtime determinism enforcement?

       , uniq     :: !(IORef Word64)
       , unjoined :: !(IORef (M.Map IVarID SomeSyncable))
          -- ^ Threads which have not yet had their effects joined
          -- into another thread.
       , parent   :: !C.ThreadId
          -- ^ The main thread for this runDetIO session.

       , shellLeases :: Maybe (C.Chan Word)
          -- ^ An optional channel used to throttle the number of
          -- concurrently active shell calls.  N tokens are put in the
          -- channel, and only while holding a token can a thread do a shell call.
       }

newtype DetIO a = DetIO (S.StateT ThreadState IO a)
  deriving ( Alternative
           , Applicative
           , Functor
           , Monad
           , MonadPlus
           , MonadFail
           )

-- | By convention we typically designate a particular directory as
-- the primary input to the deterministic workflow.  Returns an absolute path.
getMainInDir :: DetIO FilePath
getMainInDir = undefined

-- | By convention we typically designate a particular directory as
-- the primary output from the deterministic workflow.  Returns an absolute path.
getMainOutDir :: DetIO FilePath
getMainOutDir = undefined


-- Helpers
--------------------------------------------------------------------------------


-- Increment to count operations along ONE thread.
incr :: Pedigree -> Pedigree
incr (Pedigree n ti) = Pedigree (n+1) ti

-- replace with real fetch/add & counter:
fetchAdd1 :: IORef Word64 -> IO Word64
fetchAdd1 ref = atomicModifyIORef' ref (\n -> (n+1,n))

-- Here's the magic.  We crawl through a DAG of log messages and
-- inter-thread dependencies, printing the messages in a deterministic
-- order based on the dependence structure.
crawl :: Log -> IO ()
crawl lg = do
    trace ("CRAWLING:\n   "++(L.concat (L.intersperse "\n   " $ L.map show lg))) $ return ()
    sq <- gather [] lg
    trace ("GATHERED: "++show sq) $ return ()
    -- printit (Seq.toList sq)
    mapM_ printit sq
 where

   -- Backwards walk through the DAG that returns a linear series of
   -- messages to print.  Cut it off when we find something already marked.
   gather :: [Log] -> Log -> IO (Seq Msg)
   gather [] [] = return Seq.empty

   gather todos (m@Msg{mark} : rest) = do
       mrk <- readIORef mark
       if mrk
        -- OPTIMIZATION: we should be able to stop printing everything
        -- BEFORE this if this message is already marked as printed:
        then return Seq.empty
       -- Keep scrolling, add a message to the end of our final log:
        else fmap (|> m) $ gather todos rest

   -- A join point!!  Here we must consider two possible upstreams.
   -- We always favor OUR thread, we put the other on the pile:
   gather todos (Link _ log : rest) =
       trace ("DEFERRING LINK "++show log++"\n  proceeding w/ "++show rest)
       gather (L.insert log todos) rest

   -- Take the next log in pedigree order:
   gather (next:todos) [] =
     trace ("CHOICE POINT: " ++ show next ++"\n  over "++show todos) $
     gather todos next

   printit :: Msg -> IO ()
   printit (Msg txt _ped mark) =
     do b <- atomicModifyIORef' mark (\b -> (True,b))
        S.unless b $ do T.putStr txt

   printit (Link{}) = error "DetPrint/crawl/printit: internal error."

--------------------------------------------------------------------------------

-- | Run a deterministic computation in (nondeterministic) IO context.
--
-- The computation terminates when the main thread, and any forked
-- threads, have all terminated.  Only the value from the main thread
-- is returned.
runDetIO :: DetIO a -> IO a
runDetIO = runDetIOWith Nothing (IP emptyPerms)

-- It's extremely dangerous to give DetIO a MonadIO instance,
-- so let's just define a function instead.
liftIOToDet :: IO a -> DetIO a
liftIOToDet = DetIO . liftIO

lio :: IO a -> DetIO a
lio = liftIOToDet -- Shorthand for this file


-- | Build up an initial permission from a list of read paths and
-- write paths.
initPerms :: [FilePath] -> [FilePath] -> InitialPerms
initPerms rs ws = P.foldr (\/) (IP emptyPerms) ips
  where
   ips = L.map fromPathPerm (L.map mkPermR rs ++ L.map mkPermRW ws)

newTokenPool :: Word -> IO (C.Chan Word)
newTokenPool n
  | n <= 0 = error $ "newTokenPool: cannot limit to less than one token: "++show n
  | otherwise = do ch <- C.newChan
                   forM_ [1..n] (C.writeChan ch)
                   return ch

-- | Run with a given initial permission.  This is unsafe, allowing
-- forging of permissions, but that's ok because it's in IO.
--
-- This also, optionally, takes a maximum number of concurrent
-- shell-outs, to enable capping subprocesses and limiting
-- oversubscription.
runDetIOWith :: Maybe Word -> InitialPerms -> DetIO a -> IO a
-- FIXME: catch exceptions and flush out the print messages!!
runDetIOWith mjobs (IP iPerms) (DetIO act) = do
  uniq     <- newIORef 0
  unjoined <- newIORef M.empty
  tid      <- C.myThreadId
  pool     <- case mjobs of
                Just n  -> do logStrLn glog 1 $ " [detio] throttling to "++show n++" threads."
                              fmap Just $ newTokenPool n
                Nothing -> return Nothing
  let initTS = TS{ isMain = True
                 , myped  = Pedigree 0 []
                 , log    = []
                 , perms  = iPerms
                 , uniq, unjoined
                 , parent = tid
                 , shellLeases = pool
                 , execMode = Det
                 }

  -- TODO: kill unjoined threads if we receive an exception.  (Cannot
  -- use withAsync unless we go to CPS.)
  (val,ts) <- mainAct initTS

  -- Invariant-checking on final state:
  let TS{log} = ts
  case log of
    [] -> return ()
    _  -> error$ "runDetPrint: invariant broken, main thread had remaining messages: "++show log

  return val

 where
  mainAct = S.runStateT
                (do v <- act -- <<== The user's action!
                    lift $ logStrLn glog 4 $ " [detio] main computation finished, joining unjoined threads..."
                    -- FIXME: Need quiescence.  Or need to change
                    -- unjoined to be immutable and per-thread.
                    TS{unjoined=unjref} <- S.get

                    lift $ logStrLn glog 4 $ " [detio] waiting on unjoined computations."
                    -- Finally, flush all the unjoined.  Loop until it's drained.
                    let go ls = do
                         mp <- lift $ atomicModifyIORef' unjref (\m -> (M.empty,m))

                         -- COMPROMISE: don't get print messages from
                         -- unjoined threads, but don't let them diverge
                         -- or throw exceptions either.  Wait for them:
                         asncs <- lift $ forM (M.toList mp)
                                              (\(_, SomeThread (Thread _ ref)) ->
                                                   do x <- readIORef ref
                                                      case x of
                                                        Joined _    -> return Nothing
                                                        Unjoined as -> return (Just (fmap (const ()) as)))
                         let ls' = catMaybes asncs ++ ls
                         unless (P.null ls') $ do
                           lift $ logStrLn glog 4 $ " [detio] still waiting on unjoined, "++show(P.length ls')++" left"
                           (which,_) <- lift $ A.waitAny ls'
                           let ls'' = L.delete which ls'
                           unless (P.null ls'') $ go ls''
                    go []
                    lift $ logStrLn glog 4 $ " [detio] done waiting on unjoined computations."
                    return v
                    )

-- | Fork a deterministic thread which *must* complete before the
-- enclosing `runDetPrint` returns.  The thread can return a value,
-- and a blocking read on this value waits until the child thread has
-- completed.
forkIO :: DetIO a -> DetIO (Thread a)
forkIO = forkWithPerms []

-- | A version of forkIO that *gives away* permissions from the
-- parent to the child thread.
forkWithPerms :: [PathPerm] -> DetIO a -> DetIO (Thread a)
-- FIXME: use async to handle exceptions....
forkWithPerms reqPathPerms (DetIO act) = do
  -- The log is shared by both the continuation, and the child.
  TS{perms=parentPerms, ..} <- DetIO S.get

  -- First mess with perms:
  --------------------------------
  case foldrM (\ (req::PathPerm) (p,c) ->
               do (p',c') <- checkout req p
                  c''     <- addPerms c c'
                  return (p', c''))
           (parentPerms,emptyPerms) reqPathPerms
   of
    Left err -> detIOFail
                   ("Cannot fork child with these paths!:\n "++ ppShow reqPathPerms++
                    "\n\nThread holds only these permissions:\n"++ppShow parentPerms++
                    "\n\nDetailed error was:\n"++err)
    Right (newParentPerms, childPerms) -> do
      -- Adjust pedigree and do the fork
      ----------------------------------
      let ped1 = consParent myped
          ped2 = consChild myped
      -- Mutate the parent to run with these new perms and new pedigree:
      DetIO$ S.put TS{ log, isMain, uniq, unjoined, parent, shellLeases, execMode
                     , myped=ped1
                     , perms= newParentPerms }
      id   <- lio $ fetchAdd1 uniq -- Fresh id for thread-return-val IVar

      let action = (do (val,ts) <- S.runStateT act
                                              TS{ myped=ped2, perms= childPerms, isMain=False
                                                , log, uniq, unjoined, parent, shellLeases, execMode }
                       let TS{myped=finPed, log=l, perms} = ts
                       return (finPed, l, perms, val))
      -- Currently we EAGERLY push exceptions from ANY child thread directly to the root:
      fut  <- lio $ A.async (connectExns parent action)
      ref <- lio $ newIORef (Unjoined fut)
      let syncvar = Thread id ref
      lio $ addUnjoined unjoined syncvar
      return syncvar

-- | Unsafe command for debugging.  Normally we disable ASLR.
allowASLR :: DetIO ()
allowASLR = DetIO (S.modify (\ts -> case execMode ts of
                                        Det       -> ts { execMode = AllowASLR }
                                        AllowASLR -> ts
                                        NonDet    -> ts))

-- | Turn off all dynamic determinism enforcement.
fullNonDet :: DetIO ()
fullNonDet = DetIO (S.modify (\ts -> ts { execMode = NonDet }))


-- | Bounce exceptions over to another
connectExns :: C.ThreadId -> IO a -> IO a
connectExns tid act =
    act
--  E.catch act (\(e::E.SomeException) -> E.throwTo tid e >> E.throw e)

-- | Wait for several threads to /all/ complete.  However, this is
-- slightly better than 'mapM join', because it will throw an
-- exception earlier.
joinThreads :: [Thread a] -> DetIO [a]
joinThreads ls0 = do
 vals  <- lio $ mapM readIORef [ ref | Thread _ ref <- ls0 ]
 asncs <- forM vals $ \x -> case x of
                             Joined _   -> detIOFail "joinThreads: Attempt to join a thread twice."
                             Unjoined a -> return a
 let loop [] = return ()
     loop ls = do (which,_) <- lio $ A.waitAny ls -- Another one bites the dust.
                  loop (L.delete which ls)
 loop asncs
 mapM joinThread ls0

-- | Wait until the given thread terminates, and adopt the permissions
-- held by that thread.  In the case that the given thread terminated
-- with an exception, 'join' rethrows that exception.
--
-- A thread can only be joined *once*, and any further attempts to
-- join it will be considered an exception.  In contrast, *wait* is a
-- read-only operation that can be used multiple times.
joinThread :: Thread a -> DetIO a
joinThread (Thread iid ref) = do
  peek <- lio $ readIORef ref
  let err = "Attempt to join a thread twice, ID: "++show iid
  case peek of
    Joined _     -> detIOFail err
    Unjoined fut -> do
      (theirped, log, childPerms, val) <- lio $ A.wait fut -- Can rethrow exceptions.
      let fn (Joined _)     = detIOError err
          fn (Unjoined _) = (Joined val, ())
      TS{perms=parentPerms} <- DetIO S.get
      -- Here is the commit point for this operation:
      lio $ atomicModifyIORef' ref fn

      -- dbgPrint $ "join(isMain="++show isMain++"): Printing their log: "++show log
      printLog theirped log
      -- dbgPrint $"DONE PRINTING THEIR LOG... now add perms: "++
      --     show (parentPerms,childPerms)

      case addPerms parentPerms childPerms of
        Left err2 -> detIOFail $ "Error when joining child ("++show iid++
                                 ") into parent:\n"++err2
        Right perms' -> do
         -- dbgPrint $ "Setting our perms back to: "++show perms'
         DetIO $ S.modify $ \ts -> ts{perms = perms' }
         return val

-- | Wait until the given thread terminates, and return its final
-- value.  If the thread terminated with an exception, then 'wait'
-- rethrows that exception.
--
-- 'wait' has no effect on permissions, and it can be called before or
-- after the given thread has been `joined`.
waitThread :: Thread a -> DetIO a
waitThread = error "FINISHME: wait"


-- IVar operations
------------------------------------------------------------------------------

-- | Create a fresh, empty IVar.
new :: DetIO (IVar a)
new = DetIO $ do
  TS{uniq} <- S.get
  id  <- lift $ fetchAdd1 uniq
  mv  <- lift $ C.newEmptyMVar
  return $ IVar id mv

addUnjoined :: IORef (Map IVarID SomeSyncable) -> Thread a -> IO ()
addUnjoined unjoined iv@(Thread id _) =
  atomicModifyIORef' unjoined (\m -> (M.insert id (SomeThread iv) m, ()))

markJoined :: IVarID -> IORef (Map IVarID SomeSyncable) -> IO ()
markJoined id unjoined = do
  unjoinedSnap <- readIORef unjoined
  case M.lookup id unjoinedSnap of
    Nothing           -> return ()
    Just s -> atomicModifyIORef' unjoined (\m -> (M.delete (getID s) m,()))


-- | Blocking get on an IVar.
get :: IVar a -> DetIO a
-- On a get we link the writer's logged messages to ours.
get (IVar id mv) = do
  (writerPed,theirlog,val)   <- lio $ C.readMVar mv
  TS{unjoined} <- DetIO S.get
  lio $ markJoined id unjoined -- In case we finished out a thread.
  printLog writerPed theirlog
  return val

-- | Helper: join another threads log into ours, effectively replaying
-- and printing their messages on this thread.
printLog :: Pedigree -> Log -> DetIO ()
printLog writerPed theirlog = DetIO $ do
  ts@TS{isMain,log} <- S.get
  if isMain -- If we are the main thread, then we crawl the log.
    then lift$ crawl theirlog
    else do
      -- Otherwise we link the writer's log to ours.
      S.put $ ts { log = Link writerPed theirlog : log
    --             , myped = incr myped -- TODO: determine which events are counted.
                 }


-- | Fill an IVar, unblocking any waiting gets.
put :: IVar a -> a -> DetIO ()
-- Package our log and pedigree and send it with the value.
put (IVar _ mv) val = DetIO $ do
  TS{myped,log} <- S.get
  lift $ C.putMVar mv (myped,log,val)

-- Disk and stdout IO
-------------------------------------------------------------------------

-- | Log a message that will be printed deterministically, rather than
-- in realtime.
putTextLn :: Text -> DetIO ()
putTextLn str = putText (str <> "\n")

printImmediatelyDebugLvl :: Int
printImmediatelyDebugLvl = 5

-- | Log a message that will be printed deterministically, rather than
-- in realtime.
putText :: Text -> DetIO ()
putText str = DetIO $ do
  ts@TS{myped,log,isMain,execMode} <- S.get
  if execMode == NonDet
  then lift $ T.putStr str
  else if dbgLvl >= printImmediatelyDebugLvl -- HACK, at high debug levels dont delay output.
  then lift $ logStrLn glog 1 (T.unpack str) -- Can still race with output from subprocessses..
  else if isMain
   -- Don't even need a log entry if we're the main thread:
   then lift $ T.putStr str
     -- lift $ printf "%s" (T.unpack str)
   else do
    flag <- lift $ newIORef False
    S.put $ ts { log = Msg str myped flag : log
               , myped = incr myped
               }
    return ()


-- | Log a message that will be printed deterministically, rather than
-- in realtime.
putStrLn :: String -> DetIO ()
putStrLn s = putStr (s ++ "\n")

-- | Log a message that will be printed deterministically, rather than
-- in realtime.
putStr :: String -> DetIO ()
putStr = putText . pack

-- | Print a 'String' (nondeterministically) in realtime.
unsafePutStrLn :: String -> DetIO ()
unsafePutStrLn = lio . P.putStrLn

-- | Print a 'String' (nondeterministically) in realtime.
unsafePutStr :: String -> DetIO ()
unsafePutStr = lio . P.putStr

-- | Print a 'Text' (nondeterministically) in realtime.
unsafePutText :: Text -> DetIO ()
unsafePutText = lio . T.putStr

-- | Print a 'Text' (nondeterministically) in realtime.
unsafePutTextLn :: Text -> DetIO ()
unsafePutTextLn = lio . T.putStrLn

-- Uniform Errors
--------------------------------------------------------------------------------

-- | A failure in the deterministic IO monad, which becomes a failure
--  for the whole job.
detIOFail :: String -> DetIO a
-- TODO: in the future we may want to print some more info here, such
-- as the current pedigree.
detIOFail s = detIOError s

-- | An error from the DetIO system.  This is similar to detIOFail,
-- but does not have access to DetIO-specific state which can give a
-- more comprehensive error message.
detIOError :: String -> a
-- TODO: in the future we may want to print some more info here, such
-- as the current pedigree.
detIOError s = error ("[DetIO] " ++ s)


