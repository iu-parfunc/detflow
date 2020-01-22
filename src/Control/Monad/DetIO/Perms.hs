{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Abstract datatypes for permissions.
module Control.Monad.DetIO.Perms
    (
    -- * Individual permissions values
      Perm(..), noPerm
          -- TODO!  HIDE THESE.

    -- * Single-path perms
    , PathPerm, mkPermR, mkPermRW
    , fromPathPerm, pathPermToPath

    -- * Collective permissions
    , InitialPerms, Frac, (\/), halve
    , Perms, allPerms, emptyPerms
    , startingPerms, partitionPerms

    -- * Operations on permissions
    , checkout
    , addPerms
    , canRead, canWrite, canExtendDir

    -- * Testing only
    -- TODO: Hide these.
    , PathPerm(..), Perms(..), InitialPerms(..)
    , arbPerm2, joinPerm
    , normPerms, addPerms'
    )
    where

import           Control.Monad (replicateM, foldM)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Char
import           Data.List
import           Data.Either (isRight)
import           Data.Semilattice.Join
import           Data.Semilattice.Lower
import           Data.Semilattice.Meet
import           Data.Semilattice.Upper
import           System.FilePath
import           Test.QuickCheck

import Debug.Trace

{-

NOTE: Permissions design
========================

The key invariant we must maintain is that when a thread has (or
reaquires) write permission on a path, that no other thread retains
permission on it.




NOTE: Permission Lattice
------------------------

NOTE: Fractional Permissions
----------------------------



NOTE: Transitive Permissions
-----------------------------


 * Permissions with Holes.



NOTE: Directory creation
------------------------

For this simple design, we must choose whether directory creation is
explicit, or implicit in `writeFile`.  Implicit creation is
problematic, however.  Racing "mkdir -p" is not safe.  Thus we require
explicit creation with `createDirectory`.


NOTE: Semantics of R/RW on directories
--------------------------------------

 * R: ability to list the directory contents
 * RW: ability to add new children and delete the directory

However, we do NOT require R >= 0 in order to reach a CHILD which has non-zero
permissions.  I.e. "execute" in unix parlance is free.


NOTE: A list of litmus tests
----------------------------


SCRATCH:
--------

             /a/b
           parent R /a  ... can readFile /a/b/c?
           child  W /a/b ... writeFile /a/b/c /a/b/d ...

    R /a  ... gaveWrite /a/b
     \
      _    ... gaveWrite /a/b
       \
        W /a/b

-}



------------------------------------------------------------
-- Publicly visible atomic permissions
------------------------------------------------------------

type FileName = String

-- | A permission on a single directory.  These are the publically
-- visible, yes-or-no, permissions that a user can request.
--
-- Note that permision on a path applies *transitively* to all paths
-- underneath it.  I.e. permission on a directory confers permission
-- on its children, and so on.
data PathPerm = ReadPath  { pathparts :: [FileName] }
              | WritePath { pathparts :: [FileName] }
     -- ^ Discrete permissions - no fractions!
 deriving (Eq)

instance Show PathPerm where
  show pa = ppWhich pa ++" "++ pathPermToPath pa

-- [Internal]
ppWhich :: PathPerm -> String
ppWhich (ReadPath _)  = "R"
ppWhich (WritePath _) = "W"

-- | Path of a PathPerm
pathPermToPath :: PathPerm -> FilePath
pathPermToPath pp = "/" ++ concat (intersperse "/" (pathparts pp))

-- | Create a permissions tree that only has permission over the given
-- file/directory.  The resulting InitialPerms can be joined together.
fromPathPerm :: PathPerm -> InitialPerms
fromPathPerm (ReadPath  fns) = IP $ buildPerm (R 0) (R 1)  fns
fromPathPerm (WritePath fns) = IP $ buildPerm (R 0) (RW 1) fns


-- | Construct a single, fine-grained Read-only permission.
mkPermR :: FilePath -> PathPerm
mkPermR path =
 case splitDirectories path of
   "/":namels -> ReadPath namels
   _ -> error $ notAbsolutePathError "mkPermR" path

-- | Construct a single, fine-grained Read-Write permission.
mkPermRW :: FilePath -> PathPerm
mkPermRW path =
 case splitDirectories path of
   "/":namels -> WritePath namels
   _ -> error $ notAbsolutePathError "mkPermRW" path


------------------------------------------------------------
-- [Internal] Fractions with power-of-two denominators
------------------------------------------------------------

-- | A datatype for exact fractional permissions.
data Frac = Frac {-# UNPACK #-} !Int
                 {-# UNPACK #-} !Int
          -- ^ Frac x y is shorthand for (x / 2^y)

halve :: Frac -> Frac
halve (Frac 0 m) = Frac 0 m
halve (Frac n m) = Frac n (m+1)

instance Show Frac where
  show f =
    case normalize f of
      Frac x 0 -> show x
      Frac x y -> show x ++"/"++ show ((2 :: Integer)^y)

instance Num Frac where
  Frac x y + Frac a b
      | y == b    = Frac (x+a) y
      | y > b     = Frac (x + a*2^(y-b)) y
      | otherwise = Frac (x*2^(b-y) + a) b
  Frac x y * Frac a b = Frac (x*a) (y+b)
  negate (Frac x y) = Frac (negate x) y
  signum (Frac x _) = Frac (signum x) 0
  fromInteger n = Frac (fromInteger n) 0
  abs f = signum f * f

instance Ord Frac where
  compare (Frac x y) (Frac a b)
    | y == b    = compare x y
    | y > b     = compare x  (a*2^(y-b))
    | otherwise = compare (x*2^(b-y)) a

-- | Build a fraction.  Denominator must be a power of two.
mkFrac :: Int -> Int -> Frac
mkFrac x y = Frac (fromIntegral x) (log2 y)

log2 :: (Integral a1, Num a, Show a1) => a1 -> a
log2 n0 = go n0
 where
 go 1 = 0
 go n =
  case divMod n 2 of
    (n',0) -> 1 + log2 n'
    _      -> error $ "log2: not a power of 2: "++show n0

-- | Simplify the fraction.
normalize :: Frac -> Frac
normalize (Frac x 0) = Frac x 0
normalize (Frac x y) =
    case divMod x 2 of
      (x',0) -> normalize (Frac x' (y-1))
      _      -> Frac x y

instance Eq Frac where
  Frac x y == Frac a b =
    x * (2^b) == a * (2^y)

-- | Debugging only.
tup :: Frac -> (Int,Int)
tup (Frac x y) = (x,y)


-- [INTERNAL] Individual file/directory permissions
------------------------------------------------------------

-- | Fractional version, no write-only.
--
--   RW on a directory should entail the ability to completely,
--   recursively, DELETE that directory.
data Perm = R  Frac
          | RW Frac -- ^ Fractional read permission.  Write capability
                    -- active only if the fraction is exactly 1.
  deriving (Eq, Show)

weight :: Perm -> Frac
weight (R  w) = w
weight (RW w) = w


-- Collecting perms together for whole directories
------------------------------------------------------------

-- | The initial permissions at the start of a computation.
--
-- Initial Permissions form a lattice where appending is a
-- union/max-like operation.  Combining two perms always yields a
-- strictly stronger permission according to the (join) partial order.
--
newtype InitialPerms = IP Perms

-- | Join is permissions *union*.
instance Join InitialPerms where
  IP a \/ IP b = IP (go a b)
   where
    go :: Perms -> Perms -> Perms
    go (Has p1) (Has p2) = Has $ joinPerm p1 p2
    go (Node p1 ch1) (Node p2 ch2) =
        Node (joinPerm p1 p2) (M.unionWith go ch1 ch2)
    go (Has p1) (Node p2 ch2) = Node (joinPerm p1 p2) ch2
    go (Node p2 ch2) (Has p1) = Node (joinPerm p1 p2) ch2

-- | Take the stronger permission.
joinPerm :: Perm -> Perm -> Perm
joinPerm a b =
  case (a,b) of
   (R n,  R  m) -> R  (max m n)
   (RW n, RW m) -> RW (max m n)
   (RW n, R  m) -> fix n m
   (R  n, RW m) -> fix m n
 where
   -- The validity of this is subtle.
   fix rw ro = if rw == 0
               then R ro
               else RW rw

instance Lower InitialPerms where
  lowerBound = IP emptyPerms

-- | Meet is permissions *intersection*.
instance Meet InitialPerms where
  _a /\ _b = undefined

instance Upper InitialPerms where
  upperBound = IP allPerms




-- | The Perms structure is a simple Trie storing paths'
--   permissions. The root starts at "/"
--
--   The basic design is that a leaf node (file or dir), has its
--   permissions shared between N processes.  Intermediate nodes are
--   shared by all processes that hold them as leaves OR that hold
--   paths that pass through them.  For example, to delete a
--   directory, no observer may hold permissions to deeper paths
--   within that directory.
--
--   At all points in time, the global permissions held within the
--   system are conserved.  That is, applying 'addPerms' to all
--   threads' local permissions recovers the 'InitialPerms' that the
--   computation began with.
--
--   Permissions on directories are interepreted as shorthands for an
--   infinite set of permissions on children paths, held with the
--   *same* fraction as the parent.  However, lower or higher
--   fractions explicitly set on child paths "override" this default.
--   For example, if a reader of the /a directory (1.0 weight) loans
--   out a read permission on '/a/b', then both parent and child will
--   retain (only) a fractional permission on `/a/b`.
--
data Perms = Has Perm -- ^ Leaf permission.
           | Node { this :: Perm -- ^ the permissions on the intermediate
                                 -- directory itself.  Relevant to directory
                                 -- listing and deletion.
                  , children :: M.Map FileName Perms
                                -- ^ INVARIANT: non-empty.
                  }
  deriving (Eq, Show)

-- | Smart constructor.
mkNode :: Perm -> M.Map FileName Perms -> Perms
mkNode p mp | M.null mp = Has p
            | otherwise = Node p mp


notAbsolutePathError :: String -> FilePath -> String
notAbsolutePathError prefix fp =
  prefix ++ ": expected absolute path starting with /, got: " ++ fp

-- | Default starting perms if none are provided.
startingPerms :: Perms
startingPerms = emptyPerms
 -- TODO: REMOVE THIS

-- | Split permissions into a list of Read-perms and ReadWrite-perms,
-- respectively.  This returns absolute paths, as usual.
partitionPerms :: Perms -> ([FileName], [FileName])
partitionPerms perms = go parent perms ([], [])
  where
    parent = "/"
    go :: FilePath -> Perms -> ([FileName], [FileName]) -> ([FileName], [FileName])
    go parent (Has p) (rs, rws)
      = case p of
          R n  | n > 0 -> (parent:rs, rws)
          RW n | n > 0 -> (rs, parent:rws)
          _            -> (rs,rws)
    go parent (Node{this,children}) (rs, rws) =
      let (rs',rws') = case this of
                         R n  | n > 0 -> (parent:rs,rws)
                         RW n | n > 0 -> (rs, parent:rws)
                         _            -> (rs,rws)
      in M.foldrWithKey (\cfp -> go (parent </> cfp)) (rs',rws') children


-- | Check if we can read a file (or list a directory) based on a given
-- permission.
canRead :: FilePath -> Perms -> Bool
canRead path perms =
 case splitDirectories path of
   "/":namels -> case lookupPerm namels perms of
                   Nothing -> False
                   Just (R n) | n > 0      -> True
                              | otherwise  -> False
                   Just (RW n) | n > 0     -> True
                               | otherwise -> False
   _ -> error$ "canRead: given a non-absolute path: "++path

-- | Check if we can write a file (or delete a directory) based on a given
-- permission.
canWrite :: FilePath -> Perms -> Bool
canWrite path perms =
 case splitDirectories path of
   "/":namels -> case lookupPerm namels perms of
                   Nothing                 -> False
                   Just (R _)              -> False
                   Just (RW n) | n >= 1    -> True
                               | otherwise -> False
   _ -> error$ "canWrite: given a non-absolute path: "++path


-- | Can a new child entry be added within a directory?
canExtendDir :: FilePath -> Perms -> Bool
canExtendDir path perms =
 case splitDirectories path of
   "/":namels -> case lookupPerm namels perms of
                   Nothing    -> False
                   Just (R _) -> False
                   Just (RW n) | n > 0     -> True
                               | otherwise -> False
   _ -> error$ "canExtendDir: given a non-absolute path: "++path


-- | Lookup a permission for a particular path, expressed as a list of filenames.
lookupPerm :: [FileName] -> Perms -> Maybe Perm
lookupPerm [] (Has p)      = Just p
lookupPerm [] (Node p _) = Just p
lookupPerm (_:_) (Has p)   = Just p
-- This last case is most complex... we must see IF it is mentioned in the
-- subdir
lookupPerm (nxt:rst) (Node this ch) =
   case M.lookup nxt ch of
     Just p  -> lookupPerm rst p
     Nothing -> Just this


-- | The root permission, RW access to "/".
allPerms :: Perms
allPerms = Has (RW 1)

-- | Empty permissions.
emptyPerms :: Perms
emptyPerms = Has (R 0)

-- | The minimum permission: with zero fraction.
noPerm :: Perm
noPerm = R 0

--------------------------------------------------------------------------------

-- FIXME: change this to take a PathPerm

-- | Check out a permission on a path, returns (parent,child).
--   This borrows part of the permission on every directory leading
--   up to the target path.
checkout :: PathPerm -> Perms -> Either String (Perms,Perms)
checkout pp tree = go (refine namels tree) namels
 where
--   "/":namels = splitDirectories path
   namels  = pathparts pp
   getMine = case pp of
               ReadPath{}  -> getRead
               WritePath{} -> getWrite

   -- Leaf node: file OR directory.
   go (Has p) [] =
      let (p',c) = getMine p in
      pure (Has p', Has c)

   -- A leaf node from our perspective, but they want to go deeper.
   -- We dynamically refine our leaf node under its transitive interpretation.
   go (Has p) ls@(_:_) = error$ "internal error: checkout expected refined Perms, found:"
                         ++ show(Has p) ++ " with outstanding path: "++show ls

   -- If they want to checkout write permission, by default they get a
   -- fraction of the penultimate directory node so that they can
   -- EXTEND it by creating the new file.
   go Node{this,children} [last] | WritePath{} <- pp =
     case M.lookup last children of
       Just per -> do
        (new,chld) <- go per []
        let (p',c) = getSharedWrite this
        pure ( Node p' (M.insert last new children)
             , Node c  (M.singleton last chld) )
       Nothing -> error$ "internal error: checkout expected refined Perms, found:"
                         ++" no entry for outstanding path: "++show [last]


   -- Here we ask for read permission on a directory that already has
   -- refined children.  They get half of everything, recursively.
   go p@Node{} [] | ReadPath{} <- pp = pure $ getReads p
                  | otherwise = error $ "internal error."++
                                "  Should have been caught by penultimate case!"

   -- RECURSIVE CASE: The parent retains a full permission on this
   -- directory, but the NEW CHILD gets a full perm at the deeper
   -- path.  Since the child is just "passing by" they don't directly
   -- get perm on the dir itself.
   go Node{this,children} (nxt:rst) =
       case M.lookup nxt children of
         Just nxtper -> do
           (new,chld) <- go nxtper rst
           pure (Node this (M.insert nxt new children),
                 Node noPerm  (M.singleton nxt chld))
         Nothing ->  error$ "internal error: checkout expected refined Perms, found:"
                         ++" no entry for outstanding path: "++show (nxt:rst)



-- | Refine a permission so that it /temporarily/ violates our normal
-- form to describe deeper paths than necessary.  In particular, it
-- includes information on the given path (expressed as a list of
-- directories.
--
-- The inverse of refine is normalize, which clears away the redundant
-- information.
refine :: [FileName] -> Perms -> Perms
refine [] tree = tree
-- This is the essense of our transitive semantics:
refine (nxt:rst) (Has p) = Node p (M.singleton nxt (refine rst (Has p)))
refine (nxt:rst) (Node this children) =
  case M.lookup nxt children of
    Nothing -> Node this (M.insert nxt (refine rst (Has this))
                           children)
    Just ps -> Node this (M.insert nxt (refine rst ps) children)

-- | Recursively cut permissions in half, yielding (parent,child) with
-- the latter being a read-only permission.
getReads :: Perms -> (Perms,Perms)
getReads (Has p) = let (p',c) = getRead p in (Has p', Has c)
getReads (Node p children) =
    let (p',c) = getRead p
        m1 = M.map getReads children
    in
    ( Node p' (M.map fst m1)
    , Node c  (M.map snd m1))


-- | Add the permissions back together.
addPerms :: Perms -> Perms -> Either String Perms
addPerms a b = fmap normPerms (addPerms' a b)

-- | Add the permissions back together but DONT normalize the result.
addPerms' :: Perms -> Perms -> Either String Perms
addPerms' (Has p1) (Has p2) = Has <$> addPerm p1 p2
addPerms' (Node p1 ch1) (Node p2 ch2) = do
    p'  <- addPerm p1 p2
    ch' <- unionWithM addPerms' ch1 ch2
    return $ Node p' ch'

-- Identity:
addPerms' (Has (R 0)) y = pure y
addPerms' x (Has (R 0)) = pure x

-- TODO: this is subtle and may be an error.  We need to apply the
-- transitive interpretation.
addPerms' (Has p1) (Node p2 ch2) =
    -- trace ("WARNING: "++show (Has p1, Node p2 ch2))$
    Node <$> addPerm p1 p2 <*> pure ch2
--addPerms' (Node p2 ch2) (Has p1) = Node <$> addPerm p1 p2 <*> pure ch2
addPerms' x@Node{} y@Has{} = addPerms' y x


-- addPerms' a b = addPerms' b a
-- TODO: this should probably be renamed something other than join..
-- It's NOT going to be a lattice join.

-- QUESTION: do we need a "max" operation that is a total function on perms
-- taking the max of each fraction rather than adding them.  (That may be the
-- lattice join actually.)


addPerm :: Perm -> Perm -> Either String Perm
addPerm (R n) (R m)   = return $ R (n+m)
addPerm (RW n) (RW m) = return $ RW (n+m)
addPerm x (R 0) = return x
addPerm (R 0) x = return x
addPerm x (RW 0) = return x
addPerm (RW 0) x = return x
addPerm x y = Left $ "addPerm Error: Cannot join mismatched perms: "++show (x,y)



--------------------------------------------------------------------------------
-- NORMALIZATION
--------------------------------------------------------------------------------


-- | Normalize permissions to not include redundant information.  That
-- is, do not include information /implied/ by the transitive
-- interpretation of permissions.
normPerms :: Perms -> Perms
-- FIXME: out of date - rewrite this to accurately reflect the transitive interpretation.
normPerms (Has p) = Has p
normPerms (Node p ch) = mkNode p (M.mapMaybe (go p) ch)
  where
    -- This is the key bit, information REDUNDANT with the parent is removed.
    go p (Has p2) | p == p2   = Nothing
                  | otherwise = Just (Has p2)
    go p1 y@Node{} =
     case normPerms y of
       -- For it to not normalize away, there must be some child that contains information:
       Node p2 ch -> Just (Node p2 ch)
       y'@Has{}   -> go p1 y'


-- | Replicate a given permision for intermediate directories up to a
-- given leaf.
buildPerm :: Perm -> Perm -> [FileName] -> Perms
buildPerm intermediatePerm leafPerm = go
  where
    go []     = Has leafPerm
    go (x:xs) = Node { this      = intermediatePerm
                     , children  = M.singleton x $ go xs }

-- | Extract a read permission from the given permission.
--   Returns (parent,child).
getRead :: Perm -> (Perm,Perm)
getRead p =
 case p of
   R n  -> (R (halve n), R (halve n))
   -- Neither can have write because they will be sub-1.0:
   RW n -> (RW (halve n), RW (halve n))
      -- A read was requested, but here we get a fractional RW, which
      -- is just as good.  Its innaccurate, however, in the sense that
      -- it WILL serve as directory-extension permission, which we
      -- didn't really ask for.


-- | Extract an exclusive write permission from the given permission.
--   Returns (parent,child).
getWrite :: Perm -> (Perm,Perm)
getWrite p =
 case p of
   -- R n  -> error "FINISHME: getWrite" -- (R (halve n), R (halve n))
   -- -- If we halve, they both will be sub-1.0:
   -- RW n -> (RW 0, RW n)
   R n  -> error "cannot extract a write permission from read-only"
   RW n -> (RW 0, RW n)
   -- Should this be RW 0?  Should that state even exist?
   -- Or should it be `R 0`, which is bottom.

-- | Share the write permission symetrically.
getSharedWrite :: Perm -> (Perm,Perm)
getSharedWrite p =
 case p of
   R n  -> error "getSharedWrite: cannot extract a write permission from read-only"
   RW n -> (RW (halve n), RW (halve n))

-- | Modify the permissions across all nodes of a directory tree
--   (leaf and interior)
mapPerms :: (Perm -> Perm) -> Perms -> Perms
mapPerms f (Has p) = Has (f p)
mapPerms f (Node p  ch) = Node (f p) (M.map (mapPerms f) ch)

-- | Filter out nodes that are not needed.  Return Nothing if there are no nodes
-- left.
filterPerms :: (Perm -> Bool) -> Perms -> Maybe Perms
filterPerms fn (Has p)
    | fn p = Just (Has p)
    | otherwise = Nothing
filterPerms fn (Node p ch) =
    case catMaybes [ (k,) <$> filterPerms fn v | (k,v) <- M.toList ch ] of
      [] -> filterPerms fn (Has p)
      ls -> let p' = if fn p then p else p in
            pure $ Node p' (M.fromList ls)



--------------------------------------------------------------------------------
-- Misc helpers
--------------------------------------------------------------------------------

-- INEFFICIENT implementation:
-- Lame that this isn't in Data.Map.
unionWithM :: (Monad m, Ord k) => (a -> a -> m a) ->
              M.Map k a -> M.Map k a -> m (M.Map k a)
unionWithM f m1 m2 = sequence m'
 where
  m' = M.unionWith (\a b -> do a' <- a
                               b' <- b
                               f a' b')
        (M.map pure m1) (M.map pure m2)


--------------------------------------------------------------------------------
-- Testing infrastructure
--------------------------------------------------------------------------------

-- FIXME: should probably generate these just by randomly checking
-- out different paths.  OTHERWISE, we need a way of filtering to
-- assert the representational invariants.
instance Arbitrary Perms where
--  arbitrary = sized arbPerm
  arbitrary = sized arbPerm2

  shrink (Has _) = []
  -- TODO:
  shrink (Node p ch) = [ Node p ch' | ch' <- shrink ch ]

-- | Produce a completely random permission... this is tricky as it needs to
-- respect invariants (FIXME).

arbPerm :: Int -> Gen Perms
arbPerm 0 = Has <$> arbitrary
arbPerm n = do
  (Positive m) <- arbitrary
  let n' = n `div` (m + 1)
  chars <- replicateM m arbitrary
  vals  <- replicateM m (arbPerm n')
  p     <- arbitrary
  return $ Node p (M.fromList [([c],v) | (c,v) <- zip chars vals])

-- | This version instead builds a permission by performing checkouts of random
-- leaf permissions.
arbPerm2 :: Int -> Gen Perms
-- arbPerm2 0 = Has <$> arbitrary
arbPerm2 0 = return $ Has (RW 1)
arbPerm2 n = fromRight <$> act
              `suchThat` (\x -> -- trace ("Trying "++show x) $
                          isRight x)
 where
 act = do atoms :: [PathPerm] <- replicateM n arbitrary
          return $
           -- trace ("arbPerm2: joining atoms "++show atoms)$
           foldM (\acc at -> let IP pp = fromPathPerm at in
                             addPerms' acc pp) allPerms atoms


-- FIXME: this version diverges:
arbPerm3 :: Int -> Gen Perms
arbPerm3 n | n < 1 = trace "Bottom out"$ return (Has (RW 1))
arbPerm3 n = do trace "ArbPerm2" $ return ()
                p <- arbPerm3 (n-1)
                trace "Recur finished, now extend." $ return ()
                atom <- arbitrary `suchThat`
                        (\x ->
                          trace ("Trying "++show x++"\n to join into "++show p)$
                          isRight (addPerms' p x))
                let Right p' = addPerms' p atom
                return p'


fromRight :: Either a b -> b
fromRight x = let (Right y) = x in y

-- TODO: arbitrary fractions
instance Arbitrary Perm where
  arbitrary = do
    b <- arbitrary
    if b then return (R 1)
         else return (RW 1)

instance Arbitrary PathPerm where
  arbitrary = sized arbPathPerm

-- | Create a random permission with one-letter dirs, such as
arbPathPerm :: Int -> Gen PathPerm
arbPathPerm 0 = do
--  p <- arbitrary
--  return $ PathPerm [] p

-- FIXME: need to make both read and write perms work in arbitrary:
  return $ WritePath []

arbPathPerm n = do
  pp <- arbPathPerm (n-1)
  let ls = pathparts pp
  b  <- arbitrary
  -- With 50% probability pick 'a' to get more sharing.
  if b then do
    return $ pp { pathparts = ("a":ls) }
   else do
    Positive n  <- arbitrary
    return $ pp { pathparts =
                   ([chr (start + n `mod` (end-start+1))]:ls) }
 where
  start = ord 'b'
  end   = ord 'z'
