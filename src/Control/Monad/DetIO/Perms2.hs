-- | A version of permissions that uses functions only.

module Control.Monad.DetIO.Perms2
    (
    -- * Individual permissions values
      Perm, noPerm
       -- TODO!  HIDE THESE.

    -- * Single-path perms
    , PathPerm, mkPermR, mkPermRW
    , P.fromPathPerm, P.pathPermToPath

    -- * Collective permissions
    , InitialPerms, P.Frac, (\/)
    , Perms, allPerms, emptyPerms
--    , startingPerms
--    , partitionPerms

    -- * Operations on permissions
    , checkout
    , addPerms
--    , canRead, canWrite, canExtendDir
{-
    -- * Testing only
    -- TODO: Hide these.
    , PathPerm(..), Perms(..), InitialPerms(..)
    , arbPerm2, joinPerm
    , normPerms, addPerms'     
-}
    )
    where

import           Algebra.Lattice
-- import           System.FilePath
import Control.Monad.DetIO.Perms (PathPerm, mkPermR, mkPermRW)
import qualified Control.Monad.DetIO.Perms as P

--------------------------------------------------------------------------------
    
type FileName = String

-- | The boolean indicates whether the permission includes write access.
type Perm = (P.Frac,Bool)
type Perms = FilePath -> Perm

newtype InitialPerms = IP Perms

joinPerm :: Perm -> Perm -> Perm
joinPerm (f1,a) (f2,b) = (max f1 f2, a || b)

instance JoinSemiLattice InitialPerms where
  IP f \/ IP g = IP (\p -> f p `joinPerm` g p)

-- | The root permission, RW access to "/".
allPerms :: Perms
allPerms _ = (1,True)

-- | Empty permissions.
emptyPerms :: Perms
emptyPerms _ = noPerm

-- | The minimum permission: with zero fraction.
noPerm :: Perm
noPerm = (0, False)

instance BoundedJoinSemiLattice InitialPerms where
  bottom = IP emptyPerms

-- | Meet is permissions *intersection*.
instance MeetSemiLattice InitialPerms where
  _a /\ _b = error "FINISHME - meet operator"

instance BoundedMeetSemiLattice InitialPerms where
  top = IP allPerms


checkout :: PathPerm -> Perms -> Either String (Perms,Perms)
checkout pperm fn =
    Right (parent,child)
  where
    path = P.pathPermToPath pperm
    (parent,child) =
      case pperm of
        -- Write checkouts take ALL:
        P.WritePath{} -> (\p -> if isUnder p path then noPerm  else fn path,
                          \p -> if isUnder p path then fn path else noPerm)
        -- Read checkouts are symmetric:
        P.ReadPath{}  ->
            let f p = if isUnder p path
                      then halvePerm (fn path)
                      else fn path
            in (f,f)

--    isWrite = case pperm of P.ReadPath{} -> False; P.WritePath{} -> True

halvePerm :: Perm -> Perm
halvePerm (p,b) = (P.halve p, b)

-- | Is the first path somewhere at or under the second path.
--   I.e. is the second path a prefix of the first?
isUnder :: FilePath -> FilePath -> Bool
isUnder = undefined

addPerms :: Perms -> Perms -> Either String Perms
addPerms f g = Right $ \ p ->
  let (x,a) = f p
      (y,b) = g p
  in (x+y, a||b)


partitionPerms :: Perms -> ([FileName], [FileName])
partitionPerms _ = error "Cannot implement partition perms for function representations!"


                   
