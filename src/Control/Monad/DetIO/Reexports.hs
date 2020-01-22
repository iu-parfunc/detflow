{-# LANGUAGE NamedFieldPuns #-}
-- | Wrappers that simply reexport IO functionality.

module Control.Monad.DetIO.Reexports where

import           Control.Monad
import           Control.Monad.DetIO.Perms
import           System.Log.TSLogger (dbgLvl)
import           Control.Monad.DetIO.Unsafe
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List (sort)
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified Control.Monad.State.Strict as S
import qualified System.Environment as Env
import           Text.Show.Pretty (ppShow)

--------------------------------------------------------------------------------

dbg :: Perms -> String
dbg p =
  if dbgLvl >= 1
  -- TODO: when we have a prettier way of printing perms, expose this unconditionally:
  then "\nGiven current permissions on thread:\n  "++ppShow p
  else ""

-- | Deterministically read a file, which may or may not be accessible
-- from other threads.
readFile :: FilePath -> DetIO ByteString
readFile fp0 = do
  fp <- lio $ Dir.makeAbsolute fp0
  TS{perms} <- DetIO S.get
  unless (canRead fp perms) $ detIOFail $ "Reading a file without permission: "++show fp++dbg perms
  lio $ BS.readFile fp

-- | Deterministically write a file (which no other thread currently has access to).
writeFile :: FilePath -> ByteString -> DetIO ()
writeFile fp0 bs = do
  fp <- lio $ Dir.makeAbsolute fp0
  TS{perms} <- DetIO S.get
  unless (canWrite fp perms) $
     detIOFail ("writeFile without permission:\n  "++fp++dbg perms)
  unless (canWrite (FP.takeDirectory fp) perms) $
     detIOFail ("writeFile without write access to parent directory:\n  "++(FP.takeDirectory fp)++dbg perms)
  lio $ BS.writeFile fp bs

-- | Deterministically read the contents of a directory, including
-- special files "." and "..".
getDirectoryContents :: FilePath -> DetIO [FilePath]
getDirectoryContents fp0 = do
  fp <- lio $ Dir.makeAbsolute fp0
  TS{perms} <- DetIO S.get
  unless (canRead fp perms) $
     detIOFail ("Reading a directory without permission"++fp++dbg perms)
  fmap sort $ lio $ Dir.getDirectoryContents fp

-- | Deterministically read the contents of a directory.
listDirectory :: FilePath -> DetIO [FilePath]
listDirectory fp0 = do
  fp <- lio $ Dir.makeAbsolute fp0
  TS{perms} <- DetIO S.get
  unless (canRead fp perms) $
     detIOFail ("Reading a directory without permission"++fp++dbg perms)
  fmap sort $ lio $ Dir.listDirectory fp


createDirectory :: FilePath -> DetIO ()
createDirectory fp0 = do
  fp <- lio $ Dir.makeAbsolute fp0
  TS{perms} <- DetIO S.get
  unless (canWrite fp perms) $
     detIOFail ("createDirectory without permission:\n  "++fp++dbg perms)
  unless (canWrite (FP.takeDirectory fp) perms) $
     detIOFail ("createDirectory without write access to parent directory:\n  "
                ++(FP.takeDirectory fp)++dbg perms)
  lio $ Dir.createDirectory fp

createDirectoryIfMissing :: Bool -> FilePath -> DetIO ()
createDirectoryIfMissing True _fp0 =
    error "DetIO createDirectoryIfMissing does not yet handle True argument."
createDirectoryIfMissing False fp0 = do
  fp <- lio $ Dir.makeAbsolute fp0
  TS{perms} <- DetIO S.get
  unless (canWrite fp perms) $
     detIOFail ("createDirectoryIfMissing without permission:\n  "++fp++dbg perms)
  unless (canWrite (FP.takeDirectory fp) perms) $
     detIOFail ("createDirectoryIfMissing without write access to parent directory:\n  "
                ++(FP.takeDirectory fp)++dbg perms)
  lio $ Dir.createDirectoryIfMissing False fp


removeDirectoryRecursive :: FilePath -> DetIO ()
removeDirectoryRecursive fp0 = do
  fp <- lio $ Dir.makeAbsolute fp0
  TS{perms} <- DetIO S.get
  unless (canWrite fp perms) $ -- Subumes read permission (for children)
     detIOFail ("removeDirectoryRecursive without permission:\n  "++fp++dbg perms)
  lio $ Dir.removeDirectoryRecursive fp

copyFile :: FilePath -> FilePath -> DetIO ()
copyFile a0 b0 = do
  a <- lio $ Dir.makeAbsolute a0
  b <- lio $ Dir.makeAbsolute b0
  TS{perms} <- DetIO S.get
  unless (canRead a perms) $
     detIOFail ("copyFile without read permisson on destination:\n  "++a++dbg perms)
  unless (canWrite b perms) $
     detIOFail ("copyFile without write permisson on destination:\n  "++b++dbg perms)
  unless (canWrite (FP.takeDirectory b) perms) $
     detIOFail ("copyFile without write access to parent directory:\n  "++(FP.takeDirectory b)++dbg perms)
  lio $ Dir.copyFile a b


removeFile :: FilePath -> DetIO ()
removeFile fp0 = do
  fp <- lio $ Dir.makeAbsolute fp0
  TS{perms} <- DetIO S.get
  unless (canWrite fp perms) $ -- Subumes read permission (for children)
     detIOFail ("removeFile without permission:\n  "++fp ++dbg perms)
  unless (canWrite (FP.takeDirectory fp) perms) $
     detIOFail ("removeFile without write access to parent directory:\n  "++(FP.takeDirectory fp) ++ dbg perms)
  lio $ Dir.removeFile fp


doesDirectoryExist :: FilePath -> DetIO Bool
doesDirectoryExist fp0 = do
  fp <- lio $ Dir.makeAbsolute fp0
  TS{perms} <- DetIO S.get
  unless (canRead fp perms) $
     detIOFail ("Attempted doesDirectoryExist without read permission on path: "++fp ++ dbg perms)
  lio $ Dir.doesDirectoryExist fp

doesFileExist :: FilePath -> DetIO Bool
doesFileExist fp0 = do
  fp <- lio $ Dir.makeAbsolute fp0
  TS{perms} <- DetIO S.get
  unless (canRead fp perms) $
     detIOFail ("Attempted doesFileExist without read permission on path: "++fp ++ dbg perms)
  lio $ Dir.doesFileExist fp

doesPathExist :: FilePath -> DetIO Bool
doesPathExist fp0 = do
  fp <- lio $ Dir.makeAbsolute fp0
  TS{perms} <- DetIO S.get
  unless (canRead fp perms) $
     detIOFail ("Attempted doesPathExist without read permission on path: "++fp ++ dbg perms)
  lio $ Dir.doesPathExist fp

findFile :: [FilePath] -> String -> DetIO (Maybe FilePath)
findFile dirs fileName = do
  dirs'     <- lio $ mapM Dir.makeAbsolute dirs
  fileName' <- lio $ Dir.makeAbsolute fileName
  TS{perms} <- DetIO S.get
  forM_ dirs' $ \dir' ->
    unless (canRead dir' perms) $
      detIOFail ("Attempted findFile without read permission on containing directory: " ++ dir' ++ dbg perms)
  unless (canRead fileName' perms) $
    detIOFail ("Attempted findFile without read permission on file: " ++ fileName ++ dbg perms)
  lio $ Dir.findFile dirs' fileName'

makeAbsolute :: FilePath -> DetIO FilePath
makeAbsolute fp = lio $ Dir.makeAbsolute fp

getArgs :: DetIO [String]
getArgs = lio Env.getArgs

-- | We don't allow concurrently mutating the environment, so it
-- remains deterministic for the duration of the Haskell execution.
getEnvironment :: DetIO [(String,String)]
getEnvironment = lio Env.getEnvironment


getCurrentDirectory :: DetIO FilePath
getCurrentDirectory = lio Dir.getCurrentDirectory

-- setCurrentDirectory may only be used in SEQUENTIAL regions of the program.

