{-# LANGUAGE Trustworthy #-}
module Control.Monad.DetIO
    ( -- * Types
      DetIO
    , Pedigree -- , PedStep(..)
    , IVar
    , Thread

      -- * Permissions
    , PathPerm(..)
    , Perms
    , mkPermR, mkPermRW

      -- * DetIO basics
    , runDetIO
    , runDetIOWith
    , initPerms

      -- * Concurrent child threads
    , forkIO
    , forkWithPerms
    , joinThread, joinThreads, waitThread
--    , getInitPermsFromHarness

      -- * Shelling out, deterministically
    , readProcess
    , callProcess
    , callCommand
    , system
    , rawSystem
    , readCreateProcess
    , proc, shell, CreateProcess(..)
    , readShell, callCreateProcess
      
     -- * Internal
    , getWhitelist

      -- * IVar primitives
    , new, put, get

    -- * Permissions
    , canRead
    , canWrite
    -- , getMainInDir
    -- , getMainOutDir

      -- * Operations
    , DetIO.putStrLn
    , DetIO.putTextLn
    , DetIO.putStr
    , DetIO.putText
    , DetIO.readFile
    , DetIO.writeFile
    , getDirectoryContents
    , listDirectory
    , createDirectory
    , createDirectoryIfMissing
    , removeDirectoryRecursive
    , copyFile
    , removeFile
    , doesFileExist
    , doesDirectoryExist
    , doesPathExist
    , makeAbsolute
    , getArgs
    , getEnvironment
    , getCurrentDirectory
    , findFile
    )
 where

import Control.Monad.DetIO.System
import Control.Monad.DetIO.Perms
import Control.Monad.DetIO.Unsafe    as DetIO
import Control.Monad.DetIO.Reexports as DetIO
