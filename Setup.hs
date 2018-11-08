import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity

import System.FilePath
import System.Process (callProcess)

libdet, libdetC, libdetSO :: FilePath
libdet   = "cbits" </> "libdet"
libdetC  = libdet <.> "c"
libdetSO = libdet <.> "so"

detMonadBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags
                  -> IO ()
detMonadBuildHook pkgDesc lbi hooks buildFlags = do
  buildSO (\pd -> buildHook simpleUserHooks pd lbi hooks buildFlags)
          (fromFlag $ buildVerbosity buildFlags) pkgDesc

detMonadReplHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> ReplFlags
                 -> [String] -> IO ()
detMonadReplHook pkgDesc lbi hooks replFlags args = do
  buildSO (\pd -> replHook simpleUserHooks pd lbi hooks replFlags args)
          (fromFlag $ replVerbosity replFlags) pkgDesc

detMonadCopyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags
                 -> IO ()
detMonadCopyHook pkgDesc lbi hooks copyFlags =
  copySOToDataFiles (\pd -> copyHook simpleUserHooks pd lbi hooks copyFlags)
                    (fromFlag $ copyVerbosity copyFlags) pkgDesc

detMonadInstHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags
                 -> IO ()
detMonadInstHook pkgDesc lbi hooks instFlags =
  copySOToDataFiles (\pd -> instHook simpleUserHooks pd lbi hooks instFlags)
                    (fromFlag $ installVerbosity instFlags) pkgDesc

buildSO :: (PackageDescription -> IO ())
        -> Verbosity -> PackageDescription -> IO ()
buildSO act verbosity pkgDesc = do
  notice verbosity $ "Building libdet.so"
  (gcc, _) <- requireProgram verbosity gccProgram defaultProgramDb
  runProgram verbosity gcc $ words $
    "-std=c99 -Wall -fPIC -shared -Wl,--no-as-needed -o " ++
    libdetSO ++ " " ++ libdetC ++ " " ++ "-ldl -lm"
  copySOToDataFiles act verbosity pkgDesc

copySOToDataFiles :: (PackageDescription -> IO ())
                  -> Verbosity -> PackageDescription -> IO ()
copySOToDataFiles act verbosity pkgDesc = do
  notice verbosity $ "Copying " ++ libdetSO ++ " to data-files"
  let pkgDesc' = pkgDesc { dataFiles = libdetSO : dataFiles pkgDesc }
  act pkgDesc'

detMonadPostClean :: Args -> CleanFlags -> PackageDescription -> ()
                  -> IO ()
detMonadPostClean args cleanFlags pkgDesc lbi = do
  let verbosity = fromFlag $ cleanVerbosity cleanFlags
  notice verbosity $ "Removing libdet.so"
  callProcess "rm" ["-f", libdetSO]

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = detMonadBuildHook
  , replHook  = detMonadReplHook
  , postClean = detMonadPostClean
  , copyHook  = detMonadCopyHook
  , instHook  = detMonadInstHook
  }
