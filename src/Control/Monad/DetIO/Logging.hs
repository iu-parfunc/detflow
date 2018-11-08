-- | 

module Control.Monad.DetIO.Logging
    (
     -- * The global logger
     glog
     -- * Handy reexports
     , logOn, logStrLn, logTextLn, logByteStringLn
     , dbgLvl
    ) where

import System.Log.TSLogger
import System.IO.Unsafe
import System.IO

-- | Create one global logger for detmonad.
glog :: Logger
glog = unsafePerformIO $
       newLogger (0,dbgLvl)
         [ -- OutputEvents,
          OutputTo stdout ]
         DontWait
