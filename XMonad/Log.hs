module XMonad.Log
       ( infoX
       , debugX
       , warningX
       , errorX
       , criticalX
       , abortX
       , abortX'
       , setupLogger)
       where

import System.IO (stderr)
import System.FilePath ((</>))

import System.IO.Unsafe (unsafePerformIO) -- used when aborting

import System.Log.Logger (Priority(..), logM, setHandlers, updateGlobalLogger, rootLoggerName)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Formatter (simpleLogFormatter)

import Control.Monad.State


-- | Main log function
logX :: MonadIO m => Priority -> String -> String -> m ()
logX prio name msg =
  liftIO $ logM name prio msg


-- | Logging with various importance
debugX, infoX, warningX, errorX, criticalX :: MonadIO m => String -> String -> m ()

-- This is the ordering of importance, with CRITICAL being the most important.
debugX = logX DEBUG
infoX = logX INFO
warningX = logX WARNING
errorX = logX ERROR
criticalX = logX CRITICAL

-- | Abort execution, yielding a critical log entry and an error
abortX :: MonadIO m => String -> String -> m a
abortX name msg =
  do criticalX name msg
     error $ "xmonad: " ++ name ++ ": " ++ msg

-- | Abort execution outside MonadIO
abortX' :: String -> String -> a
abortX' name msg =
  -- force execution of abortX
  (unsafePerformIO $ abortX name msg) `seq` abortX' name msg


-- | Setup a logger in $XMonad/xmonad.log and on stderr
setupLogger :: MonadIO m => String -> m ()
setupLogger dir = liftIO $
  do fileH   <- fileHandler   (dir </> logFile) WARNING -- TODO:  This should be defined by the user config, and not harcoded
     streamH <- streamHandler stderr            WARNING
     updateGlobalLogger rootLoggerName $ setHandlers $
       map (flip setFormatter $ format) [streamH, fileH]
  where
    format  = simpleLogFormatter "$time, $loggername [$prio]: $msg"
    logFile = "xmonad.log"