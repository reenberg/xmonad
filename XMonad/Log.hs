module XMonad.Log
       ( infoX
       , debugX
       , warningX
       , errorX
       , criticalX
       , abortX)
       where

import System.Log.Logger (logM, Priority(..))

import Control.Monad.State


-- | Main log function
logX :: MonadIO m => Priority -> String -> String -> m ()
logX prio name msg =
  liftIO $ logM name prio msg


-- | Logging with various importance
infoX, debugX, warningX, errorX, criticalX :: MonadIO m => String -> String -> m ()

infoX = logX INFO
debugX = logX DEBUG
warningX = logX WARNING
errorX = logX ERROR
criticalX = logX CRITICAL

-- | Abort execution, yielding a critical log entry and an error
abortX :: MonadIO m => String -> String -> m a
abortX name msg =
  do criticalX name msg
     error $ "xmonad: " ++ name ++ ": " ++ msg