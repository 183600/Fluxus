{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simple debugging and logging utilities for Fluxus compiler
module Fluxus.Debug.Logger
  ( -- * Logging functions
    debugLog
  , debugLogM
  , enableDebug
  , disableDebug
  , isDebugEnabled
    -- * Breakpoint functions
  , setBreakpoint
  , checkBreakpoint
  , clearBreakpoint
  , clearAllBreakpoints
    -- * Debug state
  , DebugState
  , initDebugState
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.IO (hFlush, stdout)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- Global debug state stored in top-level IORefs to persist across calls
{-# NOINLINE globalBreakpoints #-}
globalBreakpoints :: IORef [Text]
globalBreakpoints = unsafePerformIO (newIORef [])

{-# NOINLINE globalDebugEnabled #-}
globalDebugEnabled :: IORef Bool
globalDebugEnabled = unsafePerformIO $ do
  ref <- newIORef False
  debugEnv <- lookupEnv "FLUXUS_DEBUG"
  case debugEnv of
    Just "1" -> writeIORef ref True
    Just "true" -> writeIORef ref True
    _ -> return ()
  return ref


-- | Debug state for managing breakpoints and logging
data DebugState = DebugState
  { dsBreakpoints :: !(IORef [Text])  -- Active breakpoints
  , dsDebugEnabled :: !(IORef Bool)   -- Global debug enable flag
  }

-- | Initialize debug state (returns handles to global refs now)
initDebugState :: IO DebugState
initDebugState = pure $ DebugState globalBreakpoints globalDebugEnabled

-- | Enable debug logging
enableDebug :: MonadIO m => m ()
enableDebug = liftIO $ writeIORef globalDebugEnabled True

-- | Disable debug logging
disableDebug :: MonadIO m => m ()
disableDebug = liftIO $ writeIORef globalDebugEnabled False

-- | Check if debug is enabled
isDebugEnabled :: MonadIO m => m Bool
isDebugEnabled = liftIO $ readIORef globalDebugEnabled

-- | Simple debug logging function
debugLog :: MonadIO m => Text -> m ()
debugLog msg = do
  enabled <- isDebugEnabled
  when enabled $ do
    now <- liftIO getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%H:%M:%S.%q" now
    liftIO $ putStrLn $ "[DEBUG " ++ timeStr ++ "] " ++ T.unpack msg
    liftIO $ hFlush stdout

-- | Debug logging in a monadic context
debugLogM :: MonadIO m => Text -> m ()
debugLogM msg = do
  enabled <- isDebugEnabled
  when enabled $ debugLog msg

-- | Set a breakpoint
setBreakpoint :: MonadIO m => Text -> m ()
setBreakpoint name = liftIO $ modifyIORef globalBreakpoints (name :)

-- | Check if a breakpoint is active and wait for user input
checkBreakpoint :: MonadIO m => Text -> m ()
checkBreakpoint name = do
  breakpoints <- liftIO $ readIORef globalBreakpoints
  enabled <- isDebugEnabled
  when (enabled && name `elem` breakpoints) $ liftIO $ do
    now <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%H:%M:%S.%q" now
    putStrLn $ "[DEBUG " ++ timeStr ++ "] Breakpoint hit: " ++ T.unpack name
    putStrLn "Press Enter to continue, 'q' to quit debugging..."
    hFlush stdout
    input <- getLine
    when (input == "q") $ do
      writeIORef globalDebugEnabled False
      putStrLn $ "[DEBUG " ++ timeStr ++ "] Debugging disabled"
      hFlush stdout

-- | Clear a specific breakpoint
clearBreakpoint :: MonadIO m => Text -> m ()
clearBreakpoint name = liftIO $ modifyIORef globalBreakpoints (filter (/= name))

-- | Clear all breakpoints
clearAllBreakpoints :: MonadIO m => m ()
clearAllBreakpoints = liftIO $ writeIORef globalBreakpoints []