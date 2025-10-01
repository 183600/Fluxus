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

-- | Debug state for managing breakpoints and logging
data DebugState = DebugState
  { dsBreakpoints :: !(IORef [Text])  -- Active breakpoints
  , dsDebugEnabled :: !(IORef Bool)   -- Global debug enable flag
  }

-- | Initialize debug state
initDebugState :: IO DebugState
initDebugState = do
  breakpoints <- newIORef []
  enabled <- newIORef False
  
  -- Check if debug mode should be enabled by environment variable
  debugEnv <- lookupEnv "FLUXUS_DEBUG"
  case debugEnv of
    Just "1" -> writeIORef enabled True
    Just "true" -> writeIORef enabled True
    _ -> return ()
  
  return $ DebugState breakpoints enabled

-- | Enable debug logging
enableDebug :: MonadIO m => m ()
enableDebug = liftIO $ do
  state <- initDebugState
  writeIORef (dsDebugEnabled state) True

-- | Disable debug logging
disableDebug :: MonadIO m => m ()
disableDebug = liftIO $ do
  state <- initDebugState
  writeIORef (dsDebugEnabled state) False

-- | Check if debug is enabled
isDebugEnabled :: MonadIO m => m Bool
isDebugEnabled = liftIO $ do
  state <- initDebugState
  readIORef (dsDebugEnabled state)

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
setBreakpoint name = liftIO $ do
  state <- initDebugState
  modifyIORef (dsBreakpoints state) (name :)
  debugLog $ "Breakpoint set: " <> name

-- | Check if a breakpoint is active and wait for user input
checkBreakpoint :: MonadIO m => Text -> m ()
checkBreakpoint name = do
  state <- liftIO initDebugState
  breakpoints <- liftIO $ readIORef (dsBreakpoints state)
  enabled <- isDebugEnabled
  
  when (enabled && name `elem` breakpoints) $ do
    debugLog $ "Breakpoint hit: " <> name
    debugLog "Press Enter to continue, 'q' to quit debugging..."
    
    input <- liftIO getLine
    when (input == "q") $ do
      disableDebug
      debugLog "Debugging disabled"

-- | Clear a specific breakpoint
clearBreakpoint :: MonadIO m => Text -> m ()
clearBreakpoint name = liftIO $ do
  state <- initDebugState
  modifyIORef (dsBreakpoints state) (filter (/= name))
  debugLog $ "Breakpoint cleared: " <> name

-- | Clear all breakpoints
clearAllBreakpoints :: MonadIO m => m ()
clearAllBreakpoints = liftIO $ do
  state <- initDebugState
  writeIORef (dsBreakpoints state) []
  debugLog "All breakpoints cleared"