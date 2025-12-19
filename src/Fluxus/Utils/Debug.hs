{-# LANGUAGE OverloadedStrings #-}

-- | Debug utilities for Fluxus compiler
module Fluxus.Utils.Debug
  ( debugLog
  , debugTrace
  , debugBreak
  , debugWith
  , debugAssert
  , debugTimer
  , debugCallStack
  , debugMemory
  , DebugLevel(..)
  , withDebugLevel
  , setDebugLevel
  , getDebugLevel
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import System.Environment (lookupEnv)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO.Unsafe (unsafePerformIO)

-- Global debug level reference
{-# NOINLINE debugLevelRef #-}
debugLevelRef :: IORef (Maybe DebugLevel)
debugLevelRef = unsafePerformIO $ newIORef Nothing

-- | Debug level for controlling verbosity
data DebugLevel
  = None    -- ^ No debug output
  | Error   -- ^ Only error messages
  | Warning -- ^ Warnings and errors
  | Info    -- ^ Info, warnings, and errors
  | Debug   -- ^ All debug messages
  | Trace   -- ^ Most verbose output
  deriving (Eq, Ord, Show, Read)

-- | Get debug level from environment variable FLUXUS_DEBUG
getEnvDebugLevel :: IO DebugLevel
getEnvDebugLevel = do
  result <- lookupEnv "FLUXUS_DEBUG"
  case result of
    Just "none" -> pure None
    Just "error" -> pure Error
    Just "warning" -> pure Warning
    Just "info" -> pure Info
    Just "debug" -> pure Debug
    Just "trace" -> pure Trace
    _ -> pure Warning  -- Default level

-- | Log a debug message if the current level allows it
debugLog :: MonadIO m => DebugLevel -> Text -> m ()
debugLog level msg = do
  currentLevel <- liftIO getEnvDebugLevel
  if level <= currentLevel
  then liftIO $ do
    TIO.putStr $ "[" <> T.pack (show level) <> "] " <> msg <> "\n"
    hFlush stdout
  else pure ()

-- | Trace execution with a message
debugTrace :: MonadIO m => Text -> m a -> m a
debugTrace msg action = do
  debugLog Trace $ "TRACE: " <> msg
  action

-- | Breakpoint for debugging
debugBreak :: MonadIO m => Text -> m ()
debugBreak msg = do
  debugLog Debug $ "BREAKPOINT: " <> msg
  liftIO $ do
    TIO.putStr "Press Enter to continue..."
    hFlush stdout
    _ <- getLine
    pure ()

-- | Execute an action with debug logging
debugWith :: MonadIO m => DebugLevel -> Text -> m a -> m a
debugWith level msg action = do
  debugLog level $ "ENTER: " <> msg
  result <- action
  debugLog level $ "EXIT: " <> msg
  pure result

-- | Set debug level for the current session
setDebugLevel :: DebugLevel -> IO ()
setDebugLevel level = do
  writeIORef debugLevelRef (Just level)
  putStrLn $ "Setting debug level to: " ++ show level

-- | Get current debug level
getDebugLevel :: IO DebugLevel
getDebugLevel = do
  envLevel <- getEnvDebugLevel
  refLevel <- readIORef debugLevelRef
  case refLevel of
    Just level -> return $ max envLevel level  -- Use the higher of the two levels
    Nothing -> return envLevel  -- Only use environment level if not set

-- | Execute action with temporary debug level
withDebugLevel :: DebugLevel -> IO a -> IO a
withDebugLevel level action = do
  oldLevel <- readIORef debugLevelRef
  writeIORef debugLevelRef (Just level)
  result <- action
  writeIORef debugLevelRef oldLevel
  pure result

-- | Debug assertion - fails with error message if condition is false
debugAssert :: MonadIO m => Bool -> Text -> m ()
debugAssert condition msg = do
  if not condition
  then debugLog Error $ "ASSERTION FAILED: " <> msg
  else pure ()

-- | Time an operation and log the duration
debugTimer :: MonadIO m => Text -> m a -> m a
debugTimer name action = do
  debugLog Debug $ "TIMER START: " <> name
  start <- liftIO $ getCurrentTime
  result <- action
  end <- liftIO $ getCurrentTime
  let duration = realToFrac $ diffUTCTime end start :: Double
  debugLog Debug $ "TIMER END: " <> name <> " (" <> T.pack (show duration) <> "s)"
  pure result

-- | Log current call stack location
debugCallStack :: MonadIO m => Text -> m ()
debugCallStack context = do
  debugLog Trace $ "CALL STACK: " <> context

-- | Log memory usage information
debugMemory :: MonadIO m => m ()
debugMemory = do
  debugLog Trace "Memory usage information would be displayed here"
  -- In a real implementation, you could use System.Mem or foreign calls
  -- to get actual memory usage statistics