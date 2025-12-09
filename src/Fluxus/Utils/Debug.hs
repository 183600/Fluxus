{-# LANGUAGE OverloadedStrings #-}

-- | Debug utilities for Fluxus compiler
module Fluxus.Utils.Debug
  ( debugLog
  , debugTrace
  , debugBreak
  , debugWith
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
  putStrLn $ "Setting debug level to: " ++ show level
  -- In a real implementation, you might store this in an IORef or similar

-- | Get current debug level
getDebugLevel :: IO DebugLevel
getDebugLevel = getEnvDebugLevel

-- | Execute action with temporary debug level
withDebugLevel :: DebugLevel -> IO a -> IO a
withDebugLevel level action = do
  oldLevel <- getDebugLevel
  setDebugLevel level
  result <- action
  setDebugLevel oldLevel
  pure result