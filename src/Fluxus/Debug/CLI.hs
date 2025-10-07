{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Command-line debugging interface for Fluxus compiler
module Fluxus.Debug.CLI
  ( -- * Main debugging interface
    runDebugCLI
  , debugCommandLoop
    -- * Debug commands
  , DebugCommand(..)
  , parseDebugCommand
  , executeDebugCommand
    -- * Debug context
  , DebugContext(..)
  , initDebugContext
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when)
import Control.Monad.State (MonadState, get, modify, evalStateT)
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.Haskeline
import Data.Maybe ()

import Fluxus.Debug.Logger
import Fluxus.Compiler.Driver (runCompiler, compileFileWithOptions, CompileOptions(..), defaultCompileOptions, defaultConfig)

-- | Debug commands available in the CLI
data DebugCommand
  = DebugHelp
  | DebugStep  -- Step to next compilation phase
  | DebugContinue -- Continue execution
  | DebugBreak Text -- Set breakpoint
  | DebugClear Text -- Clear breakpoint
  | DebugList -- List breakpoints
  | DebugStatus -- Show debug status
  | DebugCompile FilePath -- Compile with debugging
  | DebugQuit
  | DebugUnknown Text
  deriving (Show, Eq)

-- | Debug context maintaining current state
data DebugContext = DebugContext
  { dcCurrentPhase :: Text
  , dcBreakpoints :: [Text]
  , dcStepMode :: Bool
  , dcLastResult :: Maybe Text
  }

-- | Initialize debug context
initDebugContext :: DebugContext
initDebugContext = DebugContext
  { dcCurrentPhase = "idle"
  , dcBreakpoints = []
  , dcStepMode = False
  , dcLastResult = Nothing
  }

-- | Parse debug command from user input
parseDebugCommand :: Text -> DebugCommand
parseDebugCommand input = case T.words input of
  [] -> DebugUnknown ""
  ("help":_) -> DebugHelp
  ("h":_) -> DebugHelp
  ("step":_) -> DebugStep
  ("s":_) -> DebugStep
  ("continue":_) -> DebugContinue
  ("c":_) -> DebugContinue
  ("break":name:_) -> DebugBreak name
  ("b":name:_) -> DebugBreak name
  ("clear":name:_) -> DebugClear name
  ("list":_) -> DebugList
  ("l":_) -> DebugList
  ("status":_) -> DebugStatus
  ("st":_) -> DebugStatus
  ("compile":file:_) -> DebugCompile (T.unpack file)
  ("quit":_) -> DebugQuit
  ("q":_) -> DebugQuit
  _ -> DebugUnknown input

-- | Execute a debug command
executeDebugCommand :: (MonadIO m, MonadState DebugContext m) => DebugCommand -> m Bool
executeDebugCommand cmd = case cmd of
  DebugHelp -> do
    liftIO $ putStrLn $ unlines
      [ "Fluxus Debugger Commands:"
      , "  help, h          - Show this help"
      , "  step, s          - Step to next compilation phase"
      , "  continue, c      - Continue execution"
      , "  break <name>, b  - Set breakpoint at phase/name"
      , "  clear <name>     - Clear breakpoint"
      , "  list, l          - List active breakpoints"
      , "  status, st       - Show debug status"
      , "  compile <file>   - Compile file with debugging"
      , "  quit, q          - Exit debugger"
      ]
    return True
    
  DebugStep -> do
    modify $ \ctx -> ctx { dcStepMode = True }
    debugLog "Step mode enabled"
    return True
    
  DebugContinue -> do
    modify $ \ctx -> ctx { dcStepMode = False }
    debugLog "Continuing execution"
    return True
    
  DebugBreak name -> do
    modify $ \ctx -> ctx { dcBreakpoints = name : dcBreakpoints ctx }
    debugLog $ "Breakpoint set: " <> name
    return True
    
  DebugClear name -> do
    modify $ \ctx -> ctx { dcBreakpoints = filter (/= name) (dcBreakpoints ctx) }
    debugLog $ "Breakpoint cleared: " <> name
    return True
    
  DebugList -> do
    ctx <- get
    if null (dcBreakpoints ctx)
      then debugLog "No active breakpoints"
      else debugLog $ "Active breakpoints: " <> T.pack (show (dcBreakpoints ctx))
    return True
    
  DebugStatus -> do
    ctx <- get
    debugLog $ "Current phase: " <> dcCurrentPhase ctx
    debugLog $ "Step mode: " <> T.pack (show (dcStepMode ctx))
    debugLog $ "Breakpoints: " <> T.pack (show (dcBreakpoints ctx))
    case dcLastResult ctx of
      Just result -> debugLog $ "Last result: " <> result
      Nothing -> debugLog "No last result"
    return True
    
  DebugCompile file -> do
    debugLog $ "Compiling file: " <> T.pack file
    -- Set up compile options with debugging enabled
    let options = defaultCompileOptions
          { coSourceFile = file
          , coEnableDebugInfo = True
          , coVerboseLevel = 2
          }

    result <- liftIO $ runCompiler defaultConfig (compileFileWithOptions options)
    case result of
      Left err -> do
        debugLog $ "Compilation failed: " <> T.pack (show err)
        modify $ \ctx -> ctx { dcLastResult = Just "compilation failed" }
      Right (output, _) -> do
        debugLog $ "Compilation successful: " <> T.pack output
        modify $ \ctx -> ctx { dcLastResult = Just "compilation successful" }
    return True
    
  DebugQuit -> do
    debugLog "Exiting debugger"
    return False
    
  DebugUnknown input -> do
    debugLog $ "Unknown command: " <> input
    return True

-- | Run the debug command loop
debugCommandLoop :: (MonadIO m, MonadState DebugContext m) => m ()
debugCommandLoop = do
  debugLog "Fluxus Debugger - Type 'help' for commands"
  loop
  where
    loop = do
      minput <- liftIO $ runInputT defaultSettings $ getInputLine "fluxus-debug> "
      case minput of
        Nothing -> return () -- EOF
        Just input -> do
          let cmd = parseDebugCommand (T.pack input)
          continue <- executeDebugCommand cmd
          when continue loop

-- | Run the debug CLI interface
runDebugCLI :: IO ()
runDebugCLI = do
  -- Enable debug logging
  enableDebug
  
  -- Initialize debug context
  let initialContext = initDebugContext
  
  -- Run the command loop
  flip evalStateT initialContext $ do
    debugCommandLoop
    debugLog "Debug session ended"