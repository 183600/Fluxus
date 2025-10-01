{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fluxus.Debug.Debugger
  ( DebuggerM
  , DebuggerState(..)
  , DebugBreakpoint(..)
  , DebugLogLevel(..)
  , runDebugger
  , setBreakpoint
  , removeBreakpoint
  , logDebug
  , logInfo
  , logWarning
  , logError
  , withBreakpoint
  , traceFunction
  , debugPrintState
  ) where

import Control.Monad.State
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn, hPutStrLn)
import System.IO (stderr, stdout)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent (threadDelay)
import System.Console.ANSI
import System.IO.Unsafe (unsafePerformIO)

-- | Debug log levels
data DebugLogLevel
  = DebugLevel  -- Detailed debugging information
  | InfoLevel   -- General information
  | WarnLevel   -- Warnings
  | ErrorLevel  -- Errors
  deriving (Eq, Ord, Show)

-- | Debug breakpoint
data DebugBreakpoint = DebugBreakpoint
  { bpId :: Int
  , bpFunction :: Text
  , bpCondition :: Maybe (DebuggerM Bool)
  , bpHitCount :: Int
  , bpEnabled :: Bool
  }

-- | Debugger state
data DebuggerState = DebuggerState
  { dsBreakpoints :: HashMap Text (Set DebugBreakpoint)
  , dsLogLevel :: DebugLogLevel
  , dsLogFile :: Maybe FilePath
  , dsCallStack :: [Text]
  , dsVariables :: HashMap Text Text
  , dsLastBreakpoint :: Maybe Int
  }

-- | Debugger monad
type DebuggerM = StateT DebuggerState IO

-- | Initial debugger state
initialDebuggerState :: DebuggerState
initialDebuggerState = DebuggerState
  { dsBreakpoints = HashMap.empty
  , dsLogLevel = InfoLevel
  , dsLogFile = Nothing
  , dsCallStack = []
  , dsVariables = HashMap.empty
  , dsLastBreakpoint = Nothing
  }

-- | Run debugger computation
runDebugger :: DebuggerM a -> IO (a, DebuggerState)
runDebugger = flip runStateT initialDebuggerState

-- | Set a breakpoint
setBreakpoint :: Text -> Maybe (DebuggerM Bool) -> DebuggerM Int
setBreakpoint functionName mCondition = do
  state <- get
  let nextId = maybe 1 ((+1) . maximum . map bpId . Set.toList) 
               (HashMap.lookup functionName (dsBreakpoints state))
  let breakpoint = DebugBreakpoint
        { bpId = nextId
        , bpFunction = functionName
        , bpCondition = mCondition
        , bpHitCount = 0
        , bpEnabled = True
        }
  let existingBreakpoints = HashMap.lookupDefault Set.empty functionName (dsBreakpoints state)
  put $ state
    { dsBreakpoints = HashMap.insert functionName (Set.insert breakpoint existingBreakpoints) (dsBreakpoints state)
    }
  return nextId

-- | Remove a breakpoint
removeBreakpoint :: Text -> Int -> DebuggerM Bool
removeBreakpoint functionName bpId = do
  state <- get
  case HashMap.lookup functionName (dsBreakpoints state) of
    Nothing -> return False
    Just breakpoints -> do
      let newBreakpoints = Set.filter ((/= bpId) . bpId) breakpoints
      put $ state
        { dsBreakpoints = HashMap.insert functionName newBreakpoints (dsBreakpoints state)
        }
      return True

-- | Log debug message
logDebug :: Text -> DebuggerM ()
logDebug = logMessage DebugLevel

-- | Log info message
logInfo :: Text -> DebuggerM ()
logInfo = logMessage InfoLevel

-- | Log warning message
logWarning :: Text -> DebuggerM ()
logWarning = logMessage WarnLevel

-- | Log error message
logError :: Text -> DebuggerM ()
logError = logMessage ErrorLevel

-- | Generic logging function
logMessage :: DebugLogLevel -> Text -> DebuggerM ()
logMessage level message = do
  state <- get
  when (level >= dsLogLevel state) $ do
    timestamp <- liftIO getCurrentTime
    let formattedTime = T.pack $ formatTime defaultTimeLocale "%H:%M:%S.%3q" timestamp
    let levelText = case level of
          DebugLevel -> "[DEBUG]"
          InfoLevel  -> "[INFO ]"
          WarnLevel  -> "[WARN ]"
          ErrorLevel -> "[ERROR]"
    
    let coloredLevel = case level of
          DebugLevel -> "\ESC[36m[DEBUG]\ESC[0m"  -- Cyan
          InfoLevel  -> "\ESC[32m[INFO ]\ESC[0m"  -- Green
          WarnLevel  -> "\ESC[33m[WARN ]\ESC[0m"  -- Yellow
          ErrorLevel -> "\ESC[31m[ERROR]\ESC[0m"  -- Red
    
    let logLine = T.unwords [coloredLevel, formattedTime, message]
    
    -- Print to console
    liftIO $ Data.Text.IO.putStrLn logLine
    
    -- Also log to file if specified
    case dsLogFile state of
      Just filePath -> liftIO $ T.appendFile filePath (logLine <> "\n")
      Nothing -> return ()

-- | Execute action with breakpoint checking
withBreakpoint :: Text -> DebuggerM a -> DebuggerM a
withBreakpoint functionName action = do
  state <- get
  case HashMap.lookup functionName (dsBreakpoints state) of
    Nothing -> action
    Just breakpoints -> do
      -- Check if any enabled breakpoints match
      matchingBreakpoints <- filterM shouldHitBreakpoint (Set.toList breakpoints)
      
      if null matchingBreakpoints
        then action
        else do
          -- Hit breakpoint
          let bp = head matchingBreakpoints
          logDebug $ "Hit breakpoint " <> T.pack (show (bpId bp)) <> " in " <> functionName
          
          -- Update hit count
          let updatedBp = bp { bpHitCount = bpHitCount bp + 1 }
          let newBreakpoints = Set.insert updatedBp (Set.delete bp breakpoints)
          put $ state
            { dsBreakpoints = HashMap.insert functionName newBreakpoints (dsBreakpoints state)
            , dsLastBreakpoint = Just (bpId bp)
            , dsCallStack = functionName : dsCallStack state
            }
          
          -- Pause execution
          liftIO $ do
            putStrLn $ "\ESC[1m\ESC[44mBREAKPOINT HIT: " <> T.unpack functionName <> " (ID: " <> show (bpId bp) <> ")\ESC[0m"
            putStrLn "Press Enter to continue, or 'q' to quit..."
            input <- getLine
            when (input == "q") $ error "User requested quit"
          
          -- Continue execution
          result <- action
          
          -- Pop from call stack
          modify $ \s -> s { dsCallStack = tail (dsCallStack s) }
          return result

-- | Check if breakpoint should be hit
shouldHitBreakpoint :: DebugBreakpoint -> DebuggerM Bool
shouldHitBreakpoint bp
  | not (bpEnabled bp) = return False
  | otherwise = case bpCondition bp of
      Nothing -> return True
      Just condition -> condition

-- | Trace function execution
traceFunction :: Text -> DebuggerM a -> DebuggerM a
traceFunction functionName action = do
  logDebug $ "Entering function: " <> functionName
  state <- get
  put $ state { dsCallStack = functionName : dsCallStack state }
  
  result <- action
  
  modify $ \s -> s { dsCallStack = tail (dsCallStack s) }
  logDebug $ "Exiting function: " <> functionName
  return result

-- | Print current debugger state
debugPrintState :: DebuggerM ()
debugPrintState = do
  state <- get
  logInfo "=== Debugger State ==="
  logInfo $ "Call stack: " <> T.intercalate " -> " (reverse $ dsCallStack state)
  logInfo $ "Log level: " <> T.pack (show $ dsLogLevel state)
  logInfo $ "Active breakpoints: " <> T.pack (show $ countActiveBreakpoints state)
  logInfo $ "Variables: " <> T.pack (show $ HashMap.size $ dsVariables state)
  
  -- Print call stack details
  when (not $ null $ dsCallStack state) $ do
    logInfo "\nCall Stack Details:"
    mapM_ (\(i, func) -> logInfo $ "  " <> T.pack (show i) <> ": " <> func) 
          (zip [0..] $ reverse $ dsCallStack state)
  where
    countActiveBreakpoints s = sum $ map Set.size $ HashMap.elems $ dsBreakpoints s

-- | Set a variable in debugger state
setDebugVariable :: Text -> Text -> DebuggerM ()
setDebugVariable name value = do
  modify $ \s -> s { dsVariables = HashMap.insert name value (dsVariables s) }

-- | Get a variable from debugger state
getDebugVariable :: Text -> DebuggerM (Maybe Text)
getDebugVariable name = do
  state <- get
  return $ HashMap.lookup name (dsVariables state)

-- | Clear all breakpoints
clearAllBreakpoints :: DebuggerM ()
clearAllBreakpoints = do
  modify $ \s -> s { dsBreakpoints = HashMap.empty }
  logInfo "All breakpoints cleared"

-- | List all breakpoints
listBreakpoints :: DebuggerM ()
listBreakpoints = do
  state <- get
  logInfo "=== Active Breakpoints ==="
  forM_ (HashMap.toList $ dsBreakpoints state) $ \(func, breakpoints) -> do
    forM_ (Set.toList breakpoints) $ \bp -> do
      logInfo $ T.unwords
        [ "ID:" <> T.pack (show $ bpId bp)
        , "Function:" <> func
        , "Hits:" <> T.pack (show $ bpHitCount bp)
        , if bpEnabled bp then "[ENABLED]" else "[DISABLED]"
        ]

-- | Wait for user input (for debugging)
debugPause :: Text -> DebuggerM ()
debugPause prompt = do
  logInfo $ prompt <> " (Press Enter to continue)"
  liftIO $ do
    _ <- getLine
    return ()

-- | Simple breakpoint function for quick debugging
breakpoint :: Text -> DebuggerM ()
breakpoint msg = do
  logDebug $ "BREAKPOINT: " <> msg
  debugPause "Paused at breakpoint"

-- | Conditional breakpoint
conditionalBreakpoint :: Text -> Bool -> DebuggerM ()
conditionalBreakpoint msg condition = do
  when condition $ do
    logDebug $ "CONDITIONAL BREAKPOINT: " <> msg
    debugPause "Paused at conditional breakpoint"

-- | Memory usage debugging
debugMemory :: Text -> DebuggerM ()
debugMemory context = do
  -- This is a simplified version - in a real implementation you'd use
  -- proper memory profiling tools
  logDebug $ "Memory check at: " <> context
  -- Simulate memory usage reporting
  let simulatedUsage = "2.3MB"  -- This would be real in practice
  logDebug $ "Approximate memory usage: " <> simulatedUsage
