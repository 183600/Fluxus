{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Fluxus.Internal.Monad
  ( -- * Core Monad
    CompilerM(..)
  , runCompilerM
  , evalCompilerM
  , execCompilerM
    
    -- * Error Handling
  , CompilerError(..)
  , throwCompilerError
  , catchCompilerError
  , tryCompilerM
    
    -- * Environment and State Types
  , CompilerEnv(..)
  , CompilerState(..)
  , CompilerPhase(..)
    
    -- * State Operations
  , get
  , gets
  , put
  , modify
  , modify'
    
    -- * Reader Operations
  , ask
  , asks
  , local
    
    -- * IO Operations
  , liftIO
    
    -- * Utility Functions
  , withPhase
  , getPhase
  , addWarning
  , getWarnings
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.List (intercalate)

-- ============================================================================
-- Core Types
-- ============================================================================

-- | Compilation phases for better error reporting
data CompilerPhase
  = Lexing
  | Parsing
  | TypeChecking
  | CodeGeneration
  | Optimization
  | Linking
  deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- | Compiler errors with phase information
data CompilerError
  = SyntaxError CompilerPhase Text (Maybe (Int, Int))
  | TypeError CompilerPhase Text (Maybe (Int, Int))
  | InternalError CompilerPhase Text
  | MultipleErrors [CompilerError]
  deriving stock (Eq, Generic)
    deriving anyclass (NFData)

-- | Pretty printing for compiler errors
instance Show CompilerError where
  show (SyntaxError phase msg loc) = 
    formatError "Syntax Error" phase msg loc
  show (TypeError phase msg loc) = 
    formatError "Type Error" phase msg loc
  show (InternalError phase msg) = 
    formatError "Internal Error" phase msg Nothing
  show (MultipleErrors errs) = 
    "Multiple Errors:\n" ++ intercalate "\n\n" (map show errs)

formatError :: String -> CompilerPhase -> Text -> Maybe (Int, Int) -> String
formatError errType phase msg loc =
  let locStr = case loc of
        Just (line, col) -> " at line " ++ show line ++ ", column " ++ show col
        Nothing -> ""
  in errType ++ " during " ++ show phase ++ locStr ++ ": " ++ T.unpack msg

-- | Read-only environment for the compiler
data CompilerEnv = CompilerEnv
  { envSourceFile     :: FilePath           -- ^ Current source file being compiled
  , envOutputDir      :: FilePath           -- ^ Output directory for generated files
  , envOptLevel       :: Int                -- ^ Optimization level (0-3)
  , envDebugMode      :: Bool               -- ^ Enable debug output
  , envWarningsAsErrors :: Bool             -- ^ Treat warnings as errors
  , envMaxErrors      :: Int                -- ^ Maximum errors before stopping
  , envVerbosity      :: Int                -- ^ Verbosity level for logging
  } deriving stock (Show, Eq)

-- | Mutable state during compilation
data CompilerState = CompilerState
  { statePhase        :: CompilerPhase      -- ^ Current compilation phase
  , stateWarnings     :: [Text]             -- ^ Accumulated warnings
  , stateErrorCount   :: Int                -- ^ Number of errors encountered
  , stateSymbolTable  :: SymbolTable        -- ^ Symbol table for name resolution
  , stateTypeEnv      :: TypeEnvironment    -- ^ Type environment
  , stateGeneratedCode :: [Text]            -- ^ Generated code fragments
  } deriving stock (Show, Eq)

-- | Simple symbol table (placeholder - expand as needed)
type SymbolTable = [(Text, SymbolInfo)]

data SymbolInfo = SymbolInfo
  { symbolName :: Text
  , symbolType :: Text  -- Simplified type representation
  , symbolScope :: Int
  } deriving stock (Show, Eq)

-- | Simple type environment (placeholder - expand as needed)
type TypeEnvironment = [(Text, TypeInfo)]

data TypeInfo = TypeInfo
  { typeName :: Text
  , typeKind :: Text
  } deriving stock (Show, Eq)

-- ============================================================================
-- Core Monad Definition
-- ============================================================================

-- | The core monad for the compiler, built as a transformer stack.
-- 
-- Stack order: Reader -> State -> Except -> IO
-- 
-- This ordering means:
-- - Errors (Except) will cause state changes to be rolled back
-- - IO operations are available throughout the compilation
-- - Environment is read-only and accessible everywhere
-- 
-- Using newtype provides:
-- - Strong type isolation
-- - Ability to define custom instances
-- - Better error messages
newtype CompilerM a = CompilerM
  { unCompilerM :: ReaderT CompilerEnv (StateT CompilerState (ExceptT CompilerError IO)) a
  } deriving newtype 
      ( Functor
      , Applicative
      , Monad
      , MonadReader CompilerEnv
      , MonadState CompilerState
      , MonadError CompilerError
      , MonadIO
      )

-- ============================================================================
-- Running the Compiler Monad
-- ============================================================================

-- | Run a compiler computation with given environment and initial state
runCompilerM :: CompilerEnv -> CompilerState -> CompilerM a -> IO (Either CompilerError (a, CompilerState))
runCompilerM env st (CompilerM m) = 
  runExceptT $ runStateT (runReaderT m env) st
-- | Run a compiler computation and return only the result, discarding final state
evalCompilerM :: CompilerEnv -> CompilerState -> CompilerM a -> IO (Either CompilerError a)
evalCompilerM env st m = fmap fst <$> runCompilerM env st m

-- | Run a compiler computation and return only the final state, discarding the result
execCompilerM :: CompilerEnv -> CompilerState -> CompilerM a -> IO (Either CompilerError CompilerState)
execCompilerM env st m = fmap snd <$> runCompilerM env st m

-- ============================================================================
-- Error Handling
-- ============================================================================

-- | Throw a compiler error
throwCompilerError :: CompilerError -> CompilerM a
throwCompilerError = throwError

-- | Catch and handle compiler errors
catchCompilerError :: CompilerM a -> (CompilerError -> CompilerM a) -> CompilerM a
catchCompilerError = catchError

-- | Try a computation and return either an error or the result
tryCompilerM :: CompilerM a -> CompilerM (Either CompilerError a)
tryCompilerM m = (Right <$> m) `catchCompilerError` (return . Left)

-- ============================================================================
-- Utility Functions
-- ============================================================================

-- | Execute a computation in a specific compilation phase
withPhase :: CompilerPhase -> CompilerM a -> CompilerM a
withPhase phase action = do
  oldPhase <- gets statePhase
  modify' $ \s -> s { statePhase = phase }
  result <- action `catchCompilerError` \e -> do
    modify' $ \s -> s { statePhase = oldPhase }
    throwCompilerError e
  modify' $ \s -> s { statePhase = oldPhase }
  return result
-- | Get the current compilation phase
getPhase :: CompilerM CompilerPhase
getPhase = gets statePhase
-- | Add a warning to the state
addWarning :: Text -> CompilerM ()
addWarning warning = do
  phase <- getPhase
  env <- ask
  let formattedWarning = T.pack (show phase) <> ": " <> warning
  modify' $ \s -> s { stateWarnings = formattedWarning : stateWarnings s }
  when (envWarningsAsErrors env) $ throwCompilerError $ TypeError phase warning Nothing
-- | Get all accumulated warnings
getWarnings :: CompilerM [Text]
getWarnings = gets stateWarnings
-- Default Values for Testing
-- ============================================================================

-- | Default compiler environment for testing
{-# ANN defaultCompilerEnv ("HLint: ignore" :: String) #-}
defaultCompilerEnv :: CompilerEnv
defaultCompilerEnv = CompilerEnv
  { envSourceFile = "input.flux"
  , envOutputDir = "./output"
  , envOptLevel = 0
  , envDebugMode = False
  , envWarningsAsErrors = False
  , envMaxErrors = 100
  , envVerbosity = 1
  }

-- | Default compiler state for testing
{-# ANN defaultCompilerState ("HLint: ignore" :: String) #-}
defaultCompilerState :: CompilerState
defaultCompilerState = CompilerState
  { statePhase = Lexing
  , stateWarnings = []
  , stateErrorCount = 0
  , stateSymbolTable = []
  , stateTypeEnv = []
  , stateGeneratedCode = []
  }

-- ============================================================================
-- Example Usage
-- ============================================================================

-- | Example compilation pipeline
{-# ANN exampleCompile ("HLint: ignore" :: String) #-}
exampleCompile :: Text -> CompilerM Text
exampleCompile sourceCode = do
  -- Lexing phase
  tokens <- withPhase Lexing $ do
    liftIO $ putStrLn "Lexing source code..."
    -- Actual lexing logic would go here
    return sourceCode
  
  -- Parsing phase
  ast <- withPhase Parsing $ do
    liftIO $ putStrLn "Parsing tokens..."
    -- Actual parsing logic would go here
    return tokens
  
  -- Type checking phase
  typedAst <- withPhase TypeChecking $ do
    liftIO $ putStrLn "Type checking AST..."
    env <- asks envDebugMode
    when env $ liftIO $ putStrLn "Debug: Type checking in progress"
    -- Actual type checking logic would go here
    return ast
  
  -- Code generation phase
  code <- withPhase CodeGeneration $ do
    liftIO $ putStrLn "Generating code..."
    modify $ \s -> s { stateGeneratedCode = [typedAst] }
    return typedAst
  
  -- Optimization phase (optional)
  optimized <- withPhase Optimization $ do
    optLevel <- asks envOptLevel
    if optLevel > 0
      then do
        liftIO $ putStrLn $ "Optimizing with level " ++ show optLevel ++ "..."
        -- Actual optimization logic would go here
        return code
      else return code
  
  return optimized

-- | Run the example
{-# ANN runExample ("HLint: ignore" :: String) #-}
runExample :: IO ()
runExample = do
  result <- runCompilerM defaultCompilerEnv defaultCompilerState $ 
    exampleCompile "sample source code"
  
  case result of
    Left err -> putStrLn $ "Compilation failed: " ++ show err
    Right (output, finalState) -> do
      putStrLn $ "Compilation succeeded: " ++ T.unpack output
      putStrLn $ "Warnings: " ++ show (stateWarnings finalState)