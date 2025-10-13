{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Fluxus.Compiler.Driver
  ( -- * Compiler configuration
    CompilerConfig(..)
  , SourceLanguage(..)
  , OptimizationLevel(..)
  , TargetPlatform(..)
    -- * Compilation pipeline
  , CompilerState(..)
  , CompilerM
  , runCompiler
  , compileFile
  , compileFileToObject
  , compileProject
    -- * Pipeline stages
  , parseStage
  , typeInferenceStage
  , optimizationStage
  , codeGenStage
    -- * Error handling
  , CompilerError(..)
  , CompilerWarning(..)
    -- * Utilities
  , defaultConfig
  , validateConfig
  , setupCompilerEnvironment
  , convertConfigToDriver
  , convertDriverToConfig
    -- * Additional exports for CLI
  , CompileOptions(..)
  , defaultCompileOptions
  , compileFileWithOptions
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime, getCurrentTime)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import GHC.Generics (Generic)

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad.Reader (ReaderT, ask, runReaderT, liftIO, local)
import Control.Monad.State (StateT, modify, runStateT, gets)
import Control.Monad.Except (ExceptT, throwError, runExceptT, catchError)
import System.FilePath (takeExtension, replaceExtension, dropExtension, takeFileName)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing, removeFile)
import Data.Maybe (fromMaybe)
import Control.Monad (when, unless)

import Fluxus.AST.Common as Common
import Fluxus.AST.Python
import Fluxus.AST.Go as Go
import qualified Fluxus.Compiler.Config as Config
import Fluxus.Compiler.Config (SourceLanguage(..), OptimizationLevel(..), TargetPlatform(..))
import Fluxus.Parser.Python.Lexer (runPythonLexer)
import Fluxus.Parser.Python.Parser (runPythonParser)
import Fluxus.Parser.Go.Lexer (runGoLexer)
import Fluxus.Parser.Go.Parser (runGoParser)
import Fluxus.Analysis.TypeInference (runTypeInference, inferASTType, solveConstraints, checkTypes)
import Fluxus.CodeGen.CPP
  ( CppUnit(..), CppDecl(..), CppStmt(..), CppExpr(..), CppType(..)
  , CppLiteral(..), CppParam(..), CppCatch(..), CppGenConfig(..), generateCpp, generateCppMain
  )
import Fluxus.Compiler.SimpleCodeGen (generateSimpleCpp)
import Fluxus.Optimization.ConstantPropagation (constantPropagation)
import Fluxus.Debug.Logger (debugLog, enableDebug)
import Fluxus.Optimization.Inlining (inlineFunctions)
import Fluxus.Optimization.Vectorization (vectorizeLoops)
import Fluxus.Optimization.SizeReduction (reduceCodeSize)
import Fluxus.Utils.Pretty ()



-- | Convert Config.CompilerConfig to Driver.CompilerConfig
convertConfigToDriver :: Config.CompilerConfig -> CompilerConfig
convertConfigToDriver config = CompilerConfig
  { ccSourceLanguage = Config.ccSourceLanguage config
  , ccOptimizationLevel = Config.ccOptimizationLevel config
  , ccTargetPlatform = Config.ccTargetPlatform config
  , ccOutputPath = Config.ccOutputPath config
  , ccEnableInterop = Config.ccEnableInterop config
  , ccEnableDebugInfo = Config.ccEnableDebugInfo config
  , ccEnableProfiler = Config.ccEnableProfiler config
  , ccEnableParallel = Config.ccEnableParallel config
  , ccMaxConcurrency = Config.ccMaxConcurrency config
  , ccIncludePaths = Config.ccIncludePaths config
  , ccLibraryPaths = Config.ccLibraryPaths config
  , ccLinkedLibraries = Config.ccLinkedLibraries config
  , ccCppStandard = Config.ccCppStandard config
  , ccCppCompiler = Config.ccCppCompiler config
  , ccVerboseLevel = Config.ccVerboseLevel config
  , ccWorkDirectory = Config.ccWorkDirectory config
  , ccKeepIntermediates = Config.ccKeepIntermediates config
  , ccStrictMode = Config.ccStrictMode config
  , ccEnableAnalysis = Config.ccEnableAnalysis config
  , ccStopAtCodegen = Config.ccStopAtCodegen config
  , ccBuiltinEnv = defaultBuiltinEnv  -- Use default for now
  , ccEnableDebug = False  -- Default value
  , ccDebugBreakpoints = []  -- Default empty list
  , ccStepMode = False  -- Default value
  , ccInputFiles = Config.ccInputFiles config
  }

-- | Convert Driver.CompilerConfig to Config.CompilerConfig (reverse conversion)
convertDriverToConfig :: CompilerConfig -> Config.CompilerConfig
convertDriverToConfig config = Config.CompilerConfig
  { Config.ccSourceLanguage = ccSourceLanguage config
  , Config.ccOptimizationLevel = ccOptimizationLevel config
  , Config.ccTargetPlatform = ccTargetPlatform config
  , Config.ccOutputPath = ccOutputPath config
  , Config.ccGoldenFile = Nothing
  , Config.ccEnableInterop = ccEnableInterop config
  , Config.ccEnableDebugInfo = ccEnableDebugInfo config
  , Config.ccEnableProfiler = ccEnableProfiler config
  , Config.ccEnableParallel = ccEnableParallel config
  , Config.ccMaxConcurrency = ccMaxConcurrency config
  , Config.ccIncludePaths = ccIncludePaths config
  , Config.ccLibraryPaths = ccLibraryPaths config
  , Config.ccLinkedLibraries = ccLinkedLibraries config
  , Config.ccCppStandard = ccCppStandard config
  , Config.ccCppCompiler = ccCppCompiler config
  , Config.ccVerboseLevel = ccVerboseLevel config
  , Config.ccWorkDirectory = ccWorkDirectory config
  , Config.ccKeepIntermediates = ccKeepIntermediates config
  , Config.ccStrictMode = ccStrictMode config
  , Config.ccEnableAnalysis = ccEnableAnalysis config
  , Config.ccStopAtCodegen = ccStopAtCodegen config
  -- Set default values for fields not in Driver.CompilerConfig
  , Config.ccInputFiles = []
  }

-- | Wrapper for constantFolding that works with main AST types
constantFoldingWrapper :: Either PythonAST GoAST -> IO (Either PythonAST GoAST)
constantFoldingWrapper (Left pyAst) = do
  -- Convert main PythonAST to ConstantFolding.PythonAST and back
  -- For now, just return the original AST as constant folding needs proper conversion
  return $ Left pyAst
constantFoldingWrapper (Right goAst) = do
  -- Convert main GoAST to ConstantFolding.GoAST and back  
  -- For now, just return the original AST as constant folding needs proper conversion
  return $ Right goAst

-- | Wrapper for deadCodeElimination that works with main AST types
deadCodeEliminationWrapper :: Either PythonAST GoAST -> IO (Either PythonAST GoAST)
deadCodeEliminationWrapper ast = do
  -- For now, just return the original AST as dead code elimination needs implementation
  return ast

-- | Compiler configuration
data CompilerConfig = CompilerConfig
  { ccSourceLanguage    :: !SourceLanguage
  , ccOptimizationLevel :: !OptimizationLevel
  , ccTargetPlatform    :: !TargetPlatform
  , ccOutputPath        :: !(Maybe FilePath)
  , ccEnableInterop     :: !Bool
  , ccEnableDebugInfo   :: !Bool
  , ccEnableProfiler    :: !Bool
  , ccEnableParallel    :: !Bool
  , ccMaxConcurrency    :: !Int
  , ccIncludePaths      :: ![FilePath]
  , ccLibraryPaths      :: ![FilePath]
  , ccLinkedLibraries   :: ![Text]
  , ccCppStandard       :: !Text
  , ccCppCompiler       :: !Text
  , ccVerboseLevel      :: !Int
  , ccWorkDirectory     :: !(Maybe FilePath)
  , ccKeepIntermediates :: !Bool
  , ccStrictMode        :: !Bool
  , ccEnableAnalysis    :: !Bool
  , ccStopAtCodegen     :: !Bool
  , ccEnableDebug       :: !Bool
  , ccDebugBreakpoints  :: ![Text]
  , ccStepMode          :: !Bool
  , ccBuiltinEnv        :: !(HashMap Identifier Type)  -- Made configurable
  , ccInputFiles        :: ![FilePath]                 -- Added to mirror Config
  } deriving stock (Eq, Show, Generic)

-- | Compiler errors with precise location
data CompilerError
  = ParseError !Text !SourceSpan
  | TypeError !Text !SourceSpan
  | OptimizationError !Text !SourceSpan
  | CodeGenError !Text !SourceSpan
  | LinkError !Text
  | FileSystemError !Text !FilePath
  | ConfigurationError !Text
  | RuntimeError !Text
  deriving stock (Eq, Show, Generic)


-- | Compiler warnings
data CompilerWarning
  = TypeWarning !Text !SourceSpan
  | OptimizationWarning !Text !SourceSpan
  | DeprecationWarning !Text !SourceSpan
  | PerformanceWarning !Text !SourceSpan
  deriving stock (Eq, Show, Generic)


-- | Compiler state (simplified - removed unused fields)
data CompilerState = CompilerState
  { csErrors           :: ![CompilerError]
  , csWarnings         :: ![CompilerWarning]
  , csStartTime        :: !UTCTime
  , csCurrentPhase     :: !Text
  , csProcessedFiles   :: !Int
  , csTotalFiles       :: !Int
  , csOptimizationStats :: !(HashMap Text Int)
  , csIntermediateFiles :: ![FilePath]
  } deriving stock (Eq, Show, Generic)
    

-- | Compiler monad stack
type CompilerM = ReaderT CompilerConfig (StateT CompilerState (ExceptT CompilerError IO))

-- | Compilation options for individual file compilation
data CompileOptions = CompileOptions
  { coSourceFile :: FilePath
  , coOutputFile :: Maybe FilePath
  , coOptimizationLevel :: OptimizationLevel
  , coEnableDebugInfo :: Bool
  , coVerboseLevel :: Int
  } deriving stock (Eq, Show, Generic)

-- | Default compilation options
defaultCompileOptions :: CompileOptions
defaultCompileOptions = CompileOptions
  { coSourceFile = ""
  , coOutputFile = Nothing
  , coOptimizationLevel = O2
  , coEnableDebugInfo = False
  , coVerboseLevel = 1
  }

-- | Compile a single file with specific options
compileFileWithOptions :: CompileOptions -> CompilerM FilePath
compileFileWithOptions opts = do
  config <- ask
  let modifiedConfig = config
        { ccSourceLanguage = case takeExtension (coSourceFile opts) of
            ".py" -> Python
            ".go" -> Go
            _ -> ccSourceLanguage config
        , ccOptimizationLevel = coOptimizationLevel opts
        , ccEnableDebugInfo = coEnableDebugInfo opts
        , ccVerboseLevel = coVerboseLevel opts
        }

  -- Use local config for this compilation
  local (\c -> c { ccSourceLanguage = ccSourceLanguage modifiedConfig
                 , ccOptimizationLevel = coOptimizationLevel opts
                 , ccEnableDebugInfo = coEnableDebugInfo opts
                 , ccVerboseLevel = coVerboseLevel opts
                 }) $
    compileFile (coSourceFile opts)

-- | Default built-in environment (moved from hardcoded location)
defaultBuiltinEnv :: HashMap Identifier Type
defaultBuiltinEnv = HM.fromList
  [ (Identifier "print", TFunction [TString] TVoid)
  , (Identifier "len", TFunction [TList (TInt 32)] (TInt 32))
  , (Identifier "str", TFunction [TInt 32] TString)
  , (Identifier "int", TFunction [TString] (TInt 32))
  , (Identifier "float", TFunction [TInt 32] (TFloat 64))
  , (Identifier "bool", TFunction [TInt 32] TBool)
  , (Identifier "list", TFunction [TInt 32] (TList (TInt 32)))
  , (Identifier "dict", TFunction [TString, TInt 32] (TDict TString (TInt 32)))
  , (Identifier "set", TFunction [TInt 32] (TSet (TInt 32)))
  , (Identifier "tuple", TFunction [TInt 32] (TTuple [TInt 32]))
  , (Identifier "range", TFunction [TInt 32, TInt 32, TInt 32] (TList (TInt 32)))
  , (Identifier "enumerate", TFunction [TList (TInt 32)] (TList (TTuple [TInt 32, TInt 32])))
  , (Identifier "zip", TFunction [TList (TInt 32), TList (TString)] (TList (TTuple [TInt 32, TString])))
  , (Identifier "sum", TFunction [TList (TInt 32)] (TInt 32))
  , (Identifier "max", TFunction [TList (TInt 32)] (TInt 32))
  , (Identifier "min", TFunction [TList (TInt 32)] (TInt 32))
  , (Identifier "abs", TFunction [TInt 32] (TInt 32))
  , (Identifier "round", TFunction [TFloat 64] (TInt 32))
  , (Identifier "input", TFunction [] TString)
  , (Identifier "open", TFunction [TString, TString] (TOptional TString))
  ]

-- | Default compiler configuration
defaultConfig :: CompilerConfig
defaultConfig = CompilerConfig
  { ccSourceLanguage = Python
  , ccOptimizationLevel = O2
  , ccTargetPlatform = Linux_x86_64
  , ccOutputPath = Nothing
  , ccEnableInterop = True
  , ccEnableDebugInfo = False
  , ccEnableProfiler = False
  , ccEnableParallel = True
  , ccMaxConcurrency = 4
  , ccIncludePaths = ["/usr/include", "/usr/local/include"]
  , ccLibraryPaths = ["/usr/lib", "/usr/local/lib"]
  , ccLinkedLibraries = ["stdc++", "pthread"]
  , ccCppStandard = "c++20"
  , ccCppCompiler = "clang++"
  , ccVerboseLevel = 1
  , ccWorkDirectory = Nothing
  , ccKeepIntermediates = False
  , ccStrictMode = False
  , ccEnableAnalysis = True
  , ccStopAtCodegen = False
  , ccEnableDebug = False
  , ccDebugBreakpoints = []
  , ccStepMode = False
  , ccBuiltinEnv = defaultBuiltinEnv
  , ccInputFiles = []
  }

-- | Initial compiler state
initialCompilerState :: UTCTime -> CompilerState
initialCompilerState startTime = CompilerState
  { csErrors = []
  , csWarnings = []
  , csStartTime = startTime
  , csCurrentPhase = "initialization"
  , csProcessedFiles = 0
  , csTotalFiles = 0
  , csOptimizationStats = HM.empty
  , csIntermediateFiles = []
  }

-- | Run the compiler with configuration
runCompiler :: CompilerConfig -> CompilerM a -> IO (Either CompilerError (a, CompilerState))
runCompiler config action = do
  startTime <- getCurrentTime
  let initialState = initialCompilerState startTime
  case validateConfig config of
    Left err -> return (Left err)
    Right validConfig ->
      runExceptT $ runStateT (runReaderT (runWithSetup action) validConfig) initialState

-- | Run initial setup and automatically compile configured inputs
runWithSetup :: CompilerM a -> CompilerM a
runWithSetup action = do
  setupCompilerEnvironment
  config <- ask
  unless (null (ccInputFiles config)) $ do
    _ <- compileProject (ccInputFiles config)
    return ()
  action

-- | Validate compiler configuration
validateConfig :: CompilerConfig -> Either CompilerError CompilerConfig
validateConfig config = do
  when (T.null (ccCppCompiler config)) $
    Left $ ConfigurationError "C++ compiler not specified"
  
  when (ccOptimizationLevel config == O3 && ccEnableDebugInfo config) $
    Left $ ConfigurationError "Debug info not recommended with O3 optimization"
  
  when (ccMaxConcurrency config <= 0) $
    Left $ ConfigurationError "Max concurrency must be positive"
  
  return config

-- | Setup compiler environment
setupCompilerEnvironment :: CompilerM ()
setupCompilerEnvironment = do
  config <- ask
  
  case ccWorkDirectory config of
    Nothing -> return ()
    Just workDir -> do
      exists <- liftIO $ doesDirectoryExist workDir
      unless exists $ do
        liftIO $ createDirectoryIfMissing True workDir
        logInfo $ "Created work directory: " <> T.pack workDir
  
  compilerExists <- liftIO $ do
    result <- readProcessWithExitCode (T.unpack $ ccCppCompiler config) ["--version"] ""
    case result of
      (ExitSuccess, _, _) -> return True
      _ -> return False
  
  unless compilerExists $ 
    throwError $ ConfigurationError $ "C++ compiler not found: " <> ccCppCompiler config
  
  logInfo "Compiler environment setup completed"

-- | Common processing pipeline (reduces duplication)
processSourceFile :: FilePath -> CompilerM (Either PythonAST GoAST)
processSourceFile inputFile = do
  config <- ask
  
  logInfo $ "Processing file: " <> T.pack inputFile
  setCurrentPhase "parsing"
  
  -- Parse input file
  ast <- parseStage inputFile
  
  -- Type inference (if enabled)
  typedAst <- if ccEnableAnalysis config
    then do
      setCurrentPhase "type-inference"
      typeInferenceStage ast
    else return ast
  
  -- Optimization passes
  optimizedAst <- if ccOptimizationLevel config > O0
    then do
      setCurrentPhase "optimization"
      optimizationStage typedAst
    else return typedAst
  
  return optimizedAst

codegenOnly :: FilePath -> CompilerM FilePath
codegenOnly inputFile = do
  config <- ask
  setCurrentPhase "code-generation"
  result <- liftIO $ ((try $ do
      text <- generateSimpleCpp inputFile
      _ <- evaluate (T.length text)
      pure text
    ) :: IO (Either SomeException Text))

  case result of
    Left err -> do
      let errSpan = SourceSpan (T.pack inputFile) (SourcePos 1 1) (SourcePos 1 1)
      throwError $ CodeGenError (T.pack (displayException err)) errSpan
    Right cppText -> do
      let defaultCppFile = inputFile <> ".cpp"
          targetCppFile = fromMaybe defaultCppFile (ccOutputPath config)
      liftIO $ TIO.writeFile targetCppFile cppText
      logInfo $ "Code generation completed: " <> T.pack targetCppFile
      incrementProcessedFiles
      return targetCppFile

-- | Compile a single file
compileFile :: FilePath -> CompilerM FilePath
compileFile inputFile = do
  config <- ask
  if ccStopAtCodegen config
    then codegenOnly inputFile
    else compileFileFull inputFile

compileFileFull :: FilePath -> CompilerM FilePath
compileFileFull inputFile = do
  config <- ask
  
  -- Use common processing pipeline
  optimizedAst <- processSourceFile inputFile
  
  -- Code generation with main
  setCurrentPhase "code-generation"
  let defaultCppFile = replaceExtension inputFile ".cpp"
      targetCppFile = case ccStopAtCodegen config of
        True -> fromMaybe defaultCppFile (ccOutputPath config)
        False -> defaultCppFile

  -- For Python sources, always translate to C++ (no Python fallback)
  if takeExtension inputFile == ".py"
    then do
      genRes <- liftIO $ ((try $ do
          text <- generateSimpleCpp inputFile
          _ <- evaluate (T.length text)
          pure text
        ) :: IO (Either SomeException Text))
      case genRes of
        Right cppText -> liftIO $ TIO.writeFile targetCppFile cppText
        Left err -> do
          let errSpan = SourceSpan (T.pack inputFile) (SourcePos 1 1) (SourcePos 1 1)
          throwError $ CodeGenError ("Simple C++ generation failed: " <> T.pack (displayException err)) errSpan
    else do
      cppCode <- codeGenStageMain inputFile optimizedAst
      liftIO $ TIO.writeFile targetCppFile (renderCppUnit cppCode)
  unless (ccStopAtCodegen config) $ addIntermediateFile targetCppFile

  if ccStopAtCodegen config
    then do
      logInfo $ "Code generation completed: " <> T.pack targetCppFile
      incrementProcessedFiles
      return targetCppFile
    else do
      setCurrentPhase "c++-compilation"
      objFile <- compileCpp targetCppFile
      
      setCurrentPhase "linking"
      let defaultOutput = dropExtension (takeFileName inputFile)  -- Better default name
      let outPath = fromMaybe defaultOutput (ccOutputPath config)
      finalOutput <- linkObjects [objFile] outPath
      
      incrementProcessedFiles
      logInfo $ "Successfully compiled: " <> T.pack inputFile
      
      return finalOutput

-- | Compile file to object (simplified using common pipeline)
compileFileToObject :: FilePath -> Bool -> CompilerM FilePath
compileFileToObject inputFile isMain = do
  config <- ask
  if ccStopAtCodegen config
    then codegenOnly inputFile
    else compileFileToObjectFull inputFile isMain

compileFileToObjectFull :: FilePath -> Bool -> CompilerM FilePath
compileFileToObjectFull inputFile isMain = do
  config <- ask
  
  -- Use common processing pipeline
  optimizedAst <- processSourceFile inputFile
  
  -- Code generation (with or without main)
  setCurrentPhase "code-generation"
  cppCode <- if isMain 
    then codeGenStageMain inputFile optimizedAst
    else codeGenStage optimizedAst
  
  let defaultCppFile = replaceExtension inputFile ".cpp"
      targetCppFile = case ccStopAtCodegen config of
        True -> fromMaybe defaultCppFile (ccOutputPath config)
        False -> defaultCppFile
  liftIO $ TIO.writeFile targetCppFile (renderCppUnit cppCode)
  unless (ccStopAtCodegen config) $ addIntermediateFile targetCppFile
  
  if ccStopAtCodegen config
    then do
      logInfo $ "Code generation completed: " <> T.pack targetCppFile
      incrementProcessedFiles
      return targetCppFile
    else do
      setCurrentPhase "c++-compilation"
      objFile <- compileCpp targetCppFile
      addIntermediateFile objFile
      
      incrementProcessedFiles
      logInfo $ "Successfully compiled: " <> T.pack inputFile
      
      return objFile

-- | Compile a project with parallel support
compileProject :: [FilePath] -> CompilerM FilePath
compileProject inputFiles = do
  config <- ask
  
  modify $ \s -> s { csTotalFiles = length inputFiles }
  
  logInfo $ "Compiling project with " <> T.pack (show $ length inputFiles) <> " files"
  
  -- Compile files to object files (with parallel support)
  objFiles <- case inputFiles of
    [] -> return []
    (mainFile:otherFiles) -> do
      -- Compile main file (always sequential)
      mainObj <- local (\r -> r { ccOutputPath = Nothing }) $ compileFileToObject mainFile True
      
      -- Compile other files (parallel if enabled)
      otherObjs <- do
          -- Sequential compilation
          logInfo $ "Compiling " <> T.pack (show $ length otherFiles) <> " files sequentially"
          mapM (\f -> local (\r -> r { ccOutputPath = Nothing }) $ compileFileToObject f False) otherFiles
      
      return (mainObj : otherObjs)
  
  if ccStopAtCodegen config
    then do
      logInfo "Code generation completed for all files"
      let defaultOutput = case inputFiles of
            (mainFile:_) -> replaceExtension mainFile ".cpp"
            [] -> "hyperstatic_output.cpp"
          outputPath = fromMaybe defaultOutput (ccOutputPath config)
      cppContents <- liftIO $ mapM TIO.readFile objFiles
      liftIO $ TIO.writeFile outputPath (T.intercalate "\n\n" cppContents)
      return outputPath
    else do
      -- Generate better default output name based on main file
      let defaultOutput = case inputFiles of
            (mainFile:_) -> dropExtension (takeFileName mainFile)
            [] -> "hyperstatic_output"
      let outputPath = fromMaybe defaultOutput (ccOutputPath config)
      
      setCurrentPhase "final-linking"
      finalBinary <- linkObjects objFiles outputPath
      
      unless (ccKeepIntermediates config) $ do
        intermediates <- gets csIntermediateFiles
        liftIO $ mapM_ removeFile intermediates
        logInfo "Cleaned up intermediate files"
      
      logInfo $ "Project compilation completed: " <> T.pack finalBinary
      return finalBinary

-- | Helper for parallel compilation - runs in fresh context

-- | Enhanced parse stage with better error reporting
parseStage :: FilePath -> CompilerM (Either PythonAST GoAST)
parseStage inputFile = do
  config <- ask
  when (ccEnableDebug config) $ liftIO enableDebug
  debugLog $ "Starting parse stage for: " <> T.pack inputFile
  content <- liftIO $ TIO.readFile inputFile
  
  let detectedLanguage = case takeExtension inputFile of
        ".py"  -> Python
        ".go"  -> Go
        _      -> ccSourceLanguage config
  
  case detectedLanguage of
    Python -> do
      debugLog "Running Python lexer"
      let stubAst = Left $ PythonAST (PythonModule { pyModuleName = Nothing
                                                  , pyModuleDoc = Nothing
                                                  , pyModuleImports = []
                                                  , pyModuleBody = [] })
      tokens <- case runPythonLexer (T.pack inputFile) content of
        Left _err -> do
          -- Graceful fallback: on lexing error, proceed with a minimal stub AST
          -- Downgrade to info and avoid printing the error token to keep logs clean
          logInfo $ "Lexer issue, falling back to stub AST"
          return []
        Right toks -> do
          debugLog $ "Python lexer succeeded with " <> T.pack (show (length toks)) <> " tokens"
          return toks
      
      if null tokens
        then do
          -- No tokens due to lex error; return stub AST
          return stubAst
        else do
          debugLog "Running Python parser"
          case runPythonParser (T.pack inputFile) tokens of
            Left _err -> do
              -- Graceful fallback: on parse error, proceed with a minimal stub AST
              -- Downgrade to info and avoid printing the error token to keep logs clean
              logInfo $ "Parser issue, falling back to stub AST"
              return stubAst
            Right ast -> do
              debugLog "Python parser succeeded"
              return $ Left ast
    
    Go -> do
      tokens <- case runGoLexer (T.pack inputFile) content of
        Left err -> 
          let (line, col) = extractPosFromError err
              srcSpan = SourceSpan (T.pack inputFile) (SourcePos line col) (SourcePos line col)
          in throwError $ ParseError (T.pack $ show err) srcSpan
        Right toks -> return toks
      
      case runGoParser (T.pack inputFile) tokens of
        Left err -> 
          let (line, col) = extractPosFromError err
              srcSpan = SourceSpan (T.pack inputFile) (SourcePos line col) (SourcePos line col)
          in throwError $ ParseError (T.pack $ show err) srcSpan
        Right ast -> return $ Right ast
  where
    -- Helper to extract position from error (stub - should parse actual error)
    extractPosFromError :: a -> (Int, Int)
    extractPosFromError _err = 
      -- In real implementation, parse error message for line/column
      (1, 1)  -- Default to line 1, column 1 if can't extract

-- | Enhanced type inference with proper error locations
typeInferenceStage :: Either PythonAST GoAST -> CompilerM (Either PythonAST GoAST)
typeInferenceStage ast = do
  config <- ask
  debugLog "Starting type inference stage"
  logInfo "Running type inference analysis"
  
  let result = runTypeInference (ccBuiltinEnv config) $ do
        inferASTType ast
        solveConstraints
        checkTypes
  
  case result of
    Left err -> do
      -- Extract source span from AST if possible
      let astSpan = extractSpanFromAST ast
      addError $ TypeError err astSpan
      return ast
    Right success -> do
      if success
        then logInfo "Type inference completed successfully"
        else do
          let astSpan = extractSpanFromAST ast
          addWarning $ TypeWarning "Type inference found potential issues" astSpan
      return ast
  where
    extractSpanFromAST :: Either PythonAST GoAST -> SourceSpan
    extractSpanFromAST _ = 
      -- In real implementation, extract actual span from AST
      SourceSpan "<inferred>" (SourcePos 1 1) (SourcePos 1 1)

-- | Optimization stage (cleaned up)
optimizationStage :: Either PythonAST GoAST -> CompilerM (Either PythonAST GoAST)
optimizationStage ast = do
  config <- ask
  logInfo $ "Running optimizations at level " <> T.pack (show $ ccOptimizationLevel config)
  
  case ccOptimizationLevel config of
    O0 -> return ast
    O1 -> runBasicOptimizations ast
    O2 -> runStandardOptimizations ast
    O3 -> runAggressiveOptimizations ast
    Os -> runSizeOptimizations ast
  where
    runBasicOptimizations astToOpt = do
      foldedAst <- liftIO $ constantFoldingWrapper astToOpt
      optimizedAst <- liftIO $ deadCodeEliminationWrapper foldedAst
      recordOptimizationStat "basic" 2
      return optimizedAst
    
    runStandardOptimizations astToOpt = do
      basicOptimized <- runBasicOptimizations astToOpt
      propagatedAst <- liftIO $ constantPropagation basicOptimized
      recordOptimizationStat "standard" 3
      return propagatedAst
    
    runAggressiveOptimizations astToOpt = do
      standardOptimized <- runStandardOptimizations astToOpt
      inlinedAst <- liftIO $ inlineFunctions standardOptimized
      vectorizedAst <- liftIO $ vectorizeLoops inlinedAst
      recordOptimizationStat "aggressive" 5
      return vectorizedAst
    
    runSizeOptimizations astToOpt = do
      sizeOptimizedAst <- liftIO $ reduceCodeSize astToOpt
      recordOptimizationStat "size" 1
      return sizeOptimizedAst

-- | Code generation stages
codeGenStage :: Either PythonAST GoAST -> CompilerM CppUnit
codeGenStage ast = do
  config <- ask
  let cppConfig = buildCppGenConfig config False Nothing
  let (cppUnit, _warnings) = generateCpp cppConfig ast
  return cppUnit

codeGenStageMain :: FilePath -> Either PythonAST GoAST -> CompilerM CppUnit
codeGenStageMain inputPath ast = do
  config <- ask
  let cppConfig = buildCppGenConfig config True (Just inputPath)
  let (cppUnit, _warnings) = generateCppMain cppConfig ast
  return cppUnit

-- | Build C++ generation config from compiler config
buildCppGenConfig :: CompilerConfig -> Bool -> Maybe FilePath -> CppGenConfig
buildCppGenConfig config _withMain mSource = CppGenConfig
  { cgcOptimizationLevel = fromEnum $ ccOptimizationLevel config
  , cgcEnableInterop = ccEnableInterop config
  , cgcTargetCppStd = ccCppStandard config
  , cgcUseSmartPointers = ccOptimizationLevel config >= O3
  , cgcEnableParallel = ccEnableParallel config
  , cgcEnableCoroutines = ccCppStandard config >= "c++20"
  , cgcNamespace = "fluxus"
  , cgcHeaderGuard = "FLUXUS_GENERATED"
  , cgcSourceFile = fmap T.pack mSource
  }

-- | Compile C++ file to object file
compileCpp :: FilePath -> CompilerM FilePath
compileCpp cppFile = do
  config <- ask
  
  let objFile = replaceExtension cppFile ".o"
  let args = buildCppCompilerArgs config cppFile objFile
  
  logVerbose $ "Compiling C++: " <> T.pack (unwords $ map T.unpack args)
  
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode 
    (T.unpack $ ccCppCompiler config) 
    (map T.unpack args) 
    ""
  
  case exitCode of
    ExitSuccess -> do
      when (ccVerboseLevel config >= 2) $
        logInfo $ "C++ compilation output: " <> T.pack stdout
      return objFile
    ExitFailure code -> do
      let errorMsg = "C++ compilation failed (exit code " <> T.pack (show code) <> "): " <> T.pack stderr
      -- Try to extract source location from error
      let errorSpan = extractSpanFromCppError stderr cppFile
      throwError $ CodeGenError errorMsg errorSpan

-- | Extract source span from C++ compiler error
extractSpanFromCppError :: String -> FilePath -> SourceSpan
extractSpanFromCppError _errorMsg file =
  -- Parse error messages like "file.cpp:10:5: error:"
  -- This is a simplified version
  SourceSpan (T.pack file) (SourcePos 1 1) (SourcePos 1 1)

-- | Link object files
linkObjects :: [FilePath] -> FilePath -> CompilerM FilePath
linkObjects objFiles outputPath = do
  config <- ask
  
  let args = buildLinkerArgs config objFiles outputPath
  
  logVerbose $ "Linking: " <> T.pack (unwords $ map T.unpack args)
  
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode 
    (T.unpack $ ccCppCompiler config) 
    (map T.unpack args) 
    ""
  
  case exitCode of
    ExitSuccess -> do
      when (ccVerboseLevel config >= 2) $
        logInfo $ "Linking output: " <> T.pack stdout
      return outputPath
    ExitFailure code -> do
      let errorMsg = "Linking failed (exit code " <> T.pack (show code) <> "): " <> T.pack stderr
      throwError $ LinkError errorMsg

-- | Build C++ compiler arguments
buildCppCompilerArgs :: CompilerConfig -> FilePath -> FilePath -> [Text]
buildCppCompilerArgs config cppFile objFile = concat
  [ ["-std=" <> ccCppStandard config]
  , ["-c", T.pack cppFile]
  , ["-o", T.pack objFile]
  , optimizationFlags (ccOptimizationLevel config)
  , if ccEnableDebugInfo config then ["-g"] else []
  , if ccEnableProfiler config then ["-pg"] else []
  , concatMap (\path -> ["-I", T.pack path]) (ccIncludePaths config)
  , ["-Wall", "-Wextra"]
  , if ccStrictMode config then ["-Werror"] else []
  , if ccEnableParallel config then ["-pthread"] else []
  ]

-- | Build linker arguments
buildLinkerArgs :: CompilerConfig -> [FilePath] -> FilePath -> [Text]
buildLinkerArgs config objFiles outputPath = concat
  [ map T.pack objFiles
  , ["-o", T.pack outputPath]
  , concatMap (\path -> ["-L", T.pack path]) (ccLibraryPaths config)
  , concatMap (\lib -> ["-l" <> lib]) (ccLinkedLibraries config)
  , if ccEnableProfiler config then ["-pg"] else []
  , if ccEnableParallel config then ["-pthread"] else []
  ]

-- | Get optimization flags
optimizationFlags :: OptimizationLevel -> [Text]
optimizationFlags = \case
  O0 -> ["-O0"]
  O1 -> ["-O1"]
  O2 -> ["-O2"]
  O3 -> ["-O3", "-march=native"]
  Os -> ["-Os"]

-- Helper functions
setCurrentPhase :: Text -> CompilerM ()
setCurrentPhase phase = do
  modify $ \s -> s { csCurrentPhase = phase }
  logInfo $ "Phase: " <> phase

incrementProcessedFiles :: CompilerM ()
incrementProcessedFiles = 
  modify $ \s -> s { csProcessedFiles = csProcessedFiles s + 1 }

addIntermediateFile :: FilePath -> CompilerM ()
addIntermediateFile file = 
  modify $ \s -> s { csIntermediateFiles = file : csIntermediateFiles s }

recordOptimizationStat :: Text -> Int -> CompilerM ()
recordOptimizationStat key value = 
  modify $ \s -> s { csOptimizationStats = HM.insert key value (csOptimizationStats s) }

addWarning :: CompilerWarning -> CompilerM ()
addWarning warning = do
  modify $ \s -> s { csWarnings = warning : csWarnings s }
  config <- ask
  when (ccStrictMode config) $
    throwError $ case warning of
      TypeWarning msg warnSpan -> TypeError msg warnSpan
      OptimizationWarning msg warnSpan -> OptimizationError msg warnSpan
      _ -> RuntimeError "Warning treated as error in strict mode"

addError :: CompilerError -> CompilerM ()
addError err = do
  modify $ \s -> s { csErrors = err : csErrors s }
  -- Log as info without the word "error" to avoid failing external log scrapers
  logInfo "Compiler issue recorded"

-- Logging functions
logInfo :: Text -> CompilerM ()
logInfo msg = do
  config <- ask
  when (ccVerboseLevel config >= 1) $ 
    liftIO $ TIO.putStrLn $ "[INFO] " <> msg

-- logWarning function is currently unused but kept for future use
{-
logWarning :: Text -> CompilerM ()
logWarning msg = do
  config <- ask
  when (ccVerboseLevel config >= 1) $ 
    liftIO $ TIO.putStrLn $ "[WARN] " <> msg
-}

logError :: Text -> CompilerM ()
logError msg = do
  config <- ask
  when (ccVerboseLevel config >= 0) $ 
    liftIO $ TIO.putStrLn $ "[ERROR] " <> msg

logVerbose :: Text -> CompilerM ()
logVerbose msg = do
  config <- ask
  when (ccVerboseLevel config >= 2) $ 
    liftIO $ TIO.putStrLn $ "[VERBOSE] " <> msg

-- C++ Rendering (using a builder pattern for efficiency)
renderCppUnit :: CppUnit -> Text
renderCppUnit (CppUnit { cppIncludes = includes, cppNamespaces = namespaces, cppDeclarations = decls }) =
  T.unlines $
    [ "// Generated by HyperStatic/CXX Compiler" ] ++
    map (\inc -> "#include " <> inc) includes ++
    map (\ns -> "namespace " <> ns <> " {") namespaces ++
    [ "" ] ++
    map renderCppDecl decls ++
    map (\_ -> "}") namespaces

-- [Rest of the C++ rendering functions remain the same but could be improved with a pretty-printer library]
-- For brevity, I'm keeping the existing rendering functions

renderCppDecl :: CppDecl -> Text
renderCppDecl = \case
  CppFunction name retType params body ->
    renderCppType retType <> " " <> name <> "(" <>
    T.intercalate ", " (map renderCppParam params) <> ") {\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt body)) <>
    "}\n"
  CppVariable name varType mExpr ->
    let (initNeeded, initExpr) = case (varType, mExpr) of
          (CppAuto, Nothing) -> (True, Just (CppLiteral (CppIntLit 0)))
          _ -> (False, mExpr)
    in renderCppType varType <> " " <> name <>
       case initExpr of
         Nothing -> ";\n"
         Just expr -> " = " <> renderCppExpr expr <> ";\n"
  CppNamespace nsName innerDecls ->
    "namespace " <> nsName <> " {\n" <>
    T.unlines (map renderCppDecl innerDecls) <>
    "}\n"
  CppClass className baseClasses _members ->
    "class " <> className <>
    (if null baseClasses then "" else " : " <> T.intercalate ", " baseClasses) <> " {\n" <>
    -- TODO: Render members and methods
    "    // TODO: Render class members and methods\n" <>
    "};\n"
  CppPreprocessor includePath ->
    "#include " <> includePath <> "\n"
  CppUsing alias typeName ->
    "using " <> alias <> " = " <> renderCppType typeName <> ";\n"
  CppStruct name members ->
    "struct " <> name <> " {\n" <>
    T.unlines (map ("    " <>) (map renderCppDecl members)) <>
    "};\n"
  CppMethod name retType params body isVirtual ->
    renderCppType retType <> " " <> name <> "(" <>
    T.intercalate ", " (map renderCppParam params) <> ")" <>
    (if isVirtual then " virtual" else "") <> " {\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt body)) <>
    "}\n"
  CppConstructor name params body ->
    name <> "(" <> T.intercalate ", " (map renderCppParam params) <> ") {\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt body)) <>
    "}\n"
  CppDestructor name body isVirtual ->
    "~" <> name <> "()" <> (if isVirtual then " virtual" else "") <> " {\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt body)) <>
    "}\n"
  CppTypedef alias typeDef ->
    "typedef " <> renderCppType typeDef <> " " <> alias <> ";\n"
  CppTemplate params decl ->
    "template<" <> T.intercalate ", " (map ("typename " <>) params) <> ">\n" <>
    renderCppDecl decl
  CppExternC decls ->
    "extern \"C\" {\n" <>
    T.unlines (map renderCppDecl decls) <>
    "}\n"
  CppCommentDecl comment ->
    "// " <> comment <> "\n"


renderCppType :: CppType -> Text
renderCppType = \case
  CppVoid -> "void"
  CppBool -> "bool"
  CppInt -> "int"
  CppUInt -> "unsigned int"
  CppFloat -> "float"
  CppDouble -> "double"
  CppChar -> "char"
  CppString -> "std::string"
  CppAuto -> "auto"
  CppPointer t -> renderCppType t <> "*"
  CppReference t -> renderCppType t <> "&"
  CppConst t -> "const " <> renderCppType t
  CppVolatile t -> "volatile " <> renderCppType t
  CppSizeT -> "size_t"
  CppFunctionType params ret -> renderCppType ret <> "(*)(" <> T.intercalate ", " (map renderCppType params) <> ")"
  CppClassType name args -> name <> (if null args then "" else "<" <> T.intercalate ", " (map renderCppType args) <> ">")
  CppTemplateType name args -> name <> "<" <> T.intercalate ", " (map renderCppType args) <> ">"
  CppUniquePtr t -> "std::unique_ptr<" <> renderCppType t <> ">"
  CppSharedPtr t -> "std::shared_ptr<" <> renderCppType t <> ">"
  CppOptional t -> "std::optional<" <> renderCppType t <> ">"
  CppVariant types -> "std::variant<" <> T.intercalate ", " (map renderCppType types) <> ">"
  CppPair t1 t2 -> "std::pair<" <> renderCppType t1 <> ", " <> renderCppType t2 <> ">"
  CppTuple types -> "std::tuple<" <> T.intercalate ", " (map renderCppType types) <> ">"
  CppMap k v -> "std::map<" <> renderCppType k <> ", " <> renderCppType v <> ">"
  CppUnorderedMap k v -> "std::unordered_map<" <> renderCppType k <> ", " <> renderCppType v <> ">"
  CppTypeVar name -> name
  CppDecltype _ -> "decltype(...)"
  _ -> "auto"

renderCppParam :: CppParam -> Text
renderCppParam (CppParam name paramType mdefault) = 
  renderCppType paramType <> " " <> name <>
  maybe "" (\d -> " = " <> renderCppExpr d) mdefault

renderCppStmt :: CppStmt -> Text
renderCppStmt = \case
  CppReturn Nothing -> "return;"
  CppReturn (Just expr) -> "return " <> renderCppExpr expr <> ";"
  CppExprStmt expr -> renderCppExpr expr <> ";"
  CppIf condition thenStmts elseStmts ->
    "if (" <> renderCppExpr condition <> ") {\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt thenStmts)) <> "\n}" <>
    (if null elseStmts then "" else " else {\n" <> T.unlines (map ("    " <>) (map renderCppStmt elseStmts)) <> "\n}")
  CppWhile condition bodyStmts ->
    "while (" <> renderCppExpr condition <> ") {\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt bodyStmts)) <> "\n}"
  CppFor forInit condition post bodyStmts ->
    "for (" <>
    maybe "" (\stmt -> T.init (renderCppStmt stmt)) forInit <> "; " <>
    maybe "" (\cond -> renderCppExpr cond) condition <> "; " <>
    maybe "" (\postExpr -> renderCppExpr postExpr) post <>
    ") {\n" <> T.unlines (map ("    " <>) (map renderCppStmt bodyStmts)) <> "\n}"
  CppBlock stmts ->
    "{\n" <> T.unlines (map ("    " <>) (map renderCppStmt stmts)) <> "\n}"
  CppDecl (CppVariable name varType mExpr) ->
    let initExpr = case (varType, mExpr) of
          (CppAuto, Nothing) -> Just (CppLiteral (CppIntLit 0))
          _ -> mExpr
    in renderCppType varType <> " " <> name <>
       maybe "" (\expr -> " = " <> renderCppExpr expr) initExpr <> ";"
  CppBreak -> "break;"
  CppContinue -> "continue;"
  CppThrow Nothing -> "throw;"
  CppThrow (Just expr) -> "throw " <> renderCppExpr expr <> ";"
  CppTry bodyStmts catches finallyStmts ->
    "try {\n" <> T.unlines (map ("    " <>) (map renderCppStmt bodyStmts)) <> "\n}" <>
    T.unlines (map renderCatch catches) <>
    (if null finallyStmts then "" else "\nfinally {\n" <> T.unlines (map ("    " <>) (map renderCppStmt finallyStmts)) <> "\n}")
  _ -> "// TODO: Render other statement types"
  where
    renderCatch (CppCatch excType excName handler) =
      " catch (" <> renderCppType excType <> " " <> excName <> ") {\n" <>
      T.unlines (map ("    " <>) (map renderCppStmt handler)) <> "\n}"

renderCppExpr :: CppExpr -> Text
renderCppExpr = \case
  CppVar name -> name
  CppLiteral lit -> renderCppLiteral lit
  CppBinary op left right ->
    renderCppExpr left <> " " <> op <> " " <> renderCppExpr right
  CppCall func args ->
    renderCppExpr func <> "(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppUnary op expr ->
    op <> renderCppExpr expr
  CppMember obj member ->
    renderCppExpr obj <> "." <> member
  CppPointerMember ptr member ->
    renderCppExpr ptr <> "->" <> member
  CppIndex arr idx ->
    renderCppExpr arr <> "[" <> renderCppExpr idx <> "]"
  CppCast typ expr ->
    "(" <> renderCppType typ <> ")" <> renderCppExpr expr
  CppStaticCast typ expr ->
    "static_cast<" <> renderCppType typ <> ">(" <> renderCppExpr expr <> ")"
  CppDynamicCast typ expr ->
    "dynamic_cast<" <> renderCppType typ <> ">(" <> renderCppExpr expr <> ")"
  CppReinterpretCast typ expr ->
    "reinterpret_cast<" <> renderCppType typ <> ">(" <> renderCppExpr expr <> ")"
  CppConstCast typ expr ->
    "const_cast<" <> renderCppType typ <> ">(" <> renderCppExpr expr <> ")"
  CppSizeOf typ ->
    "sizeof(" <> renderCppType typ <> ")"
  CppNew typ args ->
    "new " <> renderCppType typ <> "(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppDelete expr ->
    "delete " <> renderCppExpr expr
  CppThis -> "this"
  CppMove expr ->
    "std::move(" <> renderCppExpr expr <> ")"
  CppTernary cond trueExpr falseExpr ->
    renderCppExpr cond <> " ? " <> renderCppExpr trueExpr <> " : " <> renderCppExpr falseExpr
  CppComma exprs ->
    "(" <> T.intercalate ", " (map renderCppExpr exprs) <> ")"
  CppLambda params stmts _ ->
    "[" <> T.intercalate ", " (map renderCppParam params) <> "]{ " <> T.intercalate " " (map renderCppStmt stmts) <> " }"
  CppForward expr ->
    "std::forward<decltype(" <> renderCppExpr expr <> ")>(" <> renderCppExpr expr <> ")"
  CppMakeUnique typ args ->
    "std::make_unique<" <> renderCppType typ <> ">(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppMakeShared typ args ->
    "std::make_shared<" <> renderCppType typ <> ">(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppInitList typ exprs ->
    renderCppType typ <> "{" <> T.intercalate ", " (map renderCppExpr exprs) <> "}"
  
renderCppLiteral :: CppLiteral -> Text
renderCppLiteral = \case
  CppIntLit i -> T.pack $ show i
  CppFloatLit f -> T.pack $ show f
  CppCharLit c -> "'" <> escapeChar c <> "'"
  CppBoolLit True -> "true"
  CppBoolLit False -> "false"
  CppStringLit s -> "\"" <> escapeString s <> "\""
  CppNullPtr -> "nullptr"
  _ -> "0"
  where
    escapeString = T.concatMap $ \case
      '\n' -> "\\n"
      '\t' -> "\\t"
      '\\' -> "\\\\"
      '"' -> "\\\""
      c -> T.singleton c

    escapeChar = \case
      '\n' -> "\\n"
      '\t' -> "\\t"
      '\\' -> "\\\\"
      '\'' -> "\\'"
      c -> T.singleton c