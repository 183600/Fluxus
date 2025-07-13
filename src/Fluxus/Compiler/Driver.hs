{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- | Main compiler driver that orchestrates the compilation pipeline
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
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad (when, unless)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Time
import System.FilePath
import System.Directory
import System.Process
import System.Exit
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.AST.Go
import Fluxus.Parser.Python.Lexer (runPythonLexer)
import Fluxus.Parser.Python.Parser (runPythonParser)
import Fluxus.Parser.Go.Lexer (runGoLexer)
import Fluxus.Parser.Go.Parser (runGoParser)
import Fluxus.CodeGen.CPP
import Fluxus.Utils.Pretty

-- | Source language selection
data SourceLanguage = Python | Go
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Optimization levels
data OptimizationLevel
  = O0  -- No optimization, fast compilation
  | O1  -- Basic optimizations
  | O2  -- Standard optimizations
  | O3  -- Aggressive optimizations
  | Os  -- Size optimizations
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Target platforms
data TargetPlatform
  = Linux_x86_64
  | Linux_ARM64
  | Darwin_x86_64
  | Darwin_ARM64
  | Windows_x86_64
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

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
  , ccCppStandard       :: !Text           -- "c++20", "c++23", etc.
  , ccCppCompiler       :: !Text           -- "clang++", "g++", etc.
  , ccVerboseLevel      :: !Int             -- 0 = quiet, 3 = very verbose
  , ccWorkDirectory     :: !(Maybe FilePath)
  , ccKeepIntermediates :: !Bool
  , ccStrictMode        :: !Bool            -- Treat warnings as errors
  , ccEnableAnalysis    :: !Bool            -- Enable static analysis
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Compiler errors
data CompilerError
  = ParseError !Text !SourceSpan
  | TypeError !Text !SourceSpan
  | OptimizationError !Text
  | CodeGenError !Text
  | LinkError !Text
  | FileSystemError !Text !FilePath
  | ConfigurationError !Text
  | RuntimeError !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Compiler warnings
data CompilerWarning
  = TypeWarning !Text !SourceSpan
  | OptimizationWarning !Text
  | DeprecationWarning !Text !SourceSpan
  | PerformanceWarning !Text !SourceSpan
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Compiler state
data CompilerState = CompilerState
  { csErrors           :: ![CompilerError]
  , csWarnings         :: ![CompilerWarning]
  , csStartTime        :: !UTCTime
  , csCurrentPhase     :: !Text
  , csProcessedFiles   :: !Int
  , csTotalFiles       :: !Int
  , csSymbolTable      :: !(HashMap Text Type)
  , csTypeEnvironment  :: !(HashMap Text Type)
  , csOptimizationStats :: !(HashMap Text Int)
  , csIntermediateFiles :: ![FilePath]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Compiler monad stack
type CompilerM = ReaderT CompilerConfig (StateT CompilerState (ExceptT CompilerError IO))

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
  , csSymbolTable = HM.empty
  , csTypeEnvironment = HM.empty
  , csOptimizationStats = HM.empty
  , csIntermediateFiles = []
  }

-- | Run the compiler with configuration
runCompiler :: CompilerConfig -> CompilerM a -> IO (Either CompilerError (a, CompilerState))
runCompiler config action = do
  startTime <- getCurrentTime
  let initialState = initialCompilerState startTime
  runExceptT $ runStateT (runReaderT action config) initialState

-- | Validate compiler configuration
validateConfig :: CompilerConfig -> Either CompilerError CompilerConfig
validateConfig config = do
  -- Check if C++ compiler exists
  when (T.null (ccCppCompiler config)) $
    Left $ ConfigurationError "C++ compiler not specified"
  
  -- Validate optimization level compatibility
  when (ccOptimizationLevel config == O3 && ccEnableDebugInfo config) $
    Left $ ConfigurationError "Debug info not recommended with O3 optimization"
  
  -- Validate concurrency settings
  when (ccMaxConcurrency config <= 0) $
    Left $ ConfigurationError "Max concurrency must be positive"
  
  return config

-- | Setup compiler environment
setupCompilerEnvironment :: CompilerM ()
setupCompilerEnvironment = do
  config <- ask
  
  -- Create work directory if specified
  case ccWorkDirectory config of
    Nothing -> return ()
    Just workDir -> do
      exists <- liftIO $ doesDirectoryExist workDir
      unless exists $ do
        liftIO $ createDirectoryIfMissing True workDir
        logInfo $ "Created work directory: " <> T.pack workDir
  
  -- Verify C++ compiler availability
  compilerExists <- liftIO $ do
    result <- readProcessWithExitCode (T.unpack $ ccCppCompiler config) ["--version"] ""
    case result of
      (ExitSuccess, _, _) -> return True
      _ -> return False
  
  unless compilerExists $ 
    throwError $ ConfigurationError $ "C++ compiler not found: " <> ccCppCompiler config
  
  logInfo "Compiler environment setup completed"

-- | Compile a single file
compileFile :: FilePath -> CompilerM FilePath
compileFile inputFile = do
  config <- ask
  
  logInfo $ "Compiling file: " <> T.pack inputFile
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
  
  -- Code generation
  setCurrentPhase "code-generation"
  cppCode <- codeGenStage optimizedAst
  
  -- Write intermediate C++ file
  let cppFile = replaceExtension inputFile ".cpp"
  liftIO $ TIO.writeFile cppFile (renderCppUnit cppCode)
  addIntermediateFile cppFile
  
  -- Compile C++ to object file
  setCurrentPhase "c++-compilation"
  objFile <- compileCpp cppFile
  
  -- Link if this is the final step
  setCurrentPhase "linking"
  finalOutput <- case ccOutputPath config of
    Nothing -> return objFile
    Just outPath -> linkObjects [objFile] outPath
  
  incrementProcessedFiles
  logInfo $ "Successfully compiled: " <> T.pack inputFile
  
  return finalOutput

-- | Compile a project (multiple files)
compileProject :: [FilePath] -> CompilerM FilePath
compileProject inputFiles = do
  config <- ask
  
  -- Set total file count
  modify $ \s -> s { csTotalFiles = length inputFiles }
  
  logInfo $ "Compiling project with " <> T.pack (show $ length inputFiles) <> " files"
  
  -- Compile all files to object files
  objFiles <- mapM compileFile inputFiles
  
  -- Link all object files
  let outputPath = fromMaybe "hyperstatic_output" (ccOutputPath config)
  setCurrentPhase "final-linking"
  finalBinary <- linkObjects objFiles outputPath
  
  -- Cleanup intermediate files if requested
  unless (ccKeepIntermediates config) $ do
    intermediates <- gets csIntermediateFiles
    liftIO $ mapM_ removeFile intermediates
    logInfo "Cleaned up intermediate files"
  
  logInfo $ "Project compilation completed: " <> T.pack finalBinary
  return finalBinary

-- | Parse input file based on source language (detected from file extension)
parseStage :: FilePath -> CompilerM (Either PythonAST GoAST)
parseStage inputFile = do
  config <- ask
  content <- liftIO $ TIO.readFile inputFile
  
  -- Detect language from file extension, with config as fallback
  let detectedLanguage = case takeExtension inputFile of
        ".py"  -> Python
        ".go"  -> Go
        _      -> ccSourceLanguage config  -- fallback to config
  
  case detectedLanguage of
    Python -> do
      -- Tokenize Python
      tokens <- case runPythonLexer (T.pack inputFile) content of
        Left err -> throwError $ ParseError (T.pack $ show err) (SourceSpan (T.pack inputFile) (SourcePos 0 0) (SourcePos 0 0))
        Right toks -> return toks
      
      -- Parse Python
      case runPythonParser (T.pack inputFile) tokens of
        Left err -> throwError $ ParseError (T.pack $ show err) (SourceSpan (T.pack inputFile) (SourcePos 0 0) (SourcePos 0 0))
        Right ast -> return $ Left ast
    
    Go -> do
      -- Tokenize Go
      tokens <- case runGoLexer (T.pack inputFile) content of
        Left err -> throwError $ ParseError (T.pack $ show err) (SourceSpan (T.pack inputFile) (SourcePos 0 0) (SourcePos 0 0))
        Right toks -> return toks
      
      -- Parse Go
      case runGoParser (T.pack inputFile) tokens of
        Left err -> throwError $ ParseError (T.pack $ show err) (SourceSpan (T.pack inputFile) (SourcePos 0 0) (SourcePos 0 0))
        Right ast -> return $ Right ast

-- | Type inference stage (placeholder)
typeInferenceStage :: Either PythonAST GoAST -> CompilerM (Either PythonAST GoAST)
typeInferenceStage ast = do
  logInfo "Running type inference analysis"
  -- TODO: Implement actual type inference
  addWarning $ TypeWarning "Type inference not fully implemented" (SourceSpan "<system>" (SourcePos 0 0) (SourcePos 0 0))
  return ast

-- | Optimization stage (placeholder)
optimizationStage :: Either PythonAST GoAST -> CompilerM (Either PythonAST GoAST)
optimizationStage ast = do
  config <- ask
  logInfo $ "Running optimizations at level " <> T.pack (show $ ccOptimizationLevel config)
  
  -- TODO: Implement actual optimizations
  -- - Escape analysis
  -- - Shape analysis
  -- - Ownership inference
  -- - Monomorphization
  -- - Devirtualization
  
  addWarning $ OptimizationWarning "Optimization passes not fully implemented"
  return ast

-- | Code generation stage
codeGenStage :: Either PythonAST GoAST -> CompilerM CppUnit
codeGenStage ast = do
  config <- ask
  
  let cppConfig = CppGenConfig
        { cgcOptimizationLevel = fromEnum $ ccOptimizationLevel config
        , cgcEnableInterop = ccEnableInterop config
        , cgcTargetCppStd = ccCppStandard config
        , cgcUseSmartPointers = ccOptimizationLevel config >= O2
        , cgcEnableParallel = ccEnableParallel config
        , cgcEnableCoroutines = ccCppStandard config >= "c++20"
        , cgcNamespace = "hyperstatic"
        , cgcHeaderGuard = "HYPERSTATIC_GENERATED"
        }
  
  return $ generateCpp cppConfig ast

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
      throwError $ CodeGenError errorMsg

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
  ]

-- | Build linker arguments
buildLinkerArgs :: CompilerConfig -> [FilePath] -> FilePath -> [Text]
buildLinkerArgs config objFiles outputPath = concat
  [ map T.pack objFiles
  , ["-o", T.pack outputPath]
  , concatMap (\path -> ["-L", T.pack path]) (ccLibraryPaths config)
  , concatMap (\lib -> ["-l" <> lib]) (ccLinkedLibraries config)
  , if ccEnableProfiler config then ["-pg"] else []
  ]

-- | Get optimization flags
optimizationFlags :: OptimizationLevel -> [Text]
optimizationFlags = \case
  O0 -> ["-O0"]
  O1 -> ["-O1"]
  O2 -> ["-O2"]
  O3 -> ["-O3", "-march=native"]
  Os -> ["-Os"]

-- | Utility functions
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

addWarning :: CompilerWarning -> CompilerM ()
addWarning warning = do
  modify $ \s -> s { csWarnings = warning : csWarnings s }
  logWarning $ T.pack $ show warning

logInfo :: Text -> CompilerM ()
logInfo msg = do
  config <- ask
  when (ccVerboseLevel config >= 1) $ 
    liftIO $ TIO.putStrLn $ "[INFO] " <> msg

logWarning :: Text -> CompilerM ()
logWarning msg = do
  config <- ask
  when (ccVerboseLevel config >= 1) $ 
    liftIO $ TIO.putStrLn $ "[WARN] " <> msg

logVerbose :: Text -> CompilerM ()
logVerbose msg = do
  config <- ask
  when (ccVerboseLevel config >= 2) $ 
    liftIO $ TIO.putStrLn $ "[VERBOSE] " <> msg

-- | Render C++ unit to text (placeholder)
renderCppUnit :: CppUnit -> Text
renderCppUnit unit = 
  "// Generated by HyperStatic/CXX Compiler\n" <>
  "// This is a placeholder C++ implementation\n" <>
  "#include <iostream>\n" <>
  "int main() {\n" <>
  "    std::cout << \"HyperStatic compiled program\" << std::endl;\n" <>
  "    return 0;\n" <>
  "}\n"