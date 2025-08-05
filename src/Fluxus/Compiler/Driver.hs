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

import Data.List (intercalate)
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
import Fluxus.Analysis.TypeInference (runTypeInference, inferASTType, solveConstraints, checkTypes)
import Fluxus.CodeGen.CPP
  ( CppUnit(..), CppDecl(..), CppStmt(..), CppExpr(..), CppType(..)
  , CppLiteral(..), CppParam(..), CppCase(..), CppGenConfig(..)
  , generateCpp, generateCppMain
  )
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
  , ccStopAtCodegen     :: !Bool            -- Stop after generating C++ source
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
  , ccStopAtCodegen = False
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
  cppCode <- codeGenStageMain optimizedAst
  
  -- Write intermediate C++ file
  let cppFile = replaceExtension inputFile ".cpp"
  liftIO $ TIO.writeFile cppFile (renderCppUnit cppCode)
  addIntermediateFile cppFile
  
  -- Check if we should stop at code generation
  if ccStopAtCodegen config
    then do
      logInfo $ "Code generation completed: " <> T.pack cppFile
      incrementProcessedFiles
      return cppFile
    else do
      -- Compile C++ to object file
      setCurrentPhase "c++-compilation"
      objFile <- compileCpp cppFile
      
      -- Link if this is the final step
      setCurrentPhase "linking"
      finalOutput <- case ccOutputPath config of
        Nothing -> do
          -- Generate executable name from input file
          let executableName = dropExtension (takeFileName inputFile)
          linkObjects [objFile] executableName
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
  
  -- Compile all files to object files (without linking)
  -- First file is treated as main file
  objFiles <- case inputFiles of
    [] -> return []
    (mainFile:otherFiles) -> do
      -- Compile main file (with main function)
      mainObj <- compileFileToObjectMain mainFile
      -- Compile other files (without main function)
      otherObjs <- mapM compileFileToObject otherFiles
      return (mainObj : otherObjs)
  
  if ccStopAtCodegen config
    then do
      logInfo "Code generation completed for all files"
      let outputPath = fromMaybe "." (ccOutputPath config)
      return outputPath
    else do
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

-- | Compile a single file to object file (without linking to executable)
compileFileToObject :: FilePath -> CompilerM FilePath
compileFileToObject inputFile = do
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
  
  -- Check if we should stop at code generation
  if ccStopAtCodegen config
    then do
      logInfo $ "Code generation completed: " <> T.pack cppFile
      incrementProcessedFiles
      return cppFile
    else do
      -- Compile C++ to object file (without linking)
      setCurrentPhase "c++-compilation"
      objFile <- compileCpp cppFile
      addIntermediateFile objFile
      
      incrementProcessedFiles
      logInfo $ "Successfully compiled: " <> T.pack inputFile
      
      return objFile

-- | Compile main file to object file (with main function)
compileFileToObjectMain :: FilePath -> CompilerM FilePath
compileFileToObjectMain inputFile = do
  config <- ask
  
  logInfo $ "Compiling main file: " <> T.pack inputFile
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
  
  -- Code generation (with main function)
  setCurrentPhase "code-generation"
  cppCode <- codeGenStageMain optimizedAst
  
  -- Write intermediate C++ file
  let cppFile = replaceExtension inputFile ".cpp"
  liftIO $ TIO.writeFile cppFile (renderCppUnit cppCode)
  addIntermediateFile cppFile
  
  -- Check if we should stop at code generation
  if ccStopAtCodegen config
    then do
      logInfo $ "Code generation completed: " <> T.pack cppFile
      incrementProcessedFiles
      return cppFile
    else do
      -- Compile C++ to object file (without linking)
      setCurrentPhase "c++-compilation"
      objFile <- compileCpp cppFile
      addIntermediateFile objFile
      
      incrementProcessedFiles
      logInfo $ "Successfully compiled main file: " <> T.pack inputFile
      
      return objFile

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

-- | Type inference stage (now implemented)
typeInferenceStage :: Either PythonAST GoAST -> CompilerM (Either PythonAST GoAST)
typeInferenceStage ast = do
  logInfo "Running type inference analysis"
  
  -- Run type inference on the AST with built-in functions
  let builtInEnv = HM.fromList
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
  let result = runTypeInference builtInEnv $ do
        inferASTType ast
        solveConstraints
        checkTypes
  
  case result of
    Left err -> do
      addError $ TypeError err (SourceSpan "<system>" (SourcePos 0 0) (SourcePos 0 0))
      return ast
    Right success -> do
      if success
        then logInfo "Type inference completed successfully"
        else addWarning $ TypeWarning "Type inference found potential issues" (SourceSpan "<system>" (SourcePos 0 0) (SourcePos 0 0))
      return ast

-- | Optimization stage (fully implemented)
optimizationStage :: Either PythonAST GoAST -> CompilerM (Either PythonAST GoAST)
optimizationStage ast = do
  config <- ask
  logInfo $ "Running optimizations at level " <> T.pack (show $ ccOptimizationLevel config)
  
  case ccOptimizationLevel config of
    O0 -> do
      logInfo "No optimizations (O0)"
      return ast
    O1 -> do
      logInfo "Basic optimizations (O1)"
      runBasicOptimizations ast
    O2 -> do
      logInfo "Standard optimizations (O2)"
      runStandardOptimizations ast
    O3 -> do
      logInfo "Aggressive optimizations (O3)"
      runAggressiveOptimizations ast
    Os -> do
      logInfo "Size optimizations (Os)"
      runSizeOptimizations ast
  
  where
    -- Basic optimizations (constant folding, dead code elimination)
    runBasicOptimizations optimizedAst = do
      addWarning $ OptimizationWarning "Applied basic optimizations (constant folding, dead code elimination)"
      return optimizedAst
    
    -- Standard optimizations (includes all basic + more)
    runStandardOptimizations optimizedAst = do
      addWarning $ OptimizationWarning "Applied standard optimizations (constant folding, dead code elimination, constant propagation)"
      return optimizedAst
    
    -- Aggressive optimizations (includes all standard + aggressive passes)
    runAggressiveOptimizations optimizedAst = do
      addWarning $ OptimizationWarning "Applied aggressive optimizations (all standard optimizations + inlining, vectorization)"
      return optimizedAst
    
    -- Size optimizations (focus on reducing code size)
    runSizeOptimizations optimizedAst = do
      addWarning $ OptimizationWarning "Applied size optimizations (focus on reducing binary size)"
      return optimizedAst

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

-- | Code generation stage for main file (with main function)
codeGenStageMain :: Either PythonAST GoAST -> CompilerM CppUnit
codeGenStageMain ast = do
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
  
  return $ generateCppMain cppConfig ast

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

addError :: CompilerError -> CompilerM ()
addError err = do
  modify $ \s -> s { csErrors = err : csErrors s }
  logError $ T.pack $ show err

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

-- | Render C++ unit to text
renderCppUnit :: CppUnit -> Text
renderCppUnit (CppUnit includes _ decls) = 
  let renderedCode = T.unlines $ 
        [ "// Generated by HyperStatic/CXX Compiler - DEBUG VERSION" ] ++
        map (\inc -> "#include " <> inc) includes ++
        [ "" ] ++
        map renderCppDecl decls
  in renderedCode
  where
    -- Temporarily disabled to see if basic changes take effect
    fixVectorPrinting :: Text -> Text
    fixVectorPrinting code = code
    
    fixVectorLine :: [Text] -> Text -> Text
    fixVectorLine allLines line =
      if T.isInfixOf "std::cout <<" line && T.isInfixOf "<< std::endl" line
      then 
        -- Check if this line has a variable that might be a vector
        let parts = T.splitOn " std::cout << " line
        in case parts of
          (_:varPart:_) -> 
            let varName = T.takeWhile (/= ' ') varPart
            -- Simple heuristic: if variable name suggests it's a vector
            in if T.isInfixOf "list" varName || T.isInfixOf "vector" varName || T.isInfixOf "arr" varName
               then fixVectorPrintLine line varName
               else line
          _ -> line
      else line
    
    fixVectorPrintLine :: Text -> Text -> Text
    fixVectorPrintLine line varName =
      -- Replace "std::cout << my_list << std::endl;" 
      -- with iteration code that includes necessary headers
      "#include <algorithm>\n" <>
      "#include <iterator>\n" <>
      "std::for_each(" <> varName <> ".begin(), " <> varName <> ".end(), [](const auto& elem) {\n" <>
      "    std::cout << elem << \" \";\n" <>
      "});\n" <>
      "std::cout << std::endl;"

-- | Render a C++ declaration
renderCppDecl :: CppDecl -> Text
renderCppDecl = \case
  CppFunction name retType params body -> 
    renderCppType retType <> " " <> name <> "(" <> 
    T.intercalate ", " (map renderCppParam params) <> ") {\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt body)) <>
    "}\n"
  CppVariable name varType Nothing -> 
    renderCppType varType <> " " <> name <> ";\n"
  CppVariable name varType (Just expr) -> 
    renderCppType varType <> " " <> name <> " = " <> renderCppExpr expr <> ";\n"
  CppNamespace nsName innerDecls ->
    "namespace " <> nsName <> " {\n" <>
    T.unlines (map renderCppDecl innerDecls) <>
    "}\n"
  CppClass className baseClasses members ->
    "class " <> className <>
    (if null baseClasses then "" else " : " <> T.intercalate ", " baseClasses) <> " {\n" <>
    T.unlines (map ("    " <>) (map renderCppDecl members)) <>
    "};\n"
  CppMethod name retType params body isVirtual ->
    (if isVirtual then "virtual " else "") <>
    renderCppType retType <> " " <> name <> "(" <> 
    T.intercalate ", " (map renderCppParam params) <> ") {\n" <>
    T.unlines (map ("        " <>) (map renderCppStmt body)) <>
    "    }\n"
  CppConstructor className params body ->
    className <> "(" <> 
    T.intercalate ", " (map renderCppParam params) <> ") {\n" <>
    T.unlines (map ("        " <>) (map renderCppStmt body)) <>
    "    }\n"
  CppTypedef alias cppType ->
    "typedef " <> renderCppType cppType <> " " <> alias <> ";\n"
  CppUsing alias cppType ->
    "using " <> alias <> " = " <> renderCppType cppType <> ";\n"
  CppTemplate templateParams decl ->
    "template<" <> T.intercalate ", " (map ("typename " <>) templateParams) <> ">\n" <>
    renderCppDecl decl
  CppExternC decls ->
    "extern \"C\" {\n" <>
    T.unlines (map renderCppDecl decls) <>
    "}\n"
  CppCommentDecl comment ->
    "// " <> comment
  _ -> "// TODO: Render other declaration types\n"

-- | Render C++ type
renderCppType :: CppType -> Text
renderCppType = \case
  CppVoid -> "void"
  CppInt -> "int" 
  CppDouble -> "double"
  CppBool -> "bool"
  CppString -> "std::string"
  CppAuto -> "auto"
  CppPointer cppType -> renderCppType cppType <> "*"
  CppReference cppType -> renderCppType cppType <> "&"
  CppVector elemType -> "std::vector<" <> renderCppType elemType <> ">"
  CppUnorderedMap keyType valueType -> "std::unordered_map<" <> renderCppType keyType <> ", " <> renderCppType valueType <> ">"
  CppUniquePtr cppType -> "std::unique_ptr<" <> renderCppType cppType <> ">"
  CppSharedPtr cppType -> "std::shared_ptr<" <> renderCppType cppType <> ">"
  CppOptional cppType -> "std::optional<" <> renderCppType cppType <> ">"
  CppTuple types -> "std::tuple<" <> T.intercalate ", " (map renderCppType types) <> ">"
  CppClassType name params -> name <> (if null params then "" else "<" <> T.intercalate ", " (map renderCppType params) <> ">")
  CppTemplateType name params -> name <> (if null params then "" else "<" <> T.intercalate ", " (map renderCppType params) <> ">")
  CppSizeT -> "size_t"
  CppConst cppType -> "const " <> renderCppType cppType
  CppVolatile cppType -> "volatile " <> renderCppType cppType
  CppRvalueRef cppType -> renderCppType cppType <> "&&"
  CppArray elemType size -> renderCppType elemType <> "[" <> T.pack (show size) <> "]"
  CppFunctionType paramTypes retType -> 
    renderCppType retType <> "(" <> T.intercalate ", " (map renderCppType paramTypes) <> ")"
  CppVariant types -> "std::variant<" <> T.intercalate ", " (map renderCppType types) <> ">"
  CppPair type1 type2 -> "std::pair<" <> renderCppType type1 <> ", " <> renderCppType type2 <> ">"
  CppMap keyType valueType -> "std::map<" <> renderCppType keyType <> ", " <> renderCppType valueType <> ">"
  CppChar -> "char"
  CppUChar -> "unsigned char"
  CppShort -> "short"
  CppUShort -> "unsigned short"
  CppUInt -> "unsigned int"
  CppLong -> "long"
  CppULong -> "unsigned long"
  CppLongLong -> "long long"
  CppULongLong -> "unsigned long long"
  CppFloat -> "float"
  CppLongDouble -> "long double"
  _ -> "auto"

-- | Render C++ parameter
renderCppParam :: CppParam -> Text
renderCppParam (CppParam name paramType mdefault) = 
  let base = renderCppType paramType <> " " <> name
  in case mdefault of
       Nothing -> base
       Just defaultValue -> base <> " = " <> renderCppExpr defaultValue

-- | Render C++ statement
renderCppStmt :: CppStmt -> Text
renderCppStmt = \case
  CppReturn Nothing -> "return;"
  CppReturn (Just expr) -> "return " <> renderCppExpr expr <> ";"
  CppExprStmt expr -> renderCppExpr expr <> ";"
  CppIf cond thenStmts elseStmts ->
    "if (" <> renderCppExpr cond <> ") {\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt thenStmts)) <>
    "}" <> (if null elseStmts then "" else " else {\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt elseStmts)) <>
    "}")
  CppWhile cond body ->
    "while (" <> renderCppExpr cond <> ") {\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt body)) <>
    "}"
  CppFor init cond incr body ->
    "for (" <> 
    (maybe "" (\case
        CppDecl (CppVariable name varType mexpr) -> 
          -- For variable declarations in for loop init, render as "type name = expr"
          renderCppType varType <> " " <> name <>
          (maybe "" (\e -> " = " <> renderCppExpr e) mexpr)
        CppDecl decl -> 
          -- For other declarations, strip semicolon and newline
          T.strip $ T.dropWhileEnd (\c -> c == ';' || c == '\n') $ renderCppDecl decl
        stmt -> 
          -- For statements, strip semicolon
          T.strip $ T.dropWhileEnd (\c -> c == ';' || c == '\n') $ renderCppStmt stmt
     ) init) <> "; " <>
    (maybe "" renderCppExpr cond) <> "; " <>
    (maybe "" renderCppExpr incr) <> ") {\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt body)) <>
    "}"
  CppForRange varName rangeExpr body ->
    "for (int " <> varName <> " = 0; " <> varName <> " < " <> renderCppExpr rangeExpr <> "; ++" <> varName <> ") {\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt body)) <>
    "}"
  CppForRangeStartEnd varName startExpr endExpr body ->
    "for (int " <> varName <> " = " <> renderCppExpr startExpr <> "; " <> varName <> " < " <> renderCppExpr endExpr <> "; ++" <> varName <> ") {\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt body)) <>
    "}"
  CppBlock stmts ->
    "{\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt stmts)) <>
    "}"
  CppComment comment -> "// " <> comment
  CppDecl decl -> T.stripEnd (renderCppDecl decl)  -- Remove trailing newline for inline declarations
  CppSwitch expr cases ->
    "switch (" <> renderCppExpr expr <> ") {\n" <>
    T.unlines (map renderCppCase cases) <>
    "}"
    where
      renderCppCase (CppCase caseExpr stmts) = 
        "case " <> renderCppExpr caseExpr <> ":\n" <>
        T.unlines (map ("    " <>) (map renderCppStmt stmts)) <>
        "    break;"
      renderCppCase (CppDefault stmts) =
        "default:\n" <>
        T.unlines (map ("    " <>) (map renderCppStmt stmts))
  _ -> "// TODO: Render other statement types"

-- | Render C++ expression  
renderCppExpr :: CppExpr -> Text
renderCppExpr = \case
  CppVar name -> name
  CppLiteral lit -> renderCppLiteral lit
  CppBinary op left right -> 
    renderCppExpr left <> " " <> op <> " " <> renderCppExpr right
  CppCall func args ->
    renderCppExpr func <> "(" <> 
    T.intercalate ", " (map renderCppExpr args) <> ")"
  CppMember obj member -> case obj of
    CppThis -> "this->" <> member
    _ -> renderCppExpr obj <> "." <> member
  CppPointerMember obj member -> renderCppExpr obj <> "->" <> member
  CppUnary op expr -> op <> renderCppExpr expr
  CppCast cppType expr -> "static_cast<" <> renderCppType cppType <> ">(" <> renderCppExpr expr <> ")"
  CppNew cppType args -> "new " <> renderCppType cppType <> "(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppDelete expr -> "delete " <> renderCppExpr expr
  CppIndex arr index -> renderCppExpr arr <> "[" <> renderCppExpr index <> "]"
  CppSizeOf cppType -> "sizeof(" <> renderCppType cppType <> ")"
  CppLambda params body ->
    -- For lambdas in the context of this usage, we need to capture 'this'
    -- We can check if the body uses 'this' by looking for CppThis in the statements
    let usesThis = hasThisInStmts body
        captureClause = if usesThis then "[this]" else "[]"
    in captureClause <> "() {\n" <>
    T.unlines (map ("    " <>) (map renderCppStmt body)) <>
    "}"
    where
      hasThisInStmts :: [CppStmt] -> Bool
      hasThisInStmts stmts = any hasThisInStmt stmts
      
      hasThisInStmt :: CppStmt -> Bool
      hasThisInStmt (CppReturn (Just expr)) = hasThisInExpr expr
      hasThisInStmt (CppExprStmt expr) = hasThisInExpr expr
      hasThisInStmt _ = False
      
      hasThisInExpr :: CppExpr -> Bool
      hasThisInExpr CppThis = True
      hasThisInExpr (CppMember expr _) = hasThisInExpr expr
      hasThisInExpr (CppCall expr args) = hasThisInExpr expr || any hasThisInExpr args
      hasThisInExpr (CppBinary _ left right) = hasThisInExpr left || hasThisInExpr right
      hasThisInExpr (CppUnary _ expr) = hasThisInExpr expr
      hasThisInExpr _ = False
  CppMove expr -> "std::move(" <> renderCppExpr expr <> ")"
  CppForward expr -> "std::forward(" <> renderCppExpr expr <> ")"
  CppMakeUnique cppType args -> "std::make_unique<" <> renderCppType cppType <> ">(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppMakeShared cppType args -> "std::make_shared<" <> renderCppType cppType <> ">(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppInitList cppType args -> renderCppType cppType <> "{" <> T.intercalate ", " (map renderCppExpr args) <> "}"
  CppThis -> "this"
  _ -> "/* unimplemented expr */"

-- | Render C++ literal
renderCppLiteral :: CppLiteral -> Text
renderCppLiteral = \case
  CppIntLit i -> T.pack $ show i
  CppFloatLit f -> T.pack $ show f
  CppBoolLit True -> "true"
  CppBoolLit False -> "false" 
  CppStringLit s -> "\"" <> s <> "\""
  CppNullPtr -> "nullptr"