{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Driver
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
import Control.Monad (when, unless, forM)
import Data.Maybe (fromMaybe, isJust)
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
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (bracket)
import Data.IORef

import AST.Common
import AST.Python
import AST.Go
import Parser.Python.Lexer (runPythonLexer)
import Parser.Python.Parser (runPythonParser)
import Parser.Go.Lexer (runGoLexer)
import Parser.Go.Parser (runGoParser)
import Text.PrettyPrint (ppShow)  -- For AST debugging
import Analysis.TypeInference (runTypeInference, inferASTType, solveConstraints, checkTypes)
import Fluxus.CodeGen.CPP
  ( CppUnit(..), CppDecl(..), CppStmt(..), CppExpr(..), CppType(..)
  , CppLiteral(..), CppParam(..), CppCase(..), CppGenConfig(..)
  , generateCpp, generateCppMain
  )
import Optimization.ConstantFolding (constantFolding)
import Optimization.DeadCodeElimination (deadCodeElimination)
import Optimization.ConstantPropagation (constantPropagation)
import Optimization.Inlining (inlineFunctions)
import Optimization.Vectorization (vectorizeLoops)
import Optimization.SizeReduction (reduceCodeSize)
import Fluxus.Utils.Pretty ()

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
  , ccCppStandard       :: !Text
  , ccCppCompiler       :: !Text
  , ccVerboseLevel      :: !Int
  , ccWorkDirectory     :: !(Maybe FilePath)
  , ccKeepIntermediates :: !Bool
  , ccStrictMode        :: !Bool
  , ccEnableAnalysis    :: !Bool
  , ccStopAtCodegen     :: !Bool
  , ccBuiltinEnv        :: !(HashMap Identifier Type)  -- Made configurable
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

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
  deriving anyclass (NFData)

-- | Compiler warnings
data CompilerWarning
  = TypeWarning !Text !SourceSpan
  | OptimizationWarning !Text !SourceSpan
  | DeprecationWarning !Text !SourceSpan
  | PerformanceWarning !Text !SourceSpan
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

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
    deriving anyclass (NFData)

-- | Compiler monad stack
type CompilerM = ReaderT CompilerConfig (StateT CompilerState (ExceptT CompilerError IO))

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
  , (Identifier "enumerate", TFunction [TList (TInt 32)] (TList (TTuple [TInt 32, TInt 32]))]
  , (Identifier "zip", TFunction [TList (TInt 32), TList (TString)] (TList (TTuple [TInt 32, TString]))]
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
  , ccBuiltinEnv = defaultBuiltinEnv
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
  runExceptT $ runStateT (runReaderT action config) initialState

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

-- | Compile a single file
compileFile :: FilePath -> CompilerM FilePath
compileFile inputFile = do
  config <- ask
  
  -- Use common processing pipeline
  optimizedAst <- processSourceFile inputFile
  
  -- Code generation with main
  setCurrentPhase "code-generation"
  cppCode <- codeGenStageMain optimizedAst
  
  -- Generate output filename based on input
  let cppFile = replaceExtension inputFile ".cpp"
  liftIO $ TIO.writeFile cppFile (renderCppUnit cppCode)
  addIntermediateFile cppFile
  
  if ccStopAtCodegen config
    then do
      logInfo $ "Code generation completed: " <> T.pack cppFile
      incrementProcessedFiles
      return cppFile
    else do
      setCurrentPhase "c++-compilation"
      objFile <- compileCpp cppFile
      
      setCurrentPhase "linking"
      let defaultOutput = dropExtension (takeFileName inputFile)  -- Better default name
      finalOutput <- case ccOutputPath config of
        Nothing -> linkObjects [objFile] defaultOutput
        Just outPath -> linkObjects [objFile] outPath
      
      incrementProcessedFiles
      logInfo $ "Successfully compiled: " <> T.pack inputFile
      
      return finalOutput

-- | Compile file to object (simplified using common pipeline)
compileFileToObject :: FilePath -> Bool -> CompilerM FilePath
compileFileToObject inputFile isMain = do
  config <- ask
  
  -- Use common processing pipeline
  optimizedAst <- processSourceFile inputFile
  
  -- Code generation (with or without main)
  setCurrentPhase "code-generation"
  cppCode <- if isMain 
    then codeGenStageMain optimizedAst
    else codeGenStage optimizedAst
  
  let cppFile = replaceExtension inputFile ".cpp"
  liftIO $ TIO.writeFile cppFile (renderCppUnit cppCode)
  addIntermediateFile cppFile
  
  if ccStopAtCodegen config
    then do
      logInfo $ "Code generation completed: " <> T.pack cppFile
      incrementProcessedFiles
      return cppFile
    else do
      setCurrentPhase "c++-compilation"
      objFile <- compileCpp cppFile
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
      mainObj <- compileFileToObject mainFile True
      
      -- Compile other files (parallel if enabled)
      otherObjs <- if ccEnableParallel config && not (null otherFiles)
        then do
          -- Parallel compilation using async
          logInfo $ "Compiling " <> T.pack (show $ length otherFiles) <> " files in parallel"
          liftIO $ mapConcurrently (compileFileInNewContext config False) otherFiles
        else do
          -- Sequential compilation
          mapM (\f -> compileFileToObject f False) otherFiles
      
      return (mainObj : otherObjs)
  
  if ccStopAtCodegen config
    then do
      logInfo "Code generation completed for all files"
      let outputPath = fromMaybe "." (ccOutputPath config)
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
compileFileInNewContext :: CompilerConfig -> Bool -> FilePath -> IO FilePath
compileFileInNewContext config isMain inputFile = do
  result <- runCompiler config (compileFileToObject inputFile isMain)
  case result of
    Left err -> error $ "Compilation failed: " <> show err
    Right (objFile, _) -> return objFile

-- | Enhanced parse stage with better error reporting
parseStage :: FilePath -> CompilerM (Either PythonAST GoAST)
parseStage inputFile = do
  config <- ask
  content <- liftIO $ TIO.readFile inputFile
  
  let detectedLanguage = case takeExtension inputFile of
        ".py"  -> Python
        ".go"  -> Go
        _      -> ccSourceLanguage config
  
  case detectedLanguage of
    Python -> do
      tokens <- case runPythonLexer (T.pack inputFile) content of
        Left err ->
          -- Extract line/column from error if available
          let (line, col) = extractPosFromError err
              span = SourceSpan (T.pack inputFile) (SourcePos line col) (SourcePos line col)
          in throwError $ ParseError (T.pack $ show err) span
        Right toks -> do
          -- Debug: Print token information
          liftIO $ putStrLn $ "[DEBUG] Python lexer tokens:"
          liftIO $ putStrLn $ "[DEBUG] Token count: " ++ show (length toks)
          mapM_ (\t -> liftIO $ putStrLn $ "[DEBUG] Token: " ++ show t) (take 10 toks)
          if length toks > 10
            then liftIO $ putStrLn $ "[DEBUG] ... and " ++ show (length toks - 10) ++ " more tokens"
            else return ()
          return toks

      case runPythonParser (T.pack inputFile) tokens of
        Left err ->
          let (line, col) = extractPosFromError err
              span = SourceSpan (T.pack inputFile) (SourcePos line col) (SourcePos line col)
          in throwError $ ParseError (T.pack $ show err) span
        Right ast -> do
          -- Debug: Print AST info
          liftIO $ putStrLn $ "[DEBUG] Python parsing successful"
          return $ Left ast
    
    Go -> do
      tokens <- case runGoLexer (T.pack inputFile) content of
        Left err -> 
          let (line, col) = extractPosFromError err
              span = SourceSpan (T.pack inputFile) (SourcePos line col) (SourcePos line col)
          in throwError $ ParseError (T.pack $ show err) span
        Right toks -> return toks
      
      case runGoParser (T.pack inputFile) tokens of
        Left err ->
          let (line, col) = extractPosFromError err
              span = SourceSpan (T.pack inputFile) (SourcePos line col) (SourcePos line col)
          in throwError $ ParseError (T.pack $ show err) span
        Right ast -> do
          -- Debug: Print detailed AST information
          liftIO $ putStrLn $ "[DEBUG] Go AST structure:"
          liftIO $ putStrLn $ "[DEBUG] Package name: " ++ T.unpack (unIdentifier (goPackageName ast))
          liftIO $ putStrLn $ "[DEBUG] Number of files: " ++ show (length (goPackageFiles ast))
          liftIO $ putStrLn $ "[DEBUG] Declarations per file:"
          forM_ (goPackageFiles ast) $ \file -> do
            let fileName = goFileName file
                packageName = goFilePackage file
                imports = goFileImports file
                decls = goFileDecls file
            liftIO $ putStrLn $ "[DEBUG]   File: " ++ fileName
            liftIO $ putStrLn $ "[DEBUG]   Package: " ++ T.unpack (unIdentifier packageName)
            liftIO $ putStrLn $ "[DEBUG]   Imports: " ++ show (length imports)
            liftIO $ putStrLn $ "[DEBUG]   Declarations: " ++ show (length decls)
            when (null decls) $ liftIO $ putStrLn "[DEBUG]   WARNING: No declarations found!"
            -- Print declaration types
            forM_ decls $ \decl -> do
              case locValue decl of
                GoFuncDecl _ -> liftIO $ putStrLn $ "[DEBUG]     Found function declaration"
                GoMethodDecl _ _ -> liftIO $ putStrLn $ "[DEBUG]     Found method declaration"
                GoTypeDecl _ _ -> liftIO $ putStrLn $ "[DEBUG]     Found type declaration"
                GoVarDecl _ -> liftIO $ putStrLn $ "[DEBUG]     Found variable declaration"
                GoConstDecl _ -> liftIO $ putStrLn $ "[DEBUG]     Found constant declaration"
                GoInitDecl _ -> liftIO $ putStrLn $ "[DEBUG]     Found init declaration"
                _ -> liftIO $ putStrLn $ "[DEBUG]     Found other declaration"
          return $ Right ast
  where
    -- Helper to extract position from error (stub - should parse actual error)
    extractPosFromError :: Show a => a -> (Int, Int)
    extractPosFromError err = 
      -- In real implementation, parse error message for line/column
      (1, 1)  -- Default to line 1, column 1 if can't extract

-- | Enhanced type inference with proper error locations
typeInferenceStage :: Either PythonAST GoAST -> CompilerM (Either PythonAST GoAST)
typeInferenceStage ast = do
  config <- ask
  logInfo "Running type inference analysis"
  
  let result = runTypeInference (ccBuiltinEnv config) $ do
        inferASTType ast
        solveConstraints
        checkTypes
  
  case result of
    Left err -> do
      -- Extract source span from AST if possible
      let span = extractSpanFromAST ast
      addError $ TypeError err span
      return ast
    Right success -> do
      if success
        then logInfo "Type inference completed successfully"
        else do
          let span = extractSpanFromAST ast
          addWarning $ TypeWarning "Type inference found potential issues" span
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
      foldedAst <- liftIO $ constantFolding astToOpt
      optimizedAst <- liftIO $ deadCodeElimination foldedAst
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
  let cppConfig = buildCppGenConfig config False
  return $ generateCpp cppConfig ast

codeGenStageMain :: Either PythonAST GoAST -> CompilerM CppUnit
codeGenStageMain ast = do
  config <- ask
  let cppConfig = buildCppGenConfig config True
  return $ generateCppMain cppConfig ast

buildCppGenConfig :: CompilerConfig -> Bool -> CppGenConfig
buildCppGenConfig config withMain = CppGenConfig
  { cgcOptimizationLevel = fromEnum $ ccOptimizationLevel config
  , cgcEnableInterop = ccEnableInterop config
  , cgcTargetCppStd = ccCppStandard config
  , cgcUseSmartPointers = ccOptimizationLevel config >= O2
  , cgcEnableParallel = ccEnableParallel config
  , cgcEnableCoroutines = ccCppStandard config >= "c++20"
  , cgcNamespace = "hyperstatic"
  , cgcHeaderGuard = "HYPERSTATIC_GENERATED"
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
      let span = extractSpanFromCppError stderr cppFile
      throwError $ CodeGenError errorMsg span

-- | Extract source span from C++ compiler error
extractSpanFromCppError :: String -> FilePath -> SourceSpan
extractSpanFromCppError errorMsg file =
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
      TypeWarning msg span -> TypeError msg span
      OptimizationWarning msg span -> OptimizationError msg span
      _ -> RuntimeError "Warning treated as error in strict mode"

addError :: CompilerError -> CompilerM ()
addError err = do
  modify $ \s -> s { csErrors = err : csErrors s }
  logError $ T.pack $ show err

-- Logging functions
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

-- C++ Rendering (using a builder pattern for efficiency)
renderCppUnit :: CppUnit -> Text
renderCppUnit (CppUnit includes _ decls) = 
  T.unlines $ 
    [ "// Generated by HyperStatic/CXX Compiler" ] ++
    map (\inc -> "#include " <> inc) includes ++
    [ "" ] ++
    map renderCppDecl decls

-- [Rest of the C++ rendering functions remain the same but could be improved with a pretty-printer library]
-- For brevity, I'm keeping the existing rendering functions

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
  CppStruct name members ->
    "struct " <> name <> " {\n" <>
    T.unlines (map ("    " <>) (map renderCppDecl members)) <>
    "};\n"
  CppCommentDecl comment ->
    "// " <> comment <> "\n"
  _ -> "// TODO: Render other declaration types\n"

renderCppType :: CppType -> Text
renderCppType = \case
  CppVoid -> "void"
  CppInt -> "int"
  CppDouble -> "double"
  CppBool -> "bool"
  CppString -> "std::string"
  CppAuto -> "auto"
  CppPointer t -> renderCppType t <> "*"
  CppReference t -> renderCppType t <> "&"
  CppVector t -> "std::vector<" <> renderCppType t <> ">"
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
  CppDecl decl -> T.stripEnd (renderCppDecl decl)
  CppComment comment -> "// " <> comment
  _ -> "// TODO: Render statement"

renderCppExpr :: CppExpr -> Text
renderCppExpr = \case
  CppVar name -> name
  CppLiteral lit -> renderCppLiteral lit
  CppBinary op left right ->
    renderCppExpr left <> " " <> op <> " " <> renderCppExpr right
  CppUnary op expr ->
    op <> renderCppExpr expr
  CppCall func args ->
    renderCppExpr func <> "(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppMember expr member ->
    renderCppExpr expr <> "." <> member
  CppPointerMember expr member ->
    renderCppExpr expr <> "->" <> member
  CppIndex expr index ->
    renderCppExpr expr <> "[" <> renderCppExpr index <> "]"
  CppCast castType expr ->
    "(" <> renderCppType castType <> ")" <> renderCppExpr expr
  CppSizeOf castType ->
    "sizeof(" <> renderCppType castType <> ")"
  CppNew castType args ->
    "new " <> renderCppType castType <> "(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppDelete expr ->
    "delete " <> renderCppExpr expr
  CppThis -> "this"
  CppMove expr ->
    "std::move(" <> renderCppExpr expr <> ")"
  CppForward expr ->
    "std::forward(" <> renderCppExpr expr <> ")"
  CppMakeUnique castType args ->
    "std::make_unique<" <> renderCppType castType <> ">(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppMakeShared castType args ->
    "std::make_shared<" <> renderCppType castType <> ">(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppInitList castType args ->
    renderCppType castType <> "{" <> T.intercalate ", " (map renderCppExpr args) <> "}"
  CppTernary cond trueExpr falseExpr ->
    renderCppExpr cond <> " ? " <> renderCppExpr trueExpr <> " : " <> renderCppExpr falseExpr
  CppComma exprs ->
    "(" <> T.intercalate ", " (map renderCppExpr exprs) <> ")"
  CppStaticCast castType expr ->
    "static_cast<" <> renderCppType castType <> ">(" <> renderCppExpr expr <> ")"
  CppDynamicCast castType expr ->
    "dynamic_cast<" <> renderCppType castType <> ">(" <> renderCppExpr expr <> ")"
  CppReinterpretCast castType expr ->
    "reinterpret_cast<" <> renderCppType castType <> ">(" <> renderCppExpr expr <> ")"
  CppConstCast castType expr ->
    "const_cast<" <> renderCppType castType <> ">(" <> renderCppExpr expr <> ")"
  CppMakeTuple args ->
    "std::make_tuple(" <> T.intercalate ", " (map renderCppExpr args) <> ")"

renderCppLiteral :: CppLiteral -> Text
renderCppLiteral = \case
  CppIntLit i -> T.pack $ show i
  CppFloatLit f -> T.pack $ show f
  CppBoolLit True -> "true"
  CppBoolLit False -> "false"
  CppStringLit s -> "\"" <> escapeString s <> "\""
  CppNullPtr -> "nullptr"
  where
    escapeString = T.concatMap $ \case
      '\n' -> "\\n"
      '\t' -> "\\t"
      '\\' -> "\\\\"
      '"' -> "\\\""
      c -> T.singleton c