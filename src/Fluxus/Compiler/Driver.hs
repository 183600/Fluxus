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
  , showTargetPlatform
  ) where

import Data.List (intercalate, foldl')
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad (when, unless, forM_, foldM)
import Data.Maybe (fromMaybe, maybeToList, catMaybes)
import Data.Either (partitionEithers)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:?), (.!=), withObject, object)
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
import Fluxus.Analysis.TypeInference
  ( TypeInferenceState(..)
  , InferenceResult(..)
  , runTypeInference
  , inferType
  , solveConstraints
  , applySubstitution
  )
import Fluxus.Analysis.EscapeAnalysis
  ( EscapeAnalysisState(..)
  , runEscapeAnalysis
  , optimizeMemoryAllocation
  )
import Fluxus.Analysis.ShapeAnalysis
  ( ShapeAnalysisState(..)
  , runShapeAnalysis
  , analyzeShape
  )
import Fluxus.Analysis.OwnershipInference
  ( OwnershipResult(..)
  , OwnershipInferenceState(..)
  , OwnershipStrategy(..)
  , runOwnershipInference
  , inferOwnership
  )
import Fluxus.Analysis.SmartFallback
  ( runSmartFallback
  , shouldFallbackToRuntime
  , optimizeWithFallback
  )
import Fluxus.Optimization.Monomorphization
  ( MonomorphizationResult(..)
  , MonomorphizationState(..)
  , runMonomorphization
  , monomorphize
  )
import Fluxus.Optimization.Devirtualization
  ( DevirtualizationResult(..)
  , DevirtualizationState(..)
  , runDevirtualization
  , devirtualize
  )
import Fluxus.Parser.Python.Lexer (runPythonLexer)
import Fluxus.Parser.Python.Parser (runPythonParser)
import Fluxus.Parser.Go.Lexer (runGoLexer)
import Fluxus.Parser.Go.Parser (runGoParser, GoParseError(..))
import Fluxus.CodeGen.CPP
  ( CppUnit(..), CppDecl(..), CppStmt(..), CppExpr(..), CppType(..)
  , CppLiteral(..), CppParam(..), CppCase(..), CppGenConfig(..)
  , generateCpp
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

-- | Convert target platform to configuration string
showTargetPlatform :: TargetPlatform -> String
showTargetPlatform = \case
  Linux_x86_64 -> "linux-x86_64"
  Linux_ARM64 -> "linux-arm64"
  Darwin_x86_64 -> "darwin-x86_64"
  Darwin_ARM64 -> "darwin-arm64"
  Windows_x86_64 -> "windows-x86_64"

instance ToJSON CompilerConfig where
  toJSON config = object
    [ "source_language" .= show (ccSourceLanguage config)
    , "optimization_level" .= show (ccOptimizationLevel config)
    , "target_platform" .= showTargetPlatform (ccTargetPlatform config)
    , "output_path" .= ccOutputPath config
    , "enable_interop" .= ccEnableInterop config
    , "enable_debug_info" .= ccEnableDebugInfo config
    , "enable_profiler" .= ccEnableProfiler config
    , "enable_parallel" .= ccEnableParallel config
    , "max_concurrency" .= ccMaxConcurrency config
    , "include_paths" .= ccIncludePaths config
    , "library_paths" .= ccLibraryPaths config
    , "linked_libraries" .= ccLinkedLibraries config
    , "cpp_standard" .= ccCppStandard config
    , "cpp_compiler" .= ccCppCompiler config
    , "verbose_level" .= ccVerboseLevel config
    , "work_directory" .= ccWorkDirectory config
    , "keep_intermediates" .= ccKeepIntermediates config
    , "strict_mode" .= ccStrictMode config
    , "enable_analysis" .= ccEnableAnalysis config
    ]

instance FromJSON CompilerConfig where
  parseJSON = withObject "CompilerConfig" $ \o -> do
    sourceLang <- o .:? "source_language" .!= ("Python" :: String)
    optLevel <- o .:? "optimization_level" .!= ("O2" :: String)
    targetStr <- o .:? "target_platform" .!= ("linux-x86_64" :: String)

    let parseSourceLanguage = \case
          "Python" -> Python
          "Go" -> Go
          _ -> Python
        parseOptLevel = \case
          "O0" -> O0
          "O1" -> O1
          "O2" -> O2
          "O3" -> O3
          "Os" -> Os
          _ -> O2
        parseTargetPlatformStr = \case
          "linux-x86_64" -> Just Linux_x86_64
          "linux-arm64" -> Just Linux_ARM64
          "darwin-x86_64" -> Just Darwin_x86_64
          "darwin-arm64" -> Just Darwin_ARM64
          "windows-x86_64" -> Just Windows_x86_64
          _ -> Nothing

    CompilerConfig
      <$> pure (parseSourceLanguage sourceLang)
      <*> pure (parseOptLevel optLevel)
      <*> pure (fromMaybe Linux_x86_64 (parseTargetPlatformStr targetStr))
      <*> o .:? "output_path"
      <*> o .:? "enable_interop" .!= True
      <*> o .:? "enable_debug_info" .!= False
      <*> o .:? "enable_profiler" .!= False
      <*> o .:? "enable_parallel" .!= True
      <*> o .:? "max_concurrency" .!= 4
      <*> o .:? "include_paths" .!= ["/usr/include", "/usr/local/include"]
      <*> o .:? "library_paths" .!= ["/usr/lib", "/usr/local/lib"]
      <*> o .:? "linked_libraries" .!= ["stdc++", "pthread"]
      <*> o .:? "cpp_standard" .!= "c++20"
      <*> o .:? "cpp_compiler" .!= "clang++"
      <*> o .:? "verbose_level" .!= 1
      <*> o .:? "work_directory"
      <*> o .:? "keep_intermediates" .!= False
      <*> o .:? "strict_mode" .!= False
      <*> o .:? "enable_analysis" .!= True
      <*> o .:? "stop_at_codegen" .!= False

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
  
  -- Compile all files to object files
  objFiles <- mapM compileFile inputFiles
  
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
        Left err -> throwError $ ParseError (peMessage err) (peLocation err)
        Right ast -> return $ Right ast

-- | Type inference stage
typeInferenceStage :: Either PythonAST GoAST -> CompilerM (Either PythonAST GoAST)
typeInferenceStage ast = do
  logInfo "Running type inference analysis"
  let (commonExprs, extractionIssues) = collectCommonExpressions ast
  forM_ extractionIssues $ \msg ->
    addWarning $ TypeWarning msg systemSpan
  when (null commonExprs) $ do
    addWarning $ TypeWarning "No analyzable expressions found for type inference" systemSpan
  if null commonExprs
    then return ast
    else do
      envSnapshot <- gets csTypeEnvironment
      let initialEnv = HM.fromList $ map (\(name, ty) -> (Identifier name, ty)) (HM.toList envSnapshot)
      (successes, failures) <- foldM (inferExpression initialEnv) (0 :: Int, 0 :: Int) commonExprs
      let total = successes + failures
      logInfo $ "Type inference summary: " <> textShow successes <> "/" <> textShow total <> " expressions inferred"
      when (failures > 0) $
        addWarning $ TypeWarning ("Failed to infer types for " <> textShow failures <> " expressions") systemSpan
      return ast
  where
    inferExpression env (okCount, errCount) expr = 
      case runTypeInference env $ do
        result <- inferType expr
        solveConstraints
        st <- get
        let subst = substitutions st
            finalType = applySubstitution subst (resultType result)
        pure finalType
      of
        Left err -> do
          addWarning $ TypeWarning ("Type inference failed: " <> err) systemSpan
          recordOptimizationStat "type-inference.failure"
          pure (okCount, errCount + 1)
        Right inferredType -> do
          let exprKey = renderCommonExpr expr
          modify $ \s -> s { csTypeEnvironment = HM.insert exprKey inferredType (csTypeEnvironment s) }
          recordOptimizationStat "type-inference.success"
          logVerbose $ "Inferred type for " <> exprKey <> ": " <> renderType inferredType
          pure (okCount + 1, errCount)

-- | Optimization stage
optimizationStage :: Either PythonAST GoAST -> CompilerM (Either PythonAST GoAST)
optimizationStage ast = do
  config <- ask
  logInfo $ "Running optimizations at level " <> T.pack (show $ ccOptimizationLevel config)
  let (commonExprs, extractionIssues) = collectCommonExpressions ast
  unless (null extractionIssues) $
    logVerbose $ "Skipping " <> textShow (length extractionIssues) <> " expressions during optimization due to unsupported constructs"
  when (null commonExprs) $ do
    addWarning $ OptimizationWarning "No analyzable expressions found for optimization pipeline"
  forM_ commonExprs $ \expr -> do
    let exprLabel = renderCommonExpr expr
    recordOptimizationStat "optimization.expressions"
    let ((escapeOptimized, escapeHints), escapeState) = runEscapeAnalysis (optimizeMemoryAllocation expr)
    recordOptimizationStat "optimization.escape"
    forM_ escapeHints $ \hint ->
      addWarning $ OptimizationWarning ("Escape analysis hint for " <> exprLabel <> ": " <> hint)
    let heapEscapes = length (easHeapEscapes escapeState)
    when (heapEscapes > 0) $
      recordOptimizationStatN "optimization.escape.heap" heapEscapes
    case runOwnershipInference (inferOwnership escapeOptimized) of
      Left err -> addWarning $ OptimizationWarning ("Ownership inference failed for " <> exprLabel <> ": " <> err)
      Right (ownershipResult, ownershipState) -> do
        recordOptimizationStat "optimization.ownership"
        let strategyTag = case orStrategy ownershipResult of
              StackOwned -> "stack"
              UniqueOwnership -> "unique"
              SharedOwnership -> "shared"
              BorrowedReference -> "borrowed"
              MoveSemantics -> "move"
              CopySemantics -> "copy"
              WeakReference -> "weak"
              CustomRAII -> "custom-raii"
        recordOptimizationStat ("optimization.ownership." <> strategyTag)
        forM_ (oisOptimizationHints ownershipState) $ \hint ->
          addWarning $ OptimizationWarning ("Ownership hint for " <> exprLabel <> ": " <> hint)
    case runShapeAnalysis (analyzeShape escapeOptimized) of
      Left err -> addWarning $ OptimizationWarning ("Shape analysis failed for " <> exprLabel <> ": " <> err)
      Right (_shapeInfo, shapeState) -> do
        recordOptimizationStat "optimization.shape"
        forM_ (sasOptimizations shapeState) $ \hint ->
          addWarning $ OptimizationWarning ("Shape analysis suggestion for " <> exprLabel <> ": " <> hint)
    let (fallbackRequired, _) = runSmartFallback (shouldFallbackToRuntime escapeOptimized)
    when fallbackRequired $ do
      recordOptimizationStat "optimization.fallback.runtime"
      addWarning $ OptimizationWarning ("Runtime fallback recommended for " <> exprLabel)
    let (fallbackExpr, _) = runSmartFallback (optimizeWithFallback escapeOptimized)
    let (monoResult, monoState) = runMonomorphization (monomorphize fallbackExpr)
    recordOptimizationStat "optimization.monomorphization"
    when (not (null (mrOptimizations monoResult))) $
      forM_ (mrOptimizations monoResult) $ \msg ->
        addWarning $ OptimizationWarning ("Monomorphization note for " <> exprLabel <> ": " <> msg)
    recordOptimizationStatN "optimization.specializations" (HM.size (msSpecializations monoState))
    let (devirtResult, devirtState) = runDevirtualization (devirtualize (mrExpression monoResult))
    recordOptimizationStat "optimization.devirtualization"
    when (not (null (drOptimizations devirtResult))) $
      forM_ (drOptimizations devirtResult) $ \msg ->
        addWarning $ OptimizationWarning ("Devirtualization note for " <> exprLabel <> ": " <> msg)
    let resolvedCount = HM.size (dsResolvedCalls devirtState)
    when (resolvedCount > 0) $
      recordOptimizationStatN "optimization.devirtualization.resolved" resolvedCount
    when (drExpression devirtResult /= expr) $
      recordOptimizationStat "optimization.expr.changed"
  return ast

-- | Collect analyzable expressions that can be fed into the shared analysis passes.
collectCommonExpressions :: Either PythonAST GoAST -> ([CommonExpr], [Text])
collectCommonExpressions = \case
  Left (PythonAST pyModule) ->
    let pythonExprs = collectPythonExpressions pyModule
        (issues, commons) = partitionEithers (map pythonExprToCommon pythonExprs)
    in (commons, issues)
  Right (GoAST goPackage) ->
    let goExprs = collectGoExpressions goPackage
        (issues, commons) = partitionEithers (map goExprToCommon goExprs)
    in (commons, issues)

collectPythonExpressions :: PythonModule -> [Located PythonExpr]
collectPythonExpressions pyModule =
  concatMap collectPythonStmt (pyModuleBody pyModule)

collectPythonStmt :: Located PythonStmt -> [Located PythonExpr]
collectPythonStmt (Located _ stmt) = case stmt of
  PyExprStmt expr -> [expr]
  PyAssign _ value -> [value]
  PyAugAssign _ _ value -> [value]
  PyAnnAssign _ _ maybeValue -> maybeToList maybeValue
  PyReturn maybeExpr -> maybeToList maybeExpr
  PyYield maybeExpr -> maybeToList maybeExpr
  PyYieldFrom expr -> [expr]
  PyDel exprs -> exprs
  PyAssert expr maybeMsg -> expr : maybeToList maybeMsg
  PyIf cond body orelse -> cond : collectNested body ++ collectNested orelse
  PyWhile cond body orelse -> cond : collectNested body ++ collectNested orelse
  PyFor _ iter body orelse -> iter : collectNested body ++ collectNested orelse
  PyAsyncFor _ iter body orelse -> iter : collectNested body ++ collectNested orelse
  PyWith items body -> concatMap collectPythonWithItem items ++ collectNested body
  PyAsyncWith items body -> concatMap collectPythonWithItem items ++ collectNested body
  PyTry body excepts orelse finally ->
    collectNested body ++ concatMap collectPythonExcept excepts ++ collectNested orelse ++ collectNested finally
  PyRaise maybeExc maybeFrom -> catMaybes [maybeExc, maybeFrom]
  PyFuncDef func -> collectPythonFunc func
  PyAsyncFuncDef func -> collectPythonFunc func
  PyClassDef cls -> collectPythonClass cls
  PyImport _ -> []
  PyGlobal _ -> []
  PyNonlocal _ -> []
  PyPass -> []
  PyBreak -> []
  PyContinue -> []
  where
    collectNested = concatMap collectPythonStmt

collectPythonWithItem :: Located PythonWithItem -> [Located PythonExpr]
collectPythonWithItem (Located _ item) = [pyWithContext item]

collectPythonExcept :: Located PythonExcept -> [Located PythonExpr]
collectPythonExcept (Located _ except) =
  maybeToList (pyExceptType except) ++ concatMap collectPythonStmt (pyExceptBody except)

collectPythonFunc :: PythonFuncDef -> [Located PythonExpr]
collectPythonFunc func =
  concat
    [ concatMap collectPythonDecorator (pyFuncDecorators func)
    , concatMap collectPythonParam (pyFuncParams func)
    , concatMap collectPythonStmt (pyFuncBody func)
    ]

collectPythonDecorator :: Located PythonDecorator -> [Located PythonExpr]
collectPythonDecorator (Located _ deco) =
  pyDecoratorName deco : concatMap collectPythonArgument (pyDecoratorArgs deco)

collectPythonArgument :: Located PythonArgument -> [Located PythonExpr]
collectPythonArgument (Located _ arg) = case arg of
  ArgPositional expr -> [expr]
  ArgKeyword _ expr -> [expr]
  ArgStarred expr -> [expr]
  ArgKwStarred expr -> [expr]

collectPythonParam :: Located PythonParameter -> [Located PythonExpr]
collectPythonParam (Located _ param) = case param of
  ParamNormal _ _ maybeDefault -> maybeToList maybeDefault
  ParamVarArgs _ _ -> []
  ParamKwArgs _ _ -> []
  ParamKwOnly _ _ maybeDefault -> maybeToList maybeDefault

collectPythonClass :: PythonClassDef -> [Located PythonExpr]
collectPythonClass cls =
  concat
    [ concatMap collectPythonDecorator (pyClassDecorators cls)
    , pyClassBases cls
    , map snd (pyClassKeywords cls)
    , concatMap collectPythonStmt (pyClassBody cls)
    ]

collectGoExpressions :: GoPackage -> [Located GoExpr]
collectGoExpressions goPackage =
  concatMap collectGoFile (goPackageFiles goPackage)

collectGoFile :: GoFile -> [Located GoExpr]
collectGoFile goFile = concatMap collectGoDecl (goFileDecls goFile)

collectGoDecl :: Located GoDecl -> [Located GoExpr]
collectGoDecl (Located _ decl) = case decl of
  GoImportDecl _ -> []
  GoConstDecl entries -> [expr | (_, _, expr) <- entries]
  GoTypeDecl _ _ -> []
  GoVarDecl entries -> catMaybes [expr | (_, _, expr) <- entries]
  GoFuncDecl func -> collectGoFunction func
  GoMethodDecl _ func -> collectGoFunction func

collectGoFunction :: GoFunction -> [Located GoExpr]
collectGoFunction func = collectGoStmtMaybe (goFuncBody func)

collectGoStmtMaybe :: Maybe (Located GoStmt) -> [Located GoExpr]
collectGoStmtMaybe = maybe [] collectGoStmt

collectGoStmt :: Located GoStmt -> [Located GoExpr]
collectGoStmt (Located _ stmt) = case stmt of
  GoExprStmt expr -> [expr]
  GoAssign lhs rhs -> lhs ++ rhs
  GoDefine _ rhs -> rhs
  GoIncDec expr _ -> [expr]
  GoSend chanExpr valueExpr -> [chanExpr, valueExpr]
  GoReturn exprs -> exprs
  GoBreak _ -> []
  GoContinue _ -> []
  GoGoto _ -> []
  GoFallthrough -> []
  GoEmpty -> []
  GoBlock stmts -> concatMap collectGoStmt stmts
  GoIf initStmt cond thenStmt elseStmt ->
    collectGoStmtMaybe initStmt ++ [cond] ++ collectGoStmt thenStmt ++ collectGoStmtMaybe elseStmt
  GoSwitch initStmt maybeExpr cases ->
    collectGoStmtMaybe initStmt ++ maybeToList maybeExpr ++ concatMap collectGoStmt cases
  GoTypeSwitch initStmt clause cases ->
    collectGoStmtMaybe initStmt ++ collectGoTypeSwitchClause clause ++ concatMap collectGoStmt cases
  GoFor clause body -> collectGoForClause clause ++ collectGoStmt body
  GoRange clause body -> collectGoRangeClause clause ++ collectGoStmt body
  GoSelect clauses -> concatMap collectGoCommClause clauses
  GoDefer expr -> [expr]
  GoGo expr -> [expr]
  GoCase exprs stmts -> exprs ++ concatMap collectGoStmt stmts
  GoDefault stmts -> concatMap collectGoStmt stmts
  GoCommCase maybeStmt stmts -> collectGoStmtMaybe maybeStmt ++ concatMap collectGoStmt stmts
  GoCommDefault stmts -> concatMap collectGoStmt stmts
  GoLabeled _ inner -> collectGoStmt inner

collectGoTypeSwitchClause :: GoTypeSwitchClause -> [Located GoExpr]
collectGoTypeSwitchClause clause = [goTypeSwitchExpr clause]

collectGoForClause :: Maybe GoForClause -> [Located GoExpr]
collectGoForClause Nothing = []
collectGoForClause (Just clause) =
  collectGoStmtMaybe (goForInit clause) ++ maybeToList (goForCond clause) ++ collectGoStmtMaybe (goForPost clause)

collectGoRangeClause :: GoRangeClause -> [Located GoExpr]
collectGoRangeClause clause = [goRangeExpr clause]

collectGoCommClause :: Located GoCommClause -> [Located GoExpr]
collectGoCommClause (Located _ clause) =
  collectGoStmtMaybe (goCommStmt clause) ++ concatMap collectGoStmt (goCommBody clause)

pythonExprToCommon :: Located PythonExpr -> Either Text CommonExpr
pythonExprToCommon located@(Located span expr) = case expr of
  PyLiteral lit -> CELiteral <$> pythonLiteralToLiteral span lit
  PyVar ident -> Right $ CEVar ident
  PyConst qn -> Right $ CEVar (qnName qn)
  PyBinaryOp op left right -> do
    left' <- pythonExprToLocatedCommon left
    right' <- pythonExprToLocatedCommon right
    pure $ CEBinaryOp op left' right'
  PyUnaryOp op operand -> do
    operand' <- pythonExprToLocatedCommon operand
    pure $ CEUnaryOp op operand'
  PyComparison [op] (left:right:[]) -> do
    left' <- pythonExprToLocatedCommon left
    right' <- pythonExprToLocatedCommon right
    pure $ CEComparison op left' right'
  PyComparison _ _ -> Left $ "Unsupported chained comparison at " <> formatSpan span
  PyBoolOp op operands -> do
    locatedOperands <- traverse pythonExprToLocatedCommon operands
    case locatedOperands of
      [] -> Left $ "Empty boolean operation at " <> formatSpan span
      (firstOperand:restOperands) ->
        let combined = foldl'
              (\acc next -> Located (mergeSpans (locSpan acc) (locSpan next)) (CEBinaryOp op acc next))
              firstOperand
              restOperands
        in pure $ locValue combined
  PySubscript value sliceNode -> do
    value' <- pythonExprToLocatedCommon value
    case sliceNode of
      SliceIndex idx -> do
        idx' <- pythonExprToLocatedCommon idx
        pure $ CEIndex value' idx'
      SliceSlice start end step -> case step of
        Just _ -> Left $ "Slice step is not supported in common expression lowering at " <> formatSpan span
        Nothing -> do
          start' <- traverse pythonExprToLocatedCommon start
          end' <- traverse pythonExprToLocatedCommon end
          pure $ CESlice value' start' end'
      SliceExtSlice _ -> Left $ "Extended slicing is not supported at " <> formatSpan span
  PyCall func args -> do
    func' <- pythonExprToLocatedCommon func
    args' <- traverse pythonArgumentToCommon args
    pure $ CECall func' args'
  PyAttribute obj attr -> do
    obj' <- pythonExprToLocatedCommon obj
    pure $ CEAttribute obj' attr
  PySlice _ _ _ -> Left $ "Standalone slice expressions are not supported at " <> formatSpan span
  PyList _ -> Left $ "List literals are not supported at " <> formatSpan span
  PyTuple _ -> Left $ "Tuple literals are not supported at " <> formatSpan span
  PySet _ -> Left $ "Set literals are not supported at " <> formatSpan span
  PyDict _ -> Left $ "Dictionary literals are not supported at " <> formatSpan span
  PyListComp _ _ -> Left $ "List comprehensions are not supported at " <> formatSpan span
  PySetComp _ _ -> Left $ "Set comprehensions are not supported at " <> formatSpan span
  PyDictComp _ _ _ -> Left $ "Dict comprehensions are not supported at " <> formatSpan span
  PyGenComp _ _ -> Left $ "Generator expressions are not supported at " <> formatSpan span
  PyLambda _ _ -> Left $ "Lambda expressions are not supported at " <> formatSpan span
  PyIfExp{} -> Left $ "Conditional expressions are not supported at " <> formatSpan span
  PyStarred{} -> Left $ "Starred expressions are not supported at " <> formatSpan span
  PyNamedExpr{} -> Left $ "Walrus operator expressions are not supported at " <> formatSpan span
  PyAwait{} -> Left $ "Await expressions are not supported at " <> formatSpan span
  PyAsyncCall{} -> Left $ "Async call expressions are not supported at " <> formatSpan span
  PyJoinedStr{} -> Left $ "Formatted string expressions are not supported at " <> formatSpan span
  PyFormatSpec{} -> Left $ "Format specifier expressions are not supported at " <> formatSpan span

pythonExprToLocatedCommon :: Located PythonExpr -> Either Text (Located CommonExpr)
pythonExprToLocatedCommon located@(Located span _) = do
  converted <- pythonExprToCommon located
  pure $ Located span converted

pythonLiteralToLiteral :: SourceSpan -> PythonLiteral -> Either Text Literal
pythonLiteralToLiteral span = \case
  PyInt n -> Right $ LInt (fromIntegral n :: Int64)
  PyFloat f -> Right $ LFloat f
  PyString s -> Right $ LString s
  PyBytes b -> Right $ LBytes b
  PyBool b -> Right $ LBool b
  PyNone -> Right LNone
  PyEllipsis -> Left $ "Ellipsis literal is not supported at " <> formatSpan span
  PyComplex _ _ -> Left $ "Complex literals are not supported at " <> formatSpan span

pythonArgumentToCommon :: Located PythonArgument -> Either Text (Located CommonExpr)
pythonArgumentToCommon argLocated@(Located span arg) = case arg of
  ArgPositional expr -> pythonExprToLocatedCommon expr
  ArgKeyword _ expr -> pythonExprToLocatedCommon expr
  ArgStarred _ -> Left $ "Starred positional arguments are not supported at " <> formatSpan span
  ArgKwStarred _ -> Left $ "Starred keyword arguments are not supported at " <> formatSpan span

goExprToCommon :: Located GoExpr -> Either Text CommonExpr
goExprToCommon located@(Located span expr) = case expr of
  GoLiteral lit -> CELiteral <$> goLiteralToLiteral span lit
  GoIdent ident -> Right $ CEVar ident
  GoQualifiedIdent pkg ident ->
    let pkgVar = Located span (CEVar pkg)
    in Right $ CEAttribute pkgVar ident
  GoBinaryOp op left right -> do
    left' <- goExprToLocatedCommon left
    right' <- goExprToLocatedCommon right
    pure $ CEBinaryOp op left' right'
  GoUnaryOp op operand -> do
    operand' <- goExprToLocatedCommon operand
    pure $ CEUnaryOp op operand'
  GoComparison op left right -> do
    left' <- goExprToLocatedCommon left
    right' <- goExprToLocatedCommon right
    pure $ CEComparison op left' right'
  GoCall func args -> do
    func' <- goExprToLocatedCommon func
    args' <- traverse goExprToLocatedCommon args
    pure $ CECall func' args'
  GoIndex container indexExpr -> do
    container' <- goExprToLocatedCommon container
    index' <- goExprToLocatedCommon indexExpr
    pure $ CEIndex container' index'
  GoSlice container sliceExpr -> do
    container' <- goExprToLocatedCommon container
    case goSliceMax sliceExpr of
      Just _ -> Left $ "Three-index slices are not supported at " <> formatSpan span
      Nothing -> do
        low' <- traverse goExprToLocatedCommon (goSliceLow sliceExpr)
        high' <- traverse goExprToLocatedCommon (goSliceHigh sliceExpr)
        pure $ CESlice container' low' high'
  GoSelector obj ident -> do
    obj' <- goExprToLocatedCommon obj
    pure $ CEAttribute obj' ident
  GoTypeConversion _ _ -> Left $ "Type conversions are not supported at " <> formatSpan span
  GoCompositeLit{} -> Left $ "Composite literals are not supported at " <> formatSpan span
  GoArrayLit{} -> Left $ "Array literals are not supported at " <> formatSpan span
  GoSliceLit{} -> Left $ "Slice literals are not supported at " <> formatSpan span
  GoMapLit{} -> Left $ "Map literals are not supported at " <> formatSpan span
  GoStructLit{} -> Left $ "Struct literals are not supported at " <> formatSpan span
  GoAddress{} -> Left $ "Address-of expressions are not supported at " <> formatSpan span
  GoDeref{} -> Left $ "Pointer dereference expressions are not supported at " <> formatSpan span
  GoReceive{} -> Left $ "Channel receive expressions are not supported at " <> formatSpan span
  GoTypeAssert{} -> Left $ "Type assertions are not supported at " <> formatSpan span
  GoFuncLit{} -> Left $ "Function literals are not supported at " <> formatSpan span

goExprToLocatedCommon :: Located GoExpr -> Either Text (Located CommonExpr)
goExprToLocatedCommon located@(Located span _) = do
  converted <- goExprToCommon located
  pure $ Located span converted

goLiteralToLiteral :: SourceSpan -> GoLiteral -> Either Text Literal
goLiteralToLiteral span = \case
  GoInt n -> Right $ LInt (fromIntegral n :: Int64)
  GoFloat f -> Right $ LFloat f
  GoImag _ -> Left $ "Imaginary literals are not supported at " <> formatSpan span
  GoRune c -> Right $ LChar c
  GoString s -> Right $ LString s
  GoRawString s -> Right $ LString s
  GoBool b -> Right $ LBool b
  GoNil -> Right LNone

mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans (SourceSpan file start _) (SourceSpan _ _ end) = SourceSpan file start end

formatSpan :: SourceSpan -> Text
formatSpan (SourceSpan file start _) =
  file <> ":" <> textShow (posLine start) <> ":" <> textShow (posColumn start)

systemSpan :: SourceSpan
systemSpan = SourceSpan (T.pack "<system>") (SourcePos 0 0) (SourcePos 0 0)

textShow :: Show a => a -> Text
textShow = T.pack . show

renderType :: Type -> Text
renderType = textShow

renderCommonExpr :: CommonExpr -> Text
renderCommonExpr = textShow

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

recordOptimizationStat :: Text -> CompilerM ()
recordOptimizationStat key = recordOptimizationStatN key 1

recordOptimizationStatN :: Text -> Int -> CompilerM ()
recordOptimizationStatN key delta =
  modify $ \s ->
    let updatedStats = HM.insertWith (+) key delta (csOptimizationStats s)
    in s { csOptimizationStats = updatedStats }

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

-- | Render C++ unit to text
flattenStmt :: CppStmt -> [CppStmt]
flattenStmt = \case
  CppStmtSeq stmts -> flattenStmts stmts
  stmt -> [stmt]

flattenStmts :: [CppStmt] -> [CppStmt]
flattenStmts = concatMap flattenStmt

renderCppUnit :: CppUnit -> Text
renderCppUnit (CppUnit includes _ decls) = 
  T.unlines $ 
    [ "// Generated by HyperStatic/CXX Compiler" ] ++
    map (\inc -> "#include " <> inc) includes ++
    [ "" ] ++
    map renderCppDecl decls

-- | Render a C++ declaration
renderCppDecl :: CppDecl -> Text
renderCppDecl = \case
  CppFunction name retType params body -> 
    let renderedBody = map renderCppStmt (flattenStmts body)
    in renderCppType retType <> " " <> name <> "(" <> 
       T.intercalate ", " (map renderCppParam params) <> ") {\n" <>
       T.unlines (map ("    " <>) renderedBody) <>
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
    renderClassLike "class" className baseClasses members
  CppStruct structName members ->
    renderClassLike "struct" structName [] members
  CppMethod name retType params body isVirtual ->
    let renderedBody = map renderCppStmt (flattenStmts body)
    in (if isVirtual then "virtual " else "") <>
       renderCppType retType <> " " <> name <> "(" <> 
       T.intercalate ", " (map renderCppParam params) <> ") {\n" <>
       T.unlines (map ("        " <>) renderedBody) <>
       "    }\n"
  CppConstructor className params body ->
    let renderedBody = map renderCppStmt (flattenStmts body)
    in className <> "(" <> 
       T.intercalate ", " (map renderCppParam params) <> ") {\n" <>
       T.unlines (map ("        " <>) renderedBody) <>
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
  CppAccessSpec spec ->
    spec <> ":"
  CppCommentDecl comment ->
    "// " <> comment
  _ -> "// TODO: Render other declaration types\n"
  where
    renderClassLike :: Text -> Text -> [Text] -> [CppDecl] -> Text
    renderClassLike keyword name baseClasses members =
      let header = keyword <> " " <> name <>
            (if null baseClasses then "" else " : " <> T.intercalate ", " baseClasses) <> " {\n"
          body = renderMembers Nothing members
      in header <> body <> "};\n"
      where
        renderMembers :: Maybe Text -> [CppDecl] -> Text
        renderMembers _ [] = ""
        renderMembers _ (CppAccessSpec spec : rest) =
          spec <> ":\n" <> renderMembers (Just spec) rest
        renderMembers currentAccess (decl : rest) =
          let (prefix, nextAccess) = case currentAccess of
                Nothing -> ("public:\n", Just "public")
                access -> ("", access)
              declText = ensureTrailingNewline (renderCppDecl decl)
              indented = indentDecl declText
          in prefix <> indented <> renderMembers nextAccess rest

        ensureTrailingNewline :: Text -> Text
        ensureTrailingNewline txt
          | T.null txt = txt
          | T.isSuffixOf "\n" txt = txt
          | otherwise = txt <> "\n"
    indentDecl :: Text -> Text
    indentDecl txt =
      let ls = T.lines txt
      in if null ls then "" else T.unlines (map ("    " <>) ls)

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

-- | Render C++ parameter
renderCppParam :: CppParam -> Text
renderCppParam (CppParam name paramType _) = 
  renderCppType paramType <> " " <> name

-- | Render C++ statement
renderCppStmt :: CppStmt -> Text
renderCppStmt = \case
  CppStmtSeq stmts -> T.unlines (map renderCppStmt (flattenStmts stmts))
  CppReturn Nothing -> "return;"
  CppReturn (Just expr) -> "return " <> renderCppExpr expr <> ";"
  CppExprStmt expr -> renderCppExpr expr <> ";"
  CppIf cond thenStmts elseStmts ->
    let renderedThen = map renderCppStmt (flattenStmts thenStmts)
        renderedElse = map renderCppStmt (flattenStmts elseStmts)
    in "if (" <> renderCppExpr cond <> ") {\n" <>
       T.unlines (map ("    " <>) renderedThen) <>
       "}" <> (if null renderedElse then "" else " else {\n" <>
       T.unlines (map ("    " <>) renderedElse) <>
       "}")
  CppWhile cond body ->
    let renderedBody = map renderCppStmt (flattenStmts body)
    in "while (" <> renderCppExpr cond <> ") {\n" <>
       T.unlines (map ("    " <>) renderedBody) <>
       "}"
  CppFor init cond incr body ->
    let initPart = case init of
          Nothing   -> "; "
          Just stmt -> renderCppStmt stmt <> " "
        renderedBody = map renderCppStmt (flattenStmts body)
    in "for (" <>
       initPart <>
       (maybe "" renderCppExpr cond) <> "; " <>
       (maybe "" renderCppExpr incr) <> ") {\n" <>
       T.unlines (map ("    " <>) renderedBody) <>
       "}"
  CppBlock stmts ->
    let renderedBody = map renderCppStmt (flattenStmts stmts)
    in "{\n" <>
       T.unlines (map ("    " <>) renderedBody) <>
       "}"
  CppComment comment -> "// " <> comment
  CppDecl decl -> T.stripEnd (renderCppDecl decl)  -- Remove trailing newline for inline declarations
  CppSwitch expr cases ->
    "switch (" <> renderCppExpr expr <> ") {\n" <>
    T.unlines (map renderCppCase cases) <>
    "}"
    where
      renderCppCase (CppCase caseExpr stmts) = 
        let renderedBody = map renderCppStmt (flattenStmts stmts)
        in "case " <> renderCppExpr caseExpr <> ":\n" <>
           T.unlines (map ("    " <>) renderedBody) <>
           "    break;"
      renderCppCase (CppDefault stmts) =
        let renderedBody = map renderCppStmt (flattenStmts stmts)
        in "default:\n" <>
           T.unlines (map ("    " <>) renderedBody)
  _ -> "// TODO: Render other statement types"

-- | Render C++ expression  
renderCppExpr :: CppExpr -> Text
renderCppExpr = \case
  CppVar name -> name
  CppLiteral lit -> renderCppLiteral lit
  CppBinary op left right -> 
    "(" <> renderCppExpr left <> " " <> op <> " " <> renderCppExpr right <> ")"
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
      hasThisInStmts stmts = any hasThisInStmt (flattenStmts stmts)
      
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
      hasThisInExpr (CppBracedInit _ exprs) = any hasThisInExpr exprs
      hasThisInExpr _ = False
  CppMove expr -> "std::move(" <> renderCppExpr expr <> ")"
  CppForward expr -> "std::forward(" <> renderCppExpr expr <> ")"
  CppMakeUnique cppType args -> "std::make_unique<" <> renderCppType cppType <> ">(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppMakeShared cppType args -> "std::make_shared<" <> renderCppType cppType <> ">(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppBracedInit cppType exprs ->
    renderCppType cppType <> "{" <> T.intercalate ", " (map renderCppExpr exprs) <> "}"
  CppThis -> "this"

-- | Render C++ literal
renderCppLiteral :: CppLiteral -> Text
renderCppLiteral = \case
  CppIntLit i -> T.pack $ show i
  CppFloatLit f -> T.pack $ show f
  CppBoolLit True -> "true"
  CppBoolLit False -> "false" 
  CppStringLit s -> "\"" <> s <> "\""
  CppCharLit c -> T.pack $ show c
  CppNullPtr -> "nullptr"