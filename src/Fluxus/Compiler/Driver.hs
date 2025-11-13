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
  , resolveWorkPath
  ) where

import Data.List (intercalate, foldl', partition, isPrefixOf)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad (when, unless, forM_, foldM)
import Control.Exception (IOException, try)
import Data.Maybe (fromMaybe, maybeToList, catMaybes)
import Data.Either (partitionEithers)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Time
import System.FilePath
import System.Directory
import System.Process
import System.Exit
import Data.Hashable (Hashable, hash)
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
  , EscapeResult(..)
  , runEscapeAnalysis
  , analyzeEscape
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
import Fluxus.Analysis.CommonExprLowering
  ( collectCommonExpressions
  , LoweringIssue(..)
  , renderLoweringIssue
  , isUnsupportedIssue
  , renderCommonExpr
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
  , CppCodeGenResult(..), CppCodeGenFailure(..)
  , generateCppWithAnnotations
  )
import Fluxus.CodeGen.CPP.AST (renderCppUnit)
import Fluxus.CodeGen.CPP.Diagnostics
  ( DiagnosticSeverity(..)
  , CppDiagnostic(..)
  , renderCppCodeGenError
  )
import Fluxus.Utils.Pretty hiding ((</>))

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
  , ccEnableExperimentalOptimizations :: !Bool -- Enable unfinished optimization passes
  , ccStopAtCodegen     :: !Bool            -- Stop after generating C++ source
  , ccSkipCompilerCheck :: !Bool            -- Skip verifying the C++ compiler during setup
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
    , "enable_experimental_optimizations" .= ccEnableExperimentalOptimizations config
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
    , "stop_at_codegen" .= ccStopAtCodegen config
    , "skip_compiler_check" .= ccSkipCompilerCheck config
    ]


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
  , csAnalysisAnnotations :: !AnalysisAnnotations
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
  , ccEnableExperimentalOptimizations = False
  , ccStopAtCodegen = False
  , ccSkipCompilerCheck = False
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
  , csAnalysisAnnotations = emptyAnnotations
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
  
  let skipCheck = ccSkipCompilerCheck config
      stopAtCodegen = ccStopAtCodegen config

  case () of
    _ | skipCheck ->
          logInfo "Skipping C++ compiler availability check (ccSkipCompilerCheck enabled)"
      | stopAtCodegen ->
          logInfo "Skipping C++ compiler availability check because stop-at-codegen is enabled"
      | otherwise -> do
          detectedPath <- liftIO $ do
            let compilerBinary = T.unpack (ccCppCompiler config)
            directExists <- doesFileExist compilerBinary
            if directExists
              then pure (Just compilerBinary)
              else findExecutable compilerBinary
          case detectedPath of
            Nothing ->
              throwError $ ConfigurationError $ "C++ compiler not found: " <> ccCppCompiler config <> " (enable ccSkipCompilerCheck to bypass detection)"
            Just path ->
              logVerbose $ "Detected C++ compiler at " <> T.pack path

  logInfo "Compiler environment setup completed"

-- | Compilation artifacts produced for a single file prior to linking
data CompilationArtifacts = CompilationArtifacts
  { caCppFile :: !FilePath
  , caObjectFile :: !(Maybe FilePath)
  }

-- | Resolve a candidate path into the configured work directory if one is set.
resolveWorkPath :: CompilerConfig -> FilePath -> FilePath
resolveWorkPath config candidate =
  case ccWorkDirectory config of
    Nothing -> candidate
    Just workDir ->
      let normalizedWork = normalise workDir
          normalizedCandidate = normalise candidate
          withinWork =
            let prefix = addTrailingPathSeparator normalizedWork
                target = addTrailingPathSeparator normalizedCandidate
            in prefix `isPrefixOf` target
          relativeCandidate = candidateRelative normalizedCandidate
          sanitizedRelative = sanitizeRelativePath relativeCandidate
          resolvedRelative =
            if null sanitizedRelative
              then hashedFallback normalizedCandidate
              else sanitizedRelative
      in if withinWork
           then normalizedCandidate
           else normalise (normalizedWork </> resolvedRelative)
  where
    candidateRelative path
      | isRelative path = path
      | otherwise =
          let noDrive = dropDrive path
          in dropWhile isPathSeparator noDrive
    sanitizeRelativePath path =
      let components = filter (not . null) (splitDirectories path)
          sanitizedComponents = map sanitizeComponent components
      in joinPath sanitizedComponents
    sanitizeComponent ".." = "__parent__"
    sanitizeComponent "." = "__current__"
    sanitizeComponent component = component
    hashedFallback path =
      let digest = show (abs (hash path))
          baseName = takeFileName path
          leafName =
            if null baseName
              then digest <> ".artifact"
              else baseName
      in joinPath [digest, leafName]

-- | Compute the intermediate file path for a given source using the work directory.
makeIntermediatePath :: CompilerConfig -> FilePath -> String -> FilePath
makeIntermediatePath config source newExt =
  resolveWorkPath config (replaceExtension source newExt)

-- | Resolve a default-named artifact (like the executable) into the work directory.
defaultOutputLocation :: CompilerConfig -> FilePath -> FilePath
defaultOutputLocation config name =
  case ccWorkDirectory config of
    Nothing -> name
    Just workDir -> workDir </> name

compileFileArtifacts :: FilePath -> CompilerM CompilationArtifacts
compileFileArtifacts inputFile = do
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
  let cppFile = makeIntermediatePath config inputFile ".cpp"
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory cppFile)
    TIO.writeFile cppFile (renderCppUnit cppCode)
  addIntermediateFile cppFile
  
  if ccStopAtCodegen config
    then do
      logInfo $ "Code generation completed: " <> T.pack cppFile
      incrementProcessedFiles
      pure $ CompilationArtifacts cppFile Nothing
    else do
      setCurrentPhase "c++-compilation"
      objFile <- compileCpp cppFile
      addIntermediateFile objFile
      logInfo $ "Generated object file: " <> T.pack objFile
      incrementProcessedFiles
      pure $ CompilationArtifacts cppFile (Just objFile)

-- | Compile a single file
compileFile :: FilePath -> CompilerM FilePath
compileFile inputFile = do
  config <- ask
  artifacts <- compileFileArtifacts inputFile
  
  case caObjectFile artifacts of
    Nothing -> return (caCppFile artifacts)
    Just objFile -> do
      setCurrentPhase "linking"
      finalOutput <- case ccOutputPath config of
        Nothing ->
          let executableName = dropExtension (takeFileName inputFile)
              outputPath = defaultOutputLocation config executableName
          in linkObjects [objFile] outputPath
        Just outPath -> linkObjects [objFile] outPath
      
      logInfo $ "Successfully compiled: " <> T.pack inputFile
      cleanupIntermediateFiles
      return finalOutput

-- | Compile a project (multiple files)
compileProject :: [FilePath] -> CompilerM FilePath
compileProject inputFiles = do
  config <- ask
  
  -- Set total file count
  modify $ \s -> s { csTotalFiles = length inputFiles }
  
  logInfo $ "Compiling project with " <> T.pack (show $ length inputFiles) <> " files"
  
  -- Compile all files to object files
  artifacts <- mapM compileFileArtifacts inputFiles
  
  if ccStopAtCodegen config
    then do
      logInfo "Code generation completed for all files"
      let defaultLocation = fromMaybe "." (ccWorkDirectory config)
          outputPath = fromMaybe defaultLocation (ccOutputPath config)
      return outputPath
    else do
      let objFiles = catMaybes (map caObjectFile artifacts)
      when (length objFiles /= length inputFiles) $
        throwError $ CodeGenError $ T.pack "Object file generation was skipped for one or more inputs; cannot link project without object files"
      
      let defaultOutput = defaultOutputLocation config "fluxus_output"
          outputPath = fromMaybe defaultOutput (ccOutputPath config)
      setCurrentPhase "final-linking"
      finalBinary <- linkObjects objFiles outputPath
      
      -- Cleanup intermediate files if requested
      cleanupIntermediateFiles
      
      logInfo $ "Project compilation completed: " <> T.pack finalBinary
      return finalBinary

-- | Parse input file based on source language (detected from file extension)
parseStage :: FilePath -> CompilerM (Either PythonAST GoAST)
parseStage inputFile = do
  config <- ask
  contentResult <- liftIO $ (try (TIO.readFile inputFile) :: IO (Either IOException Text))
  content <- case contentResult of
    Left ioErr ->
      throwError $ FileSystemError (textShow ioErr) inputFile
    Right fileContent ->
      return fileContent

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
      (unsupportedIssues, failureIssues) = partition isUnsupportedIssue extractionIssues
  forM_ failureIssues $ \issue ->
    addWarning $ TypeWarning (renderLoweringIssue issue) systemSpan
  unless (null unsupportedIssues) $ do
    let preview = take 3 unsupportedIssues
        summary = "Static analysis skipped " <> textShow (length unsupportedIssues) <> " expressions (unsupported lowering cases)"
        detail = T.intercalate "; " (map renderLoweringIssue preview)
        suffix = if length unsupportedIssues > length preview
          then " (+ " <> textShow (length unsupportedIssues - length preview) <> " more)"
          else ""
    logVerbose $ summary <> ": " <> detail <> suffix
  when (null commonExprs) $
    if null failureIssues
      then logInfo "No analyzable expressions found for type inference (encountered constructs are currently unsupported)"
      else addWarning $ TypeWarning "No analyzable expressions found for type inference due to lowering failures" systemSpan
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
          let annotation = ExprAnnotations
                { eaInferredType = Just inferredType
                , eaOwnership = Nothing
                , eaEscapeInfo = Nothing
                , eaOptimizationNotes = []
                }
          modify $ \s -> s { csAnalysisAnnotations = insertAnnotations exprKey annotation (csAnalysisAnnotations s) }
          recordOptimizationStat "type-inference.success"
          logVerbose $ "Inferred type for " <> exprKey <> ": " <> renderType inferredType
          pure (okCount + 1, errCount)

-- | Optimization stage
optimizationStage :: Either PythonAST GoAST -> CompilerM (Either PythonAST GoAST)
optimizationStage ast = do
  config <- ask
  let experimentalEnabled = ccEnableExperimentalOptimizations config
  logInfo $ "Running optimizations at level " <> T.pack (show $ ccOptimizationLevel config)
  unless experimentalEnabled $
    logVerbose "Experimental optimization passes are disabled by default. Enable them with --enable-experimental-optimizations to exercise monomorphization and devirtualization."
  let (commonExprs, extractionIssues) = collectCommonExpressions ast
      (unsupportedIssues, failureIssues) = partition isUnsupportedIssue extractionIssues
  forM_ failureIssues $ \issue ->
    addWarning $ OptimizationWarning (renderLoweringIssue issue)
  unless (null unsupportedIssues) $ do
    let preview = take 3 unsupportedIssues
        summary = "Skipping " <> textShow (length unsupportedIssues) <> " expressions during optimization (unsupported lowering cases)"
        detail = T.intercalate "; " (map renderLoweringIssue preview)
        suffix = if length unsupportedIssues > length preview
          then " (+ " <> textShow (length unsupportedIssues - length preview) <> " more)"
          else ""
    logVerbose $ summary <> ": " <> detail <> suffix
  when (null commonExprs) $
    if null failureIssues
      then logInfo "No analyzable expressions found for optimization pipeline (encountered constructs are currently unsupported)"
      else addWarning $ OptimizationWarning "No analyzable expressions found for optimization pipeline due to lowering failures"
  forM_ commonExprs $ \expr -> do
    let exprLabel = renderCommonExpr expr
    recordOptimizationStat "optimization.expressions"
    let ((escapeOptimized, escapeHints), escapeState) = runEscapeAnalysis (optimizeMemoryAllocation expr)
        (escapeResult, _) = runEscapeAnalysis (analyzeEscape expr)
        escapeInfo = erEscapeInfo escapeResult
        memLoc = erMemoryLocation escapeResult
        baseAnnotation = ExprAnnotations
          { eaInferredType = Nothing
          , eaOwnership = Nothing
          , eaEscapeInfo = Just escapeInfo
          , eaOptimizationNotes = escapeHints
          }
    modify $ \s -> s { csAnalysisAnnotations = insertAnnotations exprLabel baseAnnotation (csAnalysisAnnotations s) }
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
        let strategy = orStrategy ownershipResult
            strategyTag = case strategy of
              StackOwned -> "stack"
              UniqueOwnership -> "unique"
              SharedOwnership -> "shared"
              BorrowedReference -> "borrowed"
              MoveSemantics -> "move"
              CopySemantics -> "copy"
              WeakReference -> "weak"
              CustomRAII -> "custom-raii"
            ownershipInfo = OwnershipInfo
              { ownsMemory = strategy `elem` [StackOwned, UniqueOwnership]
              , canMove = strategy `elem` [MoveSemantics, UniqueOwnership]
              , refCount = Nothing
              , escapes = escapeInfo
              , memLocation = memLoc
              }
            ownershipAnnotation = ExprAnnotations
              { eaInferredType = Nothing
              , eaOwnership = Just ownershipInfo
              , eaEscapeInfo = Just escapeInfo
              , eaOptimizationNotes = oisOptimizationHints ownershipState
              }
        modify $ \s -> s { csAnalysisAnnotations = insertAnnotations exprLabel ownershipAnnotation (csAnalysisAnnotations s) }
        recordOptimizationStat ("optimization.ownership." <> strategyTag)
        forM_ (oisOptimizationHints ownershipState) $ \hint ->
          addWarning $ OptimizationWarning ("Ownership hint for " <> exprLabel <> ": " <> hint)
    case runShapeAnalysis (analyzeShape escapeOptimized) of
      Left err -> addWarning $ OptimizationWarning ("Shape analysis failed for " <> exprLabel <> ": " <> err)
      Right (_shapeInfo, shapeState) -> do
        recordOptimizationStat "optimization.shape"
        let shapeHints = sasOptimizations shapeState
            shapeAnnotation = ExprAnnotations
              { eaInferredType = Nothing
              , eaOwnership = Nothing
              , eaEscapeInfo = Nothing
              , eaOptimizationNotes = shapeHints
              }
        modify $ \s -> s { csAnalysisAnnotations = insertAnnotations exprLabel shapeAnnotation (csAnalysisAnnotations s) }
        forM_ shapeHints $ \hint ->
          addWarning $ OptimizationWarning ("Shape analysis suggestion for " <> exprLabel <> ": " <> hint)
    let (fallbackRequired, _) = runSmartFallback (shouldFallbackToRuntime escapeOptimized)
    when fallbackRequired $ do
      recordOptimizationStat "optimization.fallback.runtime"
      let fallbackAnnotation = ExprAnnotations
            { eaInferredType = Nothing
            , eaOwnership = Nothing
            , eaEscapeInfo = Nothing
            , eaOptimizationNotes = ["Runtime fallback recommended"]
            }
      modify $ \s -> s { csAnalysisAnnotations = insertAnnotations exprLabel fallbackAnnotation (csAnalysisAnnotations s) }
      addWarning $ OptimizationWarning ("Runtime fallback recommended for " <> exprLabel)
    let (fallbackExpr, _) = runSmartFallback (optimizeWithFallback escapeOptimized)
    if experimentalEnabled
      then do
        let (monoResult, monoState) = runMonomorphization (monomorphize fallbackExpr)
        recordOptimizationStat "optimization.monomorphization"
        when (not (null (mrOptimizations monoResult))) $ do
          let monoHints = mrOptimizations monoResult
              monoAnnotation = ExprAnnotations
                { eaInferredType = Nothing
                , eaOwnership = Nothing
                , eaEscapeInfo = Nothing
                , eaOptimizationNotes = monoHints
                }
          modify $ \s -> s { csAnalysisAnnotations = insertAnnotations exprLabel monoAnnotation (csAnalysisAnnotations s) }
          forM_ monoHints $ \msg ->
            addWarning $ OptimizationWarning ("Monomorphization note for " <> exprLabel <> ": " <> msg)
        recordOptimizationStatN "optimization.specializations" (HM.size (msSpecializations monoState))
        let (devirtResult, devirtState) = runDevirtualization (devirtualize (mrExpression monoResult))
        recordOptimizationStat "optimization.devirtualization"
        when (not (null (drOptimizations devirtResult))) $ do
          let devirtHints = drOptimizations devirtResult
              devirtAnnotation = ExprAnnotations
                { eaInferredType = Nothing
                , eaOwnership = Nothing
                , eaEscapeInfo = Nothing
                , eaOptimizationNotes = devirtHints
                }
          modify $ \s -> s { csAnalysisAnnotations = insertAnnotations exprLabel devirtAnnotation (csAnalysisAnnotations s) }
          forM_ devirtHints $ \msg ->
            addWarning $ OptimizationWarning ("Devirtualization note for " <> exprLabel <> ": " <> msg)
        let resolvedCount = HM.size (dsResolvedCalls devirtState)
        when (resolvedCount > 0) $
          recordOptimizationStatN "optimization.devirtualization.resolved" resolvedCount
        when (drExpression devirtResult /= expr) $
          recordOptimizationStat "optimization.expr.changed"
      else do
        recordOptimizationStat "optimization.experimental.skipped"
        logVerbose $ "Experimental optimization passes are disabled; skipping monomorphization and devirtualization for " <> exprLabel
  return ast

-- Common expression lowering utilities are provided by Fluxus.Analysis.CommonExprLowering.

systemSpan :: SourceSpan
systemSpan = SourceSpan (T.pack "<system>") (SourcePos 0 0) (SourcePos 0 0)

textShow :: Show a => a -> Text
textShow = T.pack . show

renderType :: Type -> Text
renderType = textShow

-- | Code generation stage
codeGenStage :: Either PythonAST GoAST -> CompilerM CppUnit
codeGenStage ast = do
  config <- ask
  annotations <- gets csAnalysisAnnotations
  
  logInfo $ "Code generation with " <> textShow (HM.size (unAnalysisAnnotations annotations)) <> " analysis annotations"

  let cppConfig = CppGenConfig
        { cgcOptimizationLevel = fromEnum $ ccOptimizationLevel config
        , cgcEnableInterop = ccEnableInterop config
        , cgcTargetCppStd = ccCppStandard config
        , cgcUseSmartPointers = ccOptimizationLevel config >= O2
        , cgcEnableParallel = ccEnableParallel config
        , cgcEnableCoroutines = ccCppStandard config >= "c++20"
        , cgcNamespace = "fluxus"
        , cgcHeaderGuard = "FLUXUS_GENERATED"
        , cgcStrictMode = ccStrictMode config
        }

  case generateCppWithAnnotations cppConfig annotations ast of
    Left failure -> do
      mapM_ logDiagnostic (cgfDiagnostics failure)
      let errText = case cgfErrors failure of
            [] -> "Code generation aborted due to strict diagnostics"
            errs -> T.intercalate "; " (map renderCppCodeGenError errs)
      throwError $ CodeGenError errText
    Right result -> do
      mapM_ logDiagnostic (cgrDiagnostics result)
      pure (cgrUnit result)
  where
    logDiagnostic :: CppDiagnostic -> CompilerM ()
    logDiagnostic diag =
      let baseMsg = diagMessage diag <> maybe "" (\ctx -> " (" <> ctx <> ")") (diagContext diag)
          prefixedInfo tag msg = tag <> " " <> msg
      in case diagSeverity diag of
        SeverityInfo -> logInfo $ prefixedInfo "[codegen]" baseMsg
        SeverityWarning -> logWarning $ prefixedInfo "[codegen-warning]" baseMsg
        SeverityError -> logWarning $ prefixedInfo "[codegen-error]" baseMsg



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
  
  liftIO $ createDirectoryIfMissing True (takeDirectory outputPath)
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

cleanupIntermediateFiles :: CompilerM ()
cleanupIntermediateFiles = do
  config <- ask
  unless (ccKeepIntermediates config) $ do
    intermediates <- gets csIntermediateFiles
    unless (null intermediates) $ do
      liftIO $ forM_ intermediates $ \path -> do
        exists <- doesFileExist path
        when exists $ removeFile path
      modify $ \s -> s { csIntermediateFiles = [] }
      logInfo "Cleaned up intermediate files"







