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
import Fluxus.Parser.Python.Lexer (runPythonLexer)
import Fluxus.Parser.Python.Parser (runPythonParser)
import Fluxus.Parser.Go.Lexer (runGoLexer)
import Fluxus.Parser.Go.Parser (runGoParser)
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