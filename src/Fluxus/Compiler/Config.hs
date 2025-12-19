{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- | Compiler configuration management
module Fluxus.Compiler.Config
  ( -- * Configuration loading
    loadConfig
  , loadConfigFromFile
  , ConfigFileError(..)
  , renderConfigFileError
    -- * Command line parsing
  , parseCommandLineArgs
  , LoadConfigResult(..)
  , CLICommand(..)
  , fluxusVersionString
    -- * Configuration merging
  , CompilerConfigOverrides(..)
  , emptyOverrides
  , mergeConfigs
  , applyEnvironmentOverrides
    -- * Configuration validation
  , validateConfigFile
  , checkSystemRequirements
    -- * Default configurations
  , developmentConfig
  , productionConfig
  , debugConfig
    -- * Utilities
  , configToArgs
  , printConfig
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import System.Environment (lookupEnv)
import System.Directory
import System.IO (hPutStrLn, stderr)
import Control.Monad (unless, when)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf, nub)
import GHC.Generics (Generic)

import Fluxus.Compiler.Driver

-- | Result of parsing command line arguments.
data CLICommand
  = CLICommandModify (CompilerConfig -> CompilerConfig) [FilePath]
  | CLICommandShowHelp
  | CLICommandShowVersion String

-- | Result of loading configuration, including informational CLI actions.
data LoadConfigResult
  = LoadConfigSuccess CompilerConfig
  | LoadConfigHelp
  | LoadConfigVersion String
  deriving (Eq, Show)

fluxusVersionString :: String
fluxusVersionString = "Fluxus Compiler v0.1.0"

-- | Errors that can occur while loading configuration files.
data ConfigFileError
  = ConfigFileNotFound FilePath
  | ConfigFileParseError FilePath String
  deriving (Eq, Show)

-- | Render a human-readable message for configuration file errors.
renderConfigFileError :: ConfigFileError -> String
renderConfigFileError = \case
  ConfigFileNotFound path -> "Configuration file not found: " ++ path
  ConfigFileParseError path msg -> "Failed to parse config file '" ++ path ++ "': " ++ msg

-- | Partial configuration overrides used when merging layered sources.
data CompilerConfigOverrides = CompilerConfigOverrides
  { ccoSourceLanguage    :: Maybe SourceLanguage
  , ccoOptimizationLevel :: Maybe OptimizationLevel
  , ccoTargetPlatform    :: Maybe TargetPlatform
  , ccoOutputPath        :: Maybe FilePath
  , ccoEnableInterop     :: Maybe Bool
  , ccoEnableDebugInfo   :: Maybe Bool
  , ccoEnableProfiler    :: Maybe Bool
  , ccoEnableParallel    :: Maybe Bool
  , ccoMaxConcurrency    :: Maybe Int
  , ccoIncludePaths      :: Maybe [FilePath]
  , ccoLibraryPaths      :: Maybe [FilePath]
  , ccoLinkedLibraries   :: Maybe [Text]
  , ccoCppStandard       :: Maybe Text
  , ccoCppCompiler       :: Maybe Text
  , ccoVerboseLevel      :: Maybe Int
  , ccoWorkDirectory     :: Maybe FilePath
  , ccoKeepIntermediates :: Maybe Bool
  , ccoStrictMode        :: Maybe Bool
  , ccoEnableAnalysis    :: Maybe Bool
  , ccoEnableExperimentalOptimizations :: Maybe Bool
  , ccoStopAtCodegen     :: Maybe Bool
  , ccoSkipCompilerCheck :: Maybe Bool
  } deriving (Eq, Show, Generic)

emptyOverrides :: CompilerConfigOverrides
emptyOverrides = CompilerConfigOverrides
  { ccoSourceLanguage = Nothing
  , ccoOptimizationLevel = Nothing
  , ccoTargetPlatform = Nothing
  , ccoOutputPath = Nothing
  , ccoEnableInterop = Nothing
  , ccoEnableDebugInfo = Nothing
  , ccoEnableProfiler = Nothing
  , ccoEnableParallel = Nothing
  , ccoMaxConcurrency = Nothing
  , ccoIncludePaths = Nothing
  , ccoLibraryPaths = Nothing
  , ccoLinkedLibraries = Nothing
  , ccoCppStandard = Nothing
  , ccoCppCompiler = Nothing
  , ccoVerboseLevel = Nothing
  , ccoWorkDirectory = Nothing
  , ccoKeepIntermediates = Nothing
  , ccoStrictMode = Nothing
  , ccoEnableAnalysis = Nothing
  , ccoEnableExperimentalOptimizations = Nothing
  , ccoStopAtCodegen = Nothing
  , ccoSkipCompilerCheck = Nothing
  }

instance FromJSON CompilerConfigOverrides where
  parseJSON = withObject "CompilerConfig" $ \o -> do
    rawSourceLanguage <- (o .:? "source_language" :: Parser (Maybe Text))
    rawOptimization <- (o .:? "optimization_level" :: Parser (Maybe Text))
    rawTarget <- (o .:? "target_platform" :: Parser (Maybe Text))
    outputPath <- o .:? "output_path"
    enableInterop <- o .:? "enable_interop"
    enableDebug <- o .:? "enable_debug_info"
    enableProfiler <- o .:? "enable_profiler"
    enableParallel <- o .:? "enable_parallel"
    maxConcurrency <- o .:? "max_concurrency"
    includePaths <- o .:? "include_paths"
    libraryPaths <- o .:? "library_paths"
    linkedLibraries <- o .:? "linked_libraries"
    cppStandard <- o .:? "cpp_standard"
    cppCompiler <- o .:? "cpp_compiler"
    verboseLevel <- o .:? "verbose_level"
    workDirectory <- o .:? "work_directory"
    keepIntermediates <- o .:? "keep_intermediates"
    strictMode <- o .:? "strict_mode"
    enableAnalysis <- o .:? "enable_analysis"
    enableExperimental <- o .:? "enable_experimental_optimizations"
    stopAtCodegen <- o .:? "stop_at_codegen"
    skipCompilerCheck <- o .:? "skip_compiler_check"

    let sourceLanguage = rawSourceLanguage >>= parseSourceLanguageValue
        optimizationLevel = rawOptimization >>= parseOptimizationValue
        targetPlatform = rawTarget >>= (parseTargetPlatform . T.unpack)
    pure CompilerConfigOverrides
      { ccoSourceLanguage = sourceLanguage
      , ccoOptimizationLevel = optimizationLevel
      , ccoTargetPlatform = targetPlatform
      , ccoOutputPath = outputPath
      , ccoEnableInterop = enableInterop
      , ccoEnableDebugInfo = enableDebug
      , ccoEnableProfiler = enableProfiler
      , ccoEnableParallel = enableParallel
      , ccoMaxConcurrency = maxConcurrency
      , ccoIncludePaths = includePaths
      , ccoLibraryPaths = libraryPaths
      , ccoLinkedLibraries = linkedLibraries
      , ccoCppStandard = cppStandard
      , ccoCppCompiler = cppCompiler
      , ccoVerboseLevel = verboseLevel
      , ccoWorkDirectory = workDirectory
      , ccoKeepIntermediates = keepIntermediates
      , ccoStrictMode = strictMode
      , ccoEnableAnalysis = enableAnalysis
      , ccoEnableExperimentalOptimizations = enableExperimental
      , ccoStopAtCodegen = stopAtCodegen
      , ccoSkipCompilerCheck = skipCompilerCheck
      }
    where
      parseSourceLanguageValue :: Text -> Maybe SourceLanguage
      parseSourceLanguageValue value = case T.unpack value of
        "Python" -> Just Python
        "Go" -> Just Go
        _ -> Nothing

      parseOptimizationValue :: Text -> Maybe OptimizationLevel
      parseOptimizationValue value = case T.unpack value of
        "O0" -> Just O0
        "O1" -> Just O1
        "O2" -> Just O2
        "O3" -> Just O3
        "Os" -> Just Os
        _ -> Nothing

-- | Load configuration from multiple sources with precedence:
-- 1. Command line arguments (highest)
-- 2. Environment variables
-- 3. Configuration file
-- 4. Defaults (lowest)
loadConfig :: [String] -> IO (Either String LoadConfigResult)
loadConfig args =
  case parseCommandLineArgs args of
    Left err -> pure $ Left err
    Right CLICommandShowHelp -> pure $ Right LoadConfigHelp
    Right (CLICommandShowVersion versionText) ->
      pure $ Right (LoadConfigVersion versionText)
    Right (CLICommandModify cliModifier configPaths) -> do
      let baseConfig = defaultConfig
          requestedFiles =
            if null configPaths
              then [("fluxus.yaml", False)]
              else map (\path -> (path, True)) configPaths
      configAfterFilesResult <- loadConfigFromSources baseConfig requestedFiles
      case configAfterFilesResult of
        Left err -> pure $ Left (renderConfigFileError err)
        Right configAfterFiles -> do
          configWithEnv <- applyEnvironmentOverrides configAfterFiles
          let finalConfig = applyPlatformDefaults (cliModifier configWithEnv)
          pure $ Right (LoadConfigSuccess finalConfig)
  where
    loadConfigFromSources :: CompilerConfig -> [(FilePath, Bool)] -> IO (Either ConfigFileError CompilerConfig)
    loadConfigFromSources config [] = pure $ Right config
    loadConfigFromSources config ((path, required):rest) = do
      result <- loadConfigFromFile path
      case result of
        Left err -> case err of
          ConfigFileNotFound _
            | not required -> loadConfigFromSources config rest
            | otherwise -> pure $ Left err
          _ -> pure $ Left err
        Right overrides ->
          let merged = mergeConfigs config overrides
          in loadConfigFromSources merged rest

-- | Load configuration from YAML file
loadConfigFromFile :: FilePath -> IO (Either ConfigFileError CompilerConfigOverrides)
loadConfigFromFile configFile = do
  exists <- doesFileExist configFile
  if not exists
    then pure $ Left (ConfigFileNotFound configFile)
    else do
      result <- decodeFileEither configFile
      case result of
        Left err ->
          let message = prettyPrintParseException err
          in pure $ Left (ConfigFileParseError configFile message)
        Right overrides -> pure $ Right overrides

-- | Parse command line arguments to compiler configuration modifiers or
-- informational CLI commands.
parseCommandLineArgs :: [String] -> Either String CLICommand
parseCommandLineArgs args = go id [] args
  where
    go modifier configs [] = Right (CLICommandModify modifier configs)
    go _ _ ("--help":_) = Right CLICommandShowHelp
    go _ _ ("--version":_) = Right (CLICommandShowVersion fluxusVersionString)
    go modifier configs (arg:rest) = case arg of
      "--config" -> case rest of
        (path:rest') -> go modifier (configs ++ [path]) rest'
        [] -> Left "Expected file path after --config"
      "-c" -> case rest of
        (path:rest') -> go modifier (configs ++ [path]) rest'
        [] -> Left "Expected file path after -c"
      "--python" -> set (\cfg -> cfg { ccSourceLanguage = Python }) configs rest
      "--go" -> set (\cfg -> cfg { ccSourceLanguage = Go }) configs rest
      
      "-O0" -> set (\cfg -> cfg { ccOptimizationLevel = O0 }) configs rest
      "-O1" -> set (\cfg -> cfg { ccOptimizationLevel = O1 }) configs rest
      "-O2" -> set (\cfg -> cfg { ccOptimizationLevel = O2 }) configs rest
      "-O3" -> set (\cfg -> cfg { ccOptimizationLevel = O3 }) configs rest
      "-Os" -> set (\cfg -> cfg { ccOptimizationLevel = Os }) configs rest
      
      "--enable-interop" -> set (\cfg -> cfg { ccEnableInterop = True }) configs rest
      "--disable-interop" -> set (\cfg -> cfg { ccEnableInterop = False }) configs rest
      
      "--enable-debug" -> set (\cfg -> cfg { ccEnableDebugInfo = True }) configs rest
      "--disable-debug" -> set (\cfg -> cfg { ccEnableDebugInfo = False }) configs rest
      
      "--enable-profiler" -> set (\cfg -> cfg { ccEnableProfiler = True }) configs rest
      "--disable-profiler" -> set (\cfg -> cfg { ccEnableProfiler = False }) configs rest
      
      "--enable-parallel" -> set (\cfg -> cfg { ccEnableParallel = True }) configs rest
      "--disable-parallel" -> set (\cfg -> cfg { ccEnableParallel = False }) configs rest
      
      "--strict" -> set (\cfg -> cfg { ccStrictMode = True }) configs rest
      "--no-strict" -> set (\cfg -> cfg { ccStrictMode = False }) configs rest
      
      "--enable-analysis" -> set (\cfg -> cfg { ccEnableAnalysis = True }) configs rest
      "--disable-analysis" -> set (\cfg -> cfg { ccEnableAnalysis = False }) configs rest
      
      "--enable-experimental-optimizations" -> set (\cfg -> cfg { ccEnableExperimentalOptimizations = True }) configs rest
      "--disable-experimental-optimizations" -> set (\cfg -> cfg { ccEnableExperimentalOptimizations = False }) configs rest
      
      "--stop-at-codegen" -> set (\cfg -> cfg { ccStopAtCodegen = True }) configs rest
      "--full-pipeline" -> set (\cfg -> cfg { ccStopAtCodegen = False }) configs rest
      
      "--keep-intermediates" -> set (\cfg -> cfg { ccKeepIntermediates = True }) configs rest
      "--clean-intermediates" -> set (\cfg -> cfg { ccKeepIntermediates = False }) configs rest
      
      "--skip-compiler-check" -> set (\cfg -> cfg { ccSkipCompilerCheck = True }) configs rest
      "--require-compiler-check" -> set (\cfg -> cfg { ccSkipCompilerCheck = False }) configs rest
      
      "-v" -> set (\cfg -> cfg { ccVerboseLevel = ccVerboseLevel cfg + 1 }) configs rest
      "--verbose" -> set (\cfg -> cfg { ccVerboseLevel = ccVerboseLevel cfg + 1 }) configs rest
      "--quiet" -> set (\cfg -> cfg { ccVerboseLevel = 0 }) configs rest
      
      "-o" -> case rest of
        (output:rest') -> set (\cfg -> cfg { ccOutputPath = Just output }) configs rest'
        [] -> Left "Expected output path after -o"
      
      "--output" -> case rest of
        (output:rest') -> set (\cfg -> cfg { ccOutputPath = Just output }) configs rest'
        [] -> Left "Expected output path after --output"
      
      "--work-dir" -> case rest of
        (dir:rest') -> set (\cfg -> cfg { ccWorkDirectory = Just dir }) configs rest'
        [] -> Left "Expected directory path after --work-dir"
      
      "--cpp-std" -> case rest of
        (std:rest') -> set (\cfg -> cfg { ccCppStandard = T.pack std }) configs rest'
        [] -> Left "Expected C++ standard after --cpp-std"
      
      "--cpp-compiler" -> case rest of
        (compiler:rest') -> set (\cfg -> cfg { ccCppCompiler = T.pack compiler }) configs rest'
        [] -> Left "Expected compiler path after --cpp-compiler"
      
      "--max-concurrency" -> case rest of
        (n:rest') -> case reads n of
          [(num, "")] -> set (\cfg -> cfg { ccMaxConcurrency = num }) configs rest'
          _ -> Left "Invalid number for --max-concurrency"
        [] -> Left "Expected number after --max-concurrency"
      
      "--include" -> case rest of
        (path:rest') -> set (\cfg -> cfg { ccIncludePaths = prependUnique path (ccIncludePaths cfg) }) configs rest'
        [] -> Left "Expected path after --include"
      
      "--library-path" -> case rest of
        (path:rest') -> set (\cfg -> cfg { ccLibraryPaths = prependUnique path (ccLibraryPaths cfg) }) configs rest'
        [] -> Left "Expected path after --library-path"
      
      "--link" -> case rest of
        (lib:rest') -> set (\cfg -> cfg { ccLinkedLibraries = prependUnique (T.pack lib) (ccLinkedLibraries cfg) }) configs rest'
        [] -> Left "Expected library name after --link"
      
      "--target" -> case rest of
        (target:rest') -> case parseTargetPlatform target of
          Just platform -> set (\cfg -> cfg { ccTargetPlatform = platform }) configs rest'
          Nothing -> Left $ "Unknown target platform: " ++ target
        [] -> Left "Expected target platform after --target"
      
      _ | "--" `isPrefixOf` arg ->
            Left $ "Unknown option: " ++ arg ++ ". Use --help to see available options."
      _ -> go modifier configs rest  -- Assume it's an input file
      where
        set modifyFn currentConfigs remaining = go (modifyFn . modifier) currentConfigs remaining
        prependUnique :: Eq a => a -> [a] -> [a]
        prependUnique value existing = value : filter (/= value) existing

-- | Parse target platform from string
parseTargetPlatform :: String -> Maybe TargetPlatform
parseTargetPlatform = \case
  "linux-x86_64" -> Just Linux_x86_64
  "linux-arm64" -> Just Linux_ARM64
  "darwin-x86_64" -> Just Darwin_x86_64
  "darwin-arm64" -> Just Darwin_ARM64
  "windows-x86_64" -> Just Windows_x86_64
  _ -> Nothing

-- | Merge two configurations, with the second taking precedence
mergeConfigs :: CompilerConfig -> CompilerConfigOverrides -> CompilerConfig
mergeConfigs base overrides = CompilerConfig
  { ccSourceLanguage = choose (ccoSourceLanguage overrides) (ccSourceLanguage base)
  , ccOptimizationLevel = choose (ccoOptimizationLevel overrides) (ccOptimizationLevel base)
  , ccTargetPlatform = choose (ccoTargetPlatform overrides) (ccTargetPlatform base)
  , ccOutputPath = chooseOptional (ccoOutputPath overrides) (ccOutputPath base)
  , ccEnableInterop = choose (ccoEnableInterop overrides) (ccEnableInterop base)
  , ccEnableDebugInfo = choose (ccoEnableDebugInfo overrides) (ccEnableDebugInfo base)
  , ccEnableProfiler = choose (ccoEnableProfiler overrides) (ccEnableProfiler base)
  , ccEnableParallel = choose (ccoEnableParallel overrides) (ccEnableParallel base)
  , ccMaxConcurrency = choose (ccoMaxConcurrency overrides) (ccMaxConcurrency base)
  , ccIncludePaths = mergeList (ccoIncludePaths overrides) (ccIncludePaths base)
  , ccLibraryPaths = mergeList (ccoLibraryPaths overrides) (ccLibraryPaths base)
  , ccLinkedLibraries = mergeList (ccoLinkedLibraries overrides) (ccLinkedLibraries base)
  , ccCppStandard = choose (ccoCppStandard overrides) (ccCppStandard base)
  , ccCppCompiler = choose (ccoCppCompiler overrides) (ccCppCompiler base)
  , ccVerboseLevel = choose (ccoVerboseLevel overrides) (ccVerboseLevel base)
  , ccWorkDirectory = chooseOptional (ccoWorkDirectory overrides) (ccWorkDirectory base)
  , ccKeepIntermediates = choose (ccoKeepIntermediates overrides) (ccKeepIntermediates base)
  , ccStrictMode = choose (ccoStrictMode overrides) (ccStrictMode base)
  , ccEnableAnalysis = choose (ccoEnableAnalysis overrides) (ccEnableAnalysis base)
  , ccEnableExperimentalOptimizations = choose (ccoEnableExperimentalOptimizations overrides) (ccEnableExperimentalOptimizations base)
  , ccStopAtCodegen = choose (ccoStopAtCodegen overrides) (ccStopAtCodegen base)
  , ccSkipCompilerCheck = choose (ccoSkipCompilerCheck overrides) (ccSkipCompilerCheck base)
  }
  where
    choose :: Maybe a -> a -> a
    choose maybeValue baseValue = fromMaybe baseValue maybeValue

    chooseOptional :: Maybe a -> Maybe a -> Maybe a
    chooseOptional maybeValue baseValue =
      case maybeValue of
        Just value -> Just value
        Nothing -> baseValue

    mergeList :: Eq a => Maybe [a] -> [a] -> [a]
    mergeList maybeOverride baseList =
      dedupPreservingOrder $ maybe baseList id maybeOverride

    dedupPreservingOrder :: Eq a => [a] -> [a]
    dedupPreservingOrder = nub

-- | Apply environment variable overrides
applyEnvironmentOverrides :: CompilerConfig -> IO CompilerConfig
applyEnvironmentOverrides config = do
  -- Check for common environment variables
  cppCompiler <- lookupEnv "CXX"
  cppStd <- lookupEnv "FLUXUS_CPP_STD"
  verboseLevel <- lookupEnv "FLUXUS_VERBOSE"
  enableInterop <- lookupEnv "FLUXUS_INTEROP"
  experimentalOptimizations <- lookupEnv "FLUXUS_EXPERIMENTAL_OPTIMIZATIONS"
  skipCompilerCheck <- lookupEnv "FLUXUS_SKIP_COMPILER_CHECK"

  let sanitizeEnvText :: Maybe String -> Maybe Text
      sanitizeEnvText maybeValue =
        case maybeValue of
          Nothing -> Nothing
          Just raw ->
            let trimmed = T.strip (T.pack raw)
            in if T.null trimmed then Nothing else Just trimmed
      sanitizeEnvString :: Maybe String -> Maybe String
      sanitizeEnvString = fmap T.unpack . sanitizeEnvText

      sanitizedCompiler = sanitizeEnvText cppCompiler
      sanitizedStd = sanitizeEnvText cppStd
      sanitizedVerbose = sanitizeEnvString verboseLevel
      sanitizedInterop = sanitizeEnvString enableInterop
      sanitizedExperimental = sanitizeEnvString experimentalOptimizations
      sanitizedSkip = sanitizeEnvString skipCompilerCheck

  resolvedVerboseLevel <- case sanitizedVerbose of
    Nothing -> pure (ccVerboseLevel config)
    Just raw -> case readMaybe raw of
      Just level -> pure level
      Nothing -> do
        putStrLn $ "Warning: Ignoring invalid FLUXUS_VERBOSE value '" ++ raw ++ "' (keeping existing verbosity)."
        pure (ccVerboseLevel config)

  return config
    { ccCppCompiler = fromMaybe (ccCppCompiler config) sanitizedCompiler
    , ccCppStandard = fromMaybe (ccCppStandard config) sanitizedStd
    , ccVerboseLevel = resolvedVerboseLevel
    , ccEnableInterop = parseBoolOverride (ccEnableInterop config) sanitizedInterop
    , ccEnableExperimentalOptimizations = parseBoolOverride (ccEnableExperimentalOptimizations config) sanitizedExperimental
    , ccSkipCompilerCheck = parseBoolOverride (ccSkipCompilerCheck config) sanitizedSkip
    }
  where
    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing
    parseBoolOverride current envValue =
      maybe current interpret envValue
      where
        interpret value =
          case map toLower value of
            "1" -> True
            "true" -> True
            "yes" -> True
            "on" -> True
            "0" -> False
            "false" -> False
            "no" -> False
            "off" -> False
            _ -> current

-- | Validate configuration file syntax
validateConfigFile :: FilePath -> IO (Either String ())
validateConfigFile configFile = do
  result <- loadConfigFromFile configFile
  case result of
    Left err -> return $ Left (renderConfigFileError err)
    Right _ -> return $ Right ()

-- | Check if system meets requirements for compilation
checkSystemRequirements :: CompilerConfig -> IO (Either String ())
checkSystemRequirements config = do
  let skipCompilerCheck = ccSkipCompilerCheck config
      stopAtCodegen = ccStopAtCodegen config
      shouldCheckCompiler = not skipCompilerCheck && not stopAtCodegen
  
  when skipCompilerCheck $
    hPutStrLn stderr "Warning: Skipping C++ compiler requirement check (ccSkipCompilerCheck enabled)"
  when (stopAtCodegen && not skipCompilerCheck) $
    hPutStrLn stderr "Skipping C++ compiler requirement check because stop-at-codegen is enabled"
  
  compilerCheckResult <-
    if shouldCheckCompiler
      then do
        detection <- detectCompilerBinary config
        case detection of
          Left errText -> pure $ Left (T.unpack errText)
          Right (resolved, fallbackUsed) -> do
            when fallbackUsed $
              hPutStrLn stderr $
                "Warning: Requested C++ compiler '" ++ T.unpack (ccCppCompiler config) ++
                "' was not found; using '" ++ T.unpack resolved ++ "' instead."
            pure (Right ())
      else pure (Right ())
  
  case compilerCheckResult of
    Left err -> pure $ Left err
    Right () -> do
      mapM_ checkIncludePath (ccIncludePaths config)
      mapM_ checkLibraryPath (ccLibraryPaths config)
      pure $ Right ()
  where
    checkIncludePath path = do
      exists <- doesDirectoryExist path
      unless exists $ hPutStrLn stderr $ "Warning: Include path does not exist: " ++ path
    checkLibraryPath path = do
      exists <- doesDirectoryExist path
      unless exists $ hPutStrLn stderr $ "Warning: Library path does not exist: " ++ path

-- | Predefined configurations
developmentConfig :: CompilerConfig
developmentConfig = defaultConfig
  { ccOptimizationLevel = O0
  , ccEnableDebugInfo = True
  , ccVerboseLevel = 2
  , ccKeepIntermediates = True
  , ccStrictMode = False
  }

productionConfig :: CompilerConfig
productionConfig = defaultConfig
  { ccOptimizationLevel = O3
  , ccEnableDebugInfo = False
  , ccVerboseLevel = 0
  , ccKeepIntermediates = False
  , ccStrictMode = True
  , ccEnableProfiler = False
  }

debugConfig :: CompilerConfig
debugConfig = defaultConfig
  { ccOptimizationLevel = O0
  , ccEnableDebugInfo = True
  , ccEnableProfiler = True
  , ccVerboseLevel = 3
  , ccKeepIntermediates = True
  }

-- | Convert configuration to command line arguments
configToArgs :: CompilerConfig -> [String]
configToArgs config = concat
  [ case ccSourceLanguage config of
      Python -> ["--python"]
      Go -> ["--go"]
  , case ccOptimizationLevel config of
      O0 -> ["-O0"]
      O1 -> ["-O1"]
      O2 -> ["-O2"]
      O3 -> ["-O3"]
      Os -> ["-Os"]
  , if ccEnableInterop config then ["--enable-interop"] else ["--disable-interop"]
  , if ccEnableDebugInfo config then ["--enable-debug"] else ["--disable-debug"]
  , if ccEnableProfiler config then ["--enable-profiler"] else ["--disable-profiler"]
  , if ccEnableParallel config then ["--enable-parallel"] else ["--disable-parallel"]
  , if ccStrictMode config then ["--strict"] else ["--no-strict"]
  , if ccEnableAnalysis config then ["--enable-analysis"] else ["--disable-analysis"]
  , if ccEnableExperimentalOptimizations config then ["--enable-experimental-optimizations"] else ["--disable-experimental-optimizations"]
  , if ccStopAtCodegen config then ["--stop-at-codegen"] else ["--full-pipeline"]
  , if ccKeepIntermediates config then ["--keep-intermediates"] else ["--clean-intermediates"]
  , if ccSkipCompilerCheck config then ["--skip-compiler-check"] else ["--require-compiler-check"]
  , replicate (ccVerboseLevel config) "-v"
  , maybe [] (\path -> ["-o", path]) (ccOutputPath config)
  , maybe [] (\dir -> ["--work-dir", dir]) (ccWorkDirectory config)
  , ["--cpp-std", T.unpack $ ccCppStandard config]
  , ["--cpp-compiler", T.unpack $ ccCppCompiler config]
  , ["--max-concurrency", show $ ccMaxConcurrency config]
  , concatMap (\path -> ["--include", path]) (ccIncludePaths config)
  , concatMap (\path -> ["--library-path", path]) (ccLibraryPaths config)
  , concatMap (\lib -> ["--link", T.unpack lib]) (ccLinkedLibraries config)
  , ["--target", showTargetPlatform $ ccTargetPlatform config]
  ]


-- | Pretty print configuration
printConfig :: CompilerConfig -> IO ()
printConfig config = do
  putStrLn "=== Fluxus Compiler Configuration ==="
  putStrLn $ "Source Language: " ++ show (ccSourceLanguage config)
  putStrLn $ "Optimization Level: " ++ show (ccOptimizationLevel config)
  putStrLn $ "Target Platform: " ++ showTargetPlatform (ccTargetPlatform config)
  putStrLn $ "Output Path: " ++ maybe "<auto>" id (ccOutputPath config)
  putStrLn $ "Enable Interop: " ++ show (ccEnableInterop config)
  putStrLn $ "Debug Info: " ++ show (ccEnableDebugInfo config)
  putStrLn $ "Profiler: " ++ show (ccEnableProfiler config)
  putStrLn $ "Parallel: " ++ show (ccEnableParallel config)
  putStrLn $ "Max Concurrency: " ++ show (ccMaxConcurrency config)
  putStrLn $ "C++ Standard: " ++ T.unpack (ccCppStandard config)
  putStrLn $ "C++ Compiler: " ++ T.unpack (ccCppCompiler config)
  putStrLn $ "Verbose Level: " ++ show (ccVerboseLevel config)
  putStrLn $ "Work Directory: " ++ maybe "<temp>" id (ccWorkDirectory config)
  putStrLn $ "Keep Intermediates: " ++ show (ccKeepIntermediates config)
  putStrLn $ "Strict Mode: " ++ show (ccStrictMode config)
  putStrLn $ "Skip Compiler Check: " ++ show (ccSkipCompilerCheck config)
  putStrLn $ "Stop at Codegen: " ++ show (ccStopAtCodegen config)
  putStrLn $ "Static Analysis: " ++ show (ccEnableAnalysis config)
  putStrLn $ "Experimental Optimizations: " ++ show (ccEnableExperimentalOptimizations config)
  putStrLn "=============================================="


