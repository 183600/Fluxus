{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Fluxus.Compiler.Config (
  SourceLanguage(..),
  OptimizationLevel(..),
  TargetPlatform(..),
  CompilerConfig(..),
  MergeStrategy(..),
  defaultConfig,
  developmentConfig,
  productionConfig,
  debugConfig,
  parseSourceLanguage,
  showSourceLanguage,
  parseOptLevel,
  showOptLevel,
  parseTargetPlatform,
  showTargetPlatform,
  configParser,
  loadConfig,
  loadConfigFromFile,
  mergeConfigs,
  applyEnvironmentOverrides,
  validateConfigFile,
  checkSystemRequirements,
  configToArgs,
  printConfig
) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither, ParseException)
import System.Environment (lookupEnv)
import System.Directory
import System.Info (os, arch)
import System.IO (hPutStrLn, stderr)
import Control.Monad (unless, when, forM_)
import Data.List (isInfixOf, nub)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Options.Applicative

-- ============================================================================
-- Data Types (should be in a separate Types module to avoid circular deps)
-- ============================================================================

data SourceLanguage = Python | Go
  deriving (Show, Read, Eq, Generic)

data OptimizationLevel = O0 | O1 | O2 | O3 | Os
  deriving (Show, Read, Eq, Generic, Ord, Enum)

data TargetPlatform 
  = Linux_x86_64 
  | Linux_ARM64 
  | Darwin_x86_64 
  | Darwin_ARM64 
  | Windows_x86_64
  deriving (Show, Read, Eq, Generic)

data CompilerConfig = CompilerConfig
  { ccSourceLanguage :: SourceLanguage
  , ccOptimizationLevel :: OptimizationLevel
  , ccTargetPlatform :: TargetPlatform
  , ccOutputPath :: Maybe FilePath
  , ccGoldenFile :: Maybe FilePath
  , ccEnableInterop :: Bool
  , ccEnableDebugInfo :: Bool
  , ccEnableProfiler :: Bool
  , ccEnableParallel :: Bool
  , ccMaxConcurrency :: Int
  , ccIncludePaths :: [FilePath]
  , ccLibraryPaths :: [FilePath]
  , ccLinkedLibraries :: [Text]
  , ccCppStandard :: Text
  , ccCppCompiler :: Text
  , ccVerboseLevel :: Int
  , ccWorkDirectory :: Maybe FilePath
  , ccKeepIntermediates :: Bool
  , ccStrictMode :: Bool
  , ccEnableAnalysis :: Bool
  , ccStopAtCodegen :: Bool
  , ccInputFiles :: [FilePath]  -- Added to store input files
  } deriving (Show, Eq, Generic)

-- Configuration merge strategy
data MergeStrategy = Override | Append | Keep
  deriving (Show, Eq)

-- ============================================================================
-- Default and Preset Configurations
-- ============================================================================

defaultConfig :: CompilerConfig
defaultConfig = CompilerConfig
  { ccSourceLanguage = Python
  , ccOptimizationLevel = O2
  , ccTargetPlatform = detectPlatform
  , ccOutputPath = Nothing
  , ccGoldenFile = Nothing
  , ccEnableInterop = True
  , ccEnableDebugInfo = False
  , ccEnableProfiler = False
  , ccEnableParallel = True
  , ccMaxConcurrency = 4
  , ccIncludePaths = getDefaultIncludePaths
  , ccLibraryPaths = getDefaultLibraryPaths
  , ccLinkedLibraries = ["stdc++", "pthread"]
  , ccCppStandard = "c++20"
  , ccCppCompiler = "clang++"
  , ccVerboseLevel = 1
  , ccWorkDirectory = Nothing
  , ccKeepIntermediates = False
  , ccStrictMode = False
  , ccEnableAnalysis = True
  , ccStopAtCodegen = False
  , ccInputFiles = []
  }

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

-- ============================================================================
-- Platform Detection and Defaults
-- ============================================================================

detectPlatform :: TargetPlatform
detectPlatform = case (os, arch) of
  ("linux", "x86_64")  -> Linux_x86_64
  ("linux", "aarch64") -> Linux_ARM64
  ("darwin", "x86_64") -> Darwin_x86_64
  ("darwin", "aarch64") -> Darwin_ARM64
  ("mingw32", "x86_64") -> Windows_x86_64
  ("win32", "x86_64") -> Windows_x86_64
  _ -> Linux_x86_64  -- Default fallback

getDefaultIncludePaths :: [FilePath]
getDefaultIncludePaths = case detectPlatform of
  Linux_x86_64 -> ["/usr/include", "/usr/local/include"]
  Linux_ARM64 -> ["/usr/include", "/usr/local/include"]
  Darwin_x86_64 -> ["/usr/local/include", "/opt/local/include"]
  Darwin_ARM64 -> ["/opt/homebrew/include", "/usr/local/include"]
  Windows_x86_64 -> []  -- Windows doesn't have standard paths

getDefaultLibraryPaths :: [FilePath]
getDefaultLibraryPaths = case detectPlatform of
  Linux_x86_64 -> ["/usr/lib", "/usr/local/lib", "/usr/lib/x86_64-linux-gnu"]
  Linux_ARM64 -> ["/usr/lib", "/usr/local/lib", "/usr/lib/aarch64-linux-gnu"]
  Darwin_x86_64 -> ["/usr/local/lib", "/opt/local/lib"]
  Darwin_ARM64 -> ["/opt/homebrew/lib", "/usr/local/lib"]
  Windows_x86_64 -> []

-- ============================================================================
-- Parsing and Showing Functions (DRY principle)
-- ============================================================================

parseSourceLanguage :: String -> Maybe SourceLanguage
parseSourceLanguage = \case
  "python" -> Just Python
  "Python" -> Just Python
  "go" -> Just Go
  "Go" -> Just Go
  _ -> Nothing

showSourceLanguage :: SourceLanguage -> String
showSourceLanguage = \case
  Python -> "python"
  Go -> "go"

parseOptLevel :: String -> Maybe OptimizationLevel
parseOptLevel = \case
  "O0" -> Just O0
  "O1" -> Just O1
  "O2" -> Just O2
  "O3" -> Just O3
  "Os" -> Just Os
  "0" -> Just O0
  "1" -> Just O1
  "2" -> Just O2
  "3" -> Just O3
  "s" -> Just Os
  _ -> Nothing

showOptLevel :: OptimizationLevel -> String
showOptLevel = \case
  O0 -> "O0"
  O1 -> "O1"
  O2 -> "O2"
  O3 -> "O3"
  Os -> "Os"

parseTargetPlatform :: String -> Maybe TargetPlatform
parseTargetPlatform = \case
  "linux-x86_64" -> Just Linux_x86_64
  "linux-arm64" -> Just Linux_ARM64
  "darwin-x86_64" -> Just Darwin_x86_64
  "darwin-arm64" -> Just Darwin_ARM64
  "windows-x86_64" -> Just Windows_x86_64
  _ -> Nothing

showTargetPlatform :: TargetPlatform -> String
showTargetPlatform = \case
  Linux_x86_64 -> "linux-x86_64"
  Linux_ARM64 -> "linux-arm64"
  Darwin_x86_64 -> "darwin-x86_64"
  Darwin_ARM64 -> "darwin-arm64"
  Windows_x86_64 -> "windows-x86_64"

-- ============================================================================
-- Command Line Parser using optparse-applicative
-- ============================================================================

configParser :: Options.Applicative.Parser CompilerConfig
configParser = CompilerConfig
  <$> languageOption
  <*> optimizationOption
  <*> targetOption
  <*> outputOption
  <*> goldenOption
  <*> interopOption
  <*> debugOption
  <*> profilerOption
  <*> parallelOption
  <*> concurrencyOption
  <*> includeOptions
  <*> libraryPathOptions
  <*> linkOptions
  <*> cppStandardOption
  <*> cppCompilerOption
  <*> verboseOption
  <*> workDirOption
  <*> keepIntermediatesOption
  <*> strictOption
  <*> analysisOption
  <*> stopAtCodegenOption
  <*> inputFilesOption

languageOption :: Options.Applicative.Parser SourceLanguage
languageOption = 
  flag' Python (long "python" <> help "Use Python as source language") <|>
  flag' Go (long "go" <> help "Use Go as source language") <|>
  pure (ccSourceLanguage defaultConfig)

optimizationOption :: Options.Applicative.Parser OptimizationLevel
optimizationOption =
  -- Support GCC/Clang style: -O0, -O1, -O2, -O3, -Os (also works with spaced form: -O 2)
  option (maybeReader parseOptLevel)
    ( short 'O'
   <> metavar "LEVEL"
   <> help "Optimization level (0,1,2,3 or s for size; accepts -O2, -O3, -Os)"
    ) <|>
  -- Also accept individual short flags like -0, -1, -2, -3, -s and long forms --O0..--Os
  flag' O0 (short '0' <> long "O0" <> help "No optimization") <|>
  flag' O1 (short '1' <> long "O1" <> help "Basic optimization") <|>
  flag' O2 (short '2' <> long "O2" <> help "Standard optimization") <|>
  flag' O3 (short '3' <> long "O3" <> help "Aggressive optimization") <|>
  flag' Os (short 's' <> long "Os" <> help "Size optimization") <|>
  pure (ccOptimizationLevel defaultConfig)

targetOption :: Options.Applicative.Parser TargetPlatform
targetOption = option (maybeReader parseTargetPlatform)
  ( long "target"
  <> metavar "PLATFORM"
  <> value (ccTargetPlatform defaultConfig)
  <> help "Target platform (linux-x86_64, darwin-arm64, etc.)"
  )

outputOption :: Options.Applicative.Parser (Maybe FilePath)
outputOption = optional $ strOption
  ( short 'o'
  <> long "output"
  <> metavar "FILE"
  <> help "Output file path"
  )

goldenOption :: Options.Applicative.Parser (Maybe FilePath)
goldenOption = optional $ strOption
  ( long "golden"
  <> metavar "FILE"
  <> help "Path to golden output file for test mode"
  )

interopOption :: Options.Applicative.Parser Bool
interopOption = 
  flag' True (long "enable-interop" <> help "Enable language interoperability") <|>
  flag' False (long "disable-interop" <> help "Disable language interoperability") <|>
  pure (ccEnableInterop defaultConfig)

debugOption :: Options.Applicative.Parser Bool
debugOption = 
  flag' True (long "enable-debug" <> help "Enable debug information") <|>
  flag' False (long "disable-debug" <> help "Disable debug information") <|>
  pure (ccEnableDebugInfo defaultConfig)

profilerOption :: Options.Applicative.Parser Bool
profilerOption = 
  flag' True (long "enable-profiler" <> help "Enable profiling support") <|>
  flag' False (long "disable-profiler" <> help "Disable profiling support") <|>
  pure (ccEnableProfiler defaultConfig)

parallelOption :: Options.Applicative.Parser Bool
parallelOption = 
  flag' True (long "enable-parallel" <> help "Enable parallel compilation") <|>
  flag' False (long "disable-parallel" <> help "Disable parallel compilation") <|>
  pure (ccEnableParallel defaultConfig)

concurrencyOption :: Options.Applicative.Parser Int
concurrencyOption = option auto
  ( long "max-concurrency"
  <> metavar "N"
  <> value (ccMaxConcurrency defaultConfig)
  <> help "Maximum number of concurrent jobs"
  )

includeOptions :: Options.Applicative.Parser [FilePath]
includeOptions = many $ strOption
  ( short 'I'
  <> long "include"
  <> metavar "PATH"
  <> help "Add include path"
  )

libraryPathOptions :: Options.Applicative.Parser [FilePath]
libraryPathOptions = many $ strOption
  ( short 'L'
  <> long "library-path"
  <> metavar "PATH"
  <> help "Add library path"
  )

linkOptions :: Options.Applicative.Parser [Text]
linkOptions = fmap (map T.pack) $ many $ strOption
  ( short 'l'
  <> long "link"
  <> metavar "LIBRARY"
  <> help "Link with library"
  )

cppStandardOption :: Options.Applicative.Parser Text
cppStandardOption = fmap T.pack $ strOption
  ( long "cpp-std"
  <> metavar "STD"
  <> value (T.unpack $ ccCppStandard defaultConfig)
  <> help "C++ standard (c++17, c++20, etc.)"
  )

cppCompilerOption :: Options.Applicative.Parser Text
cppCompilerOption = fmap T.pack $ strOption
  ( long "cpp-compiler"
  <> metavar "COMPILER"
  <> value (T.unpack $ ccCppCompiler defaultConfig)
  <> help "C++ compiler to use"
  )

verboseOption :: Options.Applicative.Parser Int
verboseOption = length <$> many
  (flag' () (short 'v' <> long "verbose" <> help "Increase verbosity"))

workDirOption :: Options.Applicative.Parser (Maybe FilePath)
workDirOption = optional $ strOption
  ( long "work-dir"
  <> metavar "DIR"
  <> help "Working directory for intermediate files"
  )

keepIntermediatesOption :: Options.Applicative.Parser Bool
keepIntermediatesOption = 
  flag False True
    ( long "keep-intermediates"
    <> help "Keep intermediate files"
    )

strictOption :: Options.Applicative.Parser Bool
strictOption = 
  flag False True
    ( long "strict"
    <> help "Enable strict mode"
    )

analysisOption :: Options.Applicative.Parser Bool
analysisOption = 
  flag' True (long "enable-analysis" <> help "Enable static analysis") <|>
  flag' False (long "disable-analysis" <> help "Disable static analysis") <|>
  pure (ccEnableAnalysis defaultConfig)

stopAtCodegenOption :: Options.Applicative.Parser Bool
stopAtCodegenOption = 
  flag False True
    ( long "stop-at-codegen"
    <> help "Stop after code generation"
    )

inputFilesOption :: Options.Applicative.Parser [FilePath]
inputFilesOption = many $ argument str
  ( metavar "FILES..."
  <> help "Input source files"
  )

-- Command line interface with help
commandLineInterface :: ParserInfo CompilerConfig
commandLineInterface = info (configParser <**> helper)
  ( fullDesc
  <> progDesc "Fluxus Compiler - Multi-language compilation framework"
  <> header "fluxus - compile Python and Go to optimized C++"
  <> footer "For more information, visit https://github.com/fluxus-lang/fluxus"
  )

-- ============================================================================
-- Configuration Loading and Merging
-- ============================================================================

loadConfig :: [String] -> IO (Either String CompilerConfig)
loadConfig args = do
  -- Parse command line arguments using optparse-applicative
  let parseResult = execParserPure defaultPrefs commandLineInterface args
  case parseResult of
    Options.Applicative.Success cliConfig -> do
      -- Load from config file if it exists
      configFromFile <- loadConfigFromFile "fluxus.yaml"
      case configFromFile of
        Left err -> do
          -- Only warn if file exists but can't be parsed
          exists <- doesFileExist "fluxus.yaml"
          when exists $ hPutStrLn stderr $ "Warning: " ++ err

          -- Apply environment overrides to CLI config
          finalConfig <- applyEnvironmentOverrides cliConfig
          -- Validate the configuration
          validation <- checkSystemRequirements finalConfig
          case validation of
            Left validationErr -> return $ Left validationErr
            Right () -> return $ Right finalConfig
            
        Right fileConfig -> do
          -- Merge: CLI > Environment > File > Default
          let mergedConfig = mergeConfigs Override fileConfig cliConfig
          finalConfig <- applyEnvironmentOverrides mergedConfig
          -- Validate the configuration
          validation <- checkSystemRequirements finalConfig
          case validation of
            Left validationErr -> return $ Left validationErr
            Right () -> return $ Right finalConfig
            
    Failure failure -> return $ Left $ show failure
    CompletionInvoked _ -> return $ Left "Shell completion invoked"

loadConfigFromFile :: FilePath -> IO (Either String CompilerConfig)
loadConfigFromFile configFile = do
  exists <- doesFileExist configFile
  if not exists
    then return $ Left $ "Configuration file not found: " ++ configFile
    else do
      result <- decodeFileEither configFile
      case result of
        Left err -> return $ Left $ "Failed to parse config file: " ++ prettyPrintParseException err
        Right config -> return $ Right config

-- Enhanced merge with strategy parameter

mergeConfigs :: MergeStrategy -> CompilerConfig -> CompilerConfig -> CompilerConfig
mergeConfigs strategy base override = CompilerConfig
  { ccSourceLanguage = ccSourceLanguage override
  , ccOptimizationLevel = ccOptimizationLevel override
  , ccTargetPlatform = ccTargetPlatform override
  , ccOutputPath = ccOutputPath override <|> ccOutputPath base
  , ccGoldenFile = ccGoldenFile override <|> ccGoldenFile base
  , ccEnableInterop = ccEnableInterop override
  , ccEnableDebugInfo = ccEnableDebugInfo override
  , ccEnableProfiler = ccEnableProfiler override
  , ccEnableParallel = ccEnableParallel override
  , ccMaxConcurrency = ccMaxConcurrency override
  , ccIncludePaths = mergeLists strategy (ccIncludePaths base) (ccIncludePaths override)
  , ccLibraryPaths = mergeLists strategy (ccLibraryPaths base) (ccLibraryPaths override)
  , ccLinkedLibraries = mergeLists strategy (ccLinkedLibraries base) (ccLinkedLibraries override)
  , ccCppStandard = ccCppStandard override
  , ccCppCompiler = ccCppCompiler override
  , ccVerboseLevel = ccVerboseLevel override  -- Fixed: direct override
  , ccWorkDirectory = ccWorkDirectory override <|> ccWorkDirectory base
  , ccKeepIntermediates = ccKeepIntermediates override
  , ccStrictMode = ccStrictMode override
  , ccEnableAnalysis = ccEnableAnalysis override
  , ccStopAtCodegen = ccStopAtCodegen override
  , ccInputFiles = mergeLists strategy (ccInputFiles base) (ccInputFiles override)
  }
  where
    mergeLists Override _ overrideVal = overrideVal
    mergeLists Append baseVal overrideVal = nub (overrideVal ++ baseVal)  -- Remove duplicates
    mergeLists Keep baseVal _ = baseVal

-- Enhanced environment variable support
applyEnvironmentOverrides :: CompilerConfig -> IO CompilerConfig
applyEnvironmentOverrides config = do
  -- Comprehensive environment variable support
  envVars <- sequence
    [ fmap (fmap T.pack) (lookupEnv "CXX")
    , fmap (fmap T.pack) (lookupEnv "FLUXUS_CPP_STD")
    , fmap (fmap T.pack) (lookupEnv "FLUXUS_VERBOSE")
    , fmap (fmap T.pack) (lookupEnv "FLUXUS_INTEROP")
    , fmap (fmap T.pack) (lookupEnv "FLUXUS_DEBUG")
    , fmap (fmap T.pack) (lookupEnv "FLUXUS_OPTIMIZATION")
    , fmap (fmap T.pack) (lookupEnv "FLUXUS_TARGET")
    , fmap (fmap T.pack) (lookupEnv "FLUXUS_PARALLEL")
    , fmap (fmap T.pack) (lookupEnv "FLUXUS_STRICT")
    , fmap (fmap T.pack) (lookupEnv "FLUXUS_OUTPUT")
    ]
  
  let safeEnvVars = envVars ++ replicate (10 - length envVars) Nothing
      resultVars = take 10 safeEnvVars
      cppCompiler = resultVars !! 0
      cppStd = resultVars !! 1
      verbose = resultVars !! 2
      interop = resultVars !! 3
      debug = resultVars !! 4
      optLevel = resultVars !! 5
      target = resultVars !! 6
      parallel = resultVars !! 7
      strict = resultVars !! 8
      output = resultVars !! 9
  
  return config
    { ccCppCompiler = fromMaybe (ccCppCompiler config) cppCompiler
    , ccCppStandard = fromMaybe (ccCppStandard config) cppStd
    , ccVerboseLevel = maybe (ccVerboseLevel config) (fromMaybe 0 . readMaybe . T.unpack) verbose
    , ccEnableInterop = maybe (ccEnableInterop config) (== "1") interop
    , ccEnableDebugInfo = maybe (ccEnableDebugInfo config) (== "1") debug
    , ccOptimizationLevel = maybe (ccOptimizationLevel config) 
                            (fromMaybe (ccOptimizationLevel config) . parseOptLevel . T.unpack) optLevel
    , ccTargetPlatform = maybe (ccTargetPlatform config)
                          (fromMaybe (ccTargetPlatform config) . parseTargetPlatform . T.unpack) target
    , ccEnableParallel = maybe (ccEnableParallel config) (== "1") parallel
    , ccStrictMode = maybe (ccStrictMode config) (== "1") strict
    , ccOutputPath = fmap T.unpack output <|> ccOutputPath config
    }
  where
    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

-- ============================================================================
-- Validation and System Requirements
-- ============================================================================

validateConfigFile :: FilePath -> IO (Either String ())
validateConfigFile configFile = do
  result <- loadConfigFromFile configFile
  case result of
    Left err -> return $ Left err
    Right config -> do
      -- Additional validation
      validation <- checkSystemRequirements config
      return validation

-- Cross-platform system requirements checking
checkSystemRequirements :: CompilerConfig -> IO (Either String ())
checkSystemRequirements config = do
  -- Use findExecutable instead of which (cross-platform)
  let compiler = T.unpack $ ccCppCompiler config
  compilerPath <- findExecutable compiler
  
  case compilerPath of
    Nothing -> return $ Left $ "C++ compiler not found: " ++ compiler
    Just _path -> do
      -- Validate paths exist (with warnings for missing ones)
      validatePaths "include" (ccIncludePaths config)
      validatePaths "library" (ccLibraryPaths config)
      
      -- Check for required tools based on features
      when (ccEnableProfiler config) $ do
        profilerPath <- findExecutable "perf"  -- Linux
                        <|> findExecutable "dtrace"  -- macOS
                        <|> findExecutable "xperf"  -- Windows
        case profilerPath of
          Nothing -> hPutStrLn stderr "Warning: Profiler tools not found"
          Just _ -> return ()
      
      return $ Right ()
  where
    validatePaths :: String -> [FilePath] -> IO ()
    validatePaths pathType paths = forM_ paths $ \path -> do
      exists <- doesDirectoryExist path
      unless exists $ do
        -- Only warn for non-platform-specific paths
        unless (isPlatformSpecific path) $
          hPutStrLn stderr $ "Warning: " ++ pathType ++ " path does not exist: " ++ path
    
    isPlatformSpecific path = 
      "/opt/homebrew" `isInfixOf` path ||  -- macOS ARM
      "/opt/local" `isInfixOf` path ||     -- MacPorts
      "C:\\Program Files" `isInfixOf` path  -- Windows

-- ============================================================================
-- JSON Serialization
-- ============================================================================

instance ToJSON SourceLanguage where
  toJSON = String . T.pack . showSourceLanguage

instance FromJSON SourceLanguage where
  parseJSON = withText "SourceLanguage" $ \t ->
    case parseSourceLanguage (T.unpack t) of
      Just lang -> pure lang
      Nothing -> fail $ "Unknown source language: " ++ T.unpack t

instance ToJSON OptimizationLevel where
  toJSON = String . T.pack . showOptLevel

instance FromJSON OptimizationLevel where
  parseJSON = withText "OptimizationLevel" $ \t ->
    case parseOptLevel (T.unpack t) of
      Just level -> pure level
      Nothing -> fail $ "Unknown optimization level: " ++ T.unpack t

instance ToJSON TargetPlatform where
  toJSON = String . T.pack . showTargetPlatform

instance FromJSON TargetPlatform where
  parseJSON = withText "TargetPlatform" $ \t ->
    case parseTargetPlatform (T.unpack t) of
      Just platform -> pure platform
      Nothing -> fail $ "Unknown target platform: " ++ T.unpack t

instance ToJSON CompilerConfig where
  toJSON CompilerConfig{..} = object
    [ "source_language" .= ccSourceLanguage
    , "optimization_level" .= ccOptimizationLevel
    , "target_platform" .= ccTargetPlatform
    , "output_path" .= ccOutputPath
    , "golden_file" .= ccGoldenFile
    , "enable_interop" .= ccEnableInterop
    , "enable_debug_info" .= ccEnableDebugInfo
    , "enable_profiler" .= ccEnableProfiler
    , "enable_parallel" .= ccEnableParallel
    , "max_concurrency" .= ccMaxConcurrency
    , "include_paths" .= ccIncludePaths
    , "library_paths" .= ccLibraryPaths
    , "linked_libraries" .= ccLinkedLibraries
    , "cpp_standard" .= ccCppStandard
    , "cpp_compiler" .= ccCppCompiler
    , "verbose_level" .= ccVerboseLevel
    , "work_directory" .= ccWorkDirectory
    , "keep_intermediates" .= ccKeepIntermediates
    , "strict_mode" .= ccStrictMode
    , "enable_analysis" .= ccEnableAnalysis
    , "stop_at_codegen" .= ccStopAtCodegen
    , "input_files" .= ccInputFiles
    ]

instance FromJSON CompilerConfig where
  parseJSON = withObject "CompilerConfig" $ \o -> do
    ccSourceLanguage <- o .:? "source_language" .!= Python
    ccOptimizationLevel <- o .:? "optimization_level" .!= O2
    ccTargetPlatform <- o .:? "target_platform" .!= detectPlatform
    ccOutputPath <- o .:? "output_path"
    ccGoldenFile <- o .:? "golden_file"
    ccEnableInterop <- o .:? "enable_interop" .!= True
    ccEnableDebugInfo <- o .:? "enable_debug_info" .!= False
    ccEnableProfiler <- o .:? "enable_profiler" .!= False
    ccEnableParallel <- o .:? "enable_parallel" .!= True
    ccMaxConcurrency <- o .:? "max_concurrency" .!= 4
    ccIncludePaths <- o .:? "include_paths" .!= []
    ccLibraryPaths <- o .:? "library_paths" .!= []
    ccLinkedLibraries <- o .:? "linked_libraries" .!= []
    ccCppStandard <- o .:? "cpp_standard" .!= "c++20"
    ccCppCompiler <- o .:? "cpp_compiler" .!= "clang++"
    ccVerboseLevel <- o .:? "verbose_level" .!= 1
    ccWorkDirectory <- o .:? "work_directory"
    ccKeepIntermediates <- o .:? "keep_intermediates" .!= False
    ccStrictMode <- o .:? "strict_mode" .!= False
    ccEnableAnalysis <- o .:? "enable_analysis" .!= True
    ccStopAtCodegen <- o .:? "stop_at_codegen" .!= False
    ccInputFiles <- o .:? "input_files" .!= []
    pure CompilerConfig{..}

-- ============================================================================
-- Utility Functions
-- ============================================================================

configToArgs :: CompilerConfig -> [String]
configToArgs config = concat
  [ ["--" ++ showSourceLanguage (ccSourceLanguage config)]
  , ["-" ++ showOptLevel (ccOptimizationLevel config)]
  , if ccEnableInterop config then ["--enable-interop"] else ["--disable-interop"]
  , if ccEnableDebugInfo config then ["--enable-debug"] else ["--disable-debug"]
  , if ccEnableProfiler config then ["--enable-profiler"] else ["--disable-profiler"]
  , if ccEnableParallel config then ["--enable-parallel"] else ["--disable-parallel"]
  , if ccStrictMode config then ["--strict"] else []
  , if ccKeepIntermediates config then ["--keep-intermediates"] else []
  , replicate (ccVerboseLevel config) "-v"
  , maybe [] (\path -> ["-o", path]) (ccOutputPath config)
  , maybe [] (\path -> ["--golden", path]) (ccGoldenFile config)
  , maybe [] (\dir -> ["--work-dir", dir]) (ccWorkDirectory config)
  , ["--cpp-std", T.unpack $ ccCppStandard config]
  , ["--cpp-compiler", T.unpack $ ccCppCompiler config]
  , ["--max-concurrency", show $ ccMaxConcurrency config]
  , concatMap (\path -> ["-I", path]) (ccIncludePaths config)
  , concatMap (\path -> ["-L", path]) (ccLibraryPaths config)
  , concatMap (\lib -> ["-l", T.unpack lib]) (ccLinkedLibraries config)
  , ["--target", showTargetPlatform $ ccTargetPlatform config]
  , if ccStopAtCodegen config then ["--stop-at-codegen"] else []
  , ccInputFiles config
  ]

printConfig :: CompilerConfig -> IO ()
printConfig config = do
  putStrLn "╔══════════════════════════════════════════════╗"
  putStrLn "║     Fluxus Compiler Configuration           ║"
  putStrLn "╚══════════════════════════════════════════════╝"
  putStrLn $ "  Source Language    : " ++ show (ccSourceLanguage config)
  putStrLn $ "  Optimization Level : " ++ showOptLevel (ccOptimizationLevel config)
  putStrLn $ "  Target Platform    : " ++ showTargetPlatform (ccTargetPlatform config)
  putStrLn $ "  Output Path        : " ++ fromMaybe "<auto>" (ccOutputPath config)
  putStrLn $ "  Golden File        : " ++ fromMaybe "<none>" (ccGoldenFile config)
  putStrLn $ "  Interop Enabled    : " ++ show (ccEnableInterop config)
  putStrLn $ "  Debug Info         : " ++ show (ccEnableDebugInfo config)
  putStrLn $ "  Profiler           : " ++ show (ccEnableProfiler config)
  putStrLn $ "  Parallel Build     : " ++ show (ccEnableParallel config)
  putStrLn $ "  Max Concurrency    : " ++ show (ccMaxConcurrency config)
  putStrLn $ "  C++ Standard       : " ++ T.unpack (ccCppStandard config)
  putStrLn $ "  C++ Compiler       : " ++ T.unpack (ccCppCompiler config)
  putStrLn $ "  Verbose Level      : " ++ show (ccVerboseLevel config)
  putStrLn $ "  Work Directory     : " ++ fromMaybe "<temp>" (ccWorkDirectory config)
  putStrLn $ "  Keep Intermediates : " ++ show (ccKeepIntermediates config)
  putStrLn $ "  Strict Mode        : " ++ show (ccStrictMode config)
  putStrLn $ "  Static Analysis    : " ++ show (ccEnableAnalysis config)
  putStrLn $ "  Input Files        : " ++ show (ccInputFiles config)
  putStrLn "═══════════════════════════════════════════════"

-- Helper to pretty print YAML parse exceptions
prettyPrintParseException :: ParseException -> String
prettyPrintParseException = show  -- Can be enhanced for better formatting