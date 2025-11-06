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
    -- * Command line parsing
  , parseCommandLineArgs
  , LoadConfigResult(..)
  , CLICommand(..)
  , fluxusVersionString
    -- * Configuration merging
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
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Yaml (decodeFileEither)
import System.Environment (lookupEnv)
import System.FilePath
import System.Directory
import Control.Monad (unless, when)
import Data.Char (toLower)
import Control.Monad.IO.Class
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import GHC.Generics (Generic)

import Fluxus.Compiler.Driver

-- | Result of parsing command line arguments.
data CLICommand
  = CLICommandModify (CompilerConfig -> CompilerConfig)
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

-- | Load configuration from multiple sources with precedence:
-- 1. Command line arguments (highest)
-- 2. Environment variables
-- 3. Configuration file
-- 4. Defaults (lowest)
loadConfig :: [String] -> IO (Either String LoadConfigResult)
loadConfig args = do
  -- Start with default config
  let baseConfig = defaultConfig
  
  -- Load from config file if it exists
  configFromFile <- loadConfigFromFile "fluxus.yaml"
  let configWithFile = case configFromFile of
        Left _ -> baseConfig
        Right cfg -> mergeConfigs baseConfig cfg
  
  -- Apply environment variable overrides
  configWithEnv <- applyEnvironmentOverrides configWithFile
  
  -- Apply command line arguments (highest priority)
  case parseCommandLineArgs args of
    Left err -> pure $ Left err
    Right CLICommandShowHelp -> pure $ Right LoadConfigHelp
    Right (CLICommandShowVersion versionText) ->
      pure $ Right (LoadConfigVersion versionText)
    Right (CLICommandModify cliModifier) -> do
      let finalConfig = cliModifier configWithEnv
      pure $ Right (LoadConfigSuccess finalConfig)

-- | Load configuration from YAML file
loadConfigFromFile :: FilePath -> IO (Either String CompilerConfig)
loadConfigFromFile configFile = do
  exists <- doesFileExist configFile
  if not exists
    then return $ Left $ "Configuration file not found: " ++ configFile
    else do
      result <- decodeFileEither configFile
      case result of
        Left err -> return $ Left $ "Failed to parse config file: " ++ show err
        Right config -> return $ Right config

-- | Parse command line arguments to compiler configuration modifiers or
-- informational CLI commands.
parseCommandLineArgs :: [String] -> Either String CLICommand
parseCommandLineArgs args = go id args
  where
    go modifier [] = Right (CLICommandModify modifier)
    go _ ("--help":_) = Right CLICommandShowHelp
    go _ ("--version":_) = Right (CLICommandShowVersion fluxusVersionString)
    go modifier (arg:rest) = case arg of
      "--python" -> set (\cfg -> cfg { ccSourceLanguage = Python }) rest
      "--go" -> set (\cfg -> cfg { ccSourceLanguage = Go }) rest
      
      "-O0" -> set (\cfg -> cfg { ccOptimizationLevel = O0 }) rest
      "-O1" -> set (\cfg -> cfg { ccOptimizationLevel = O1 }) rest
      "-O2" -> set (\cfg -> cfg { ccOptimizationLevel = O2 }) rest
      "-O3" -> set (\cfg -> cfg { ccOptimizationLevel = O3 }) rest
      "-Os" -> set (\cfg -> cfg { ccOptimizationLevel = Os }) rest
      
      "--enable-interop" -> set (\cfg -> cfg { ccEnableInterop = True }) rest
      "--disable-interop" -> set (\cfg -> cfg { ccEnableInterop = False }) rest
      
      "--enable-debug" -> set (\cfg -> cfg { ccEnableDebugInfo = True }) rest
      "--disable-debug" -> set (\cfg -> cfg { ccEnableDebugInfo = False }) rest
      
      "--enable-profiler" -> set (\cfg -> cfg { ccEnableProfiler = True }) rest
      "--disable-profiler" -> set (\cfg -> cfg { ccEnableProfiler = False }) rest
      
      "--enable-parallel" -> set (\cfg -> cfg { ccEnableParallel = True }) rest
      "--disable-parallel" -> set (\cfg -> cfg { ccEnableParallel = False }) rest
      
      "--strict" -> set (\cfg -> cfg { ccStrictMode = True }) rest
      "--no-strict" -> set (\cfg -> cfg { ccStrictMode = False }) rest
      
      "--keep-intermediates" -> set (\cfg -> cfg { ccKeepIntermediates = True }) rest
      "--clean-intermediates" -> set (\cfg -> cfg { ccKeepIntermediates = False }) rest
      
      "--skip-compiler-check" -> set (\cfg -> cfg { ccSkipCompilerCheck = True }) rest
      "--require-compiler-check" -> set (\cfg -> cfg { ccSkipCompilerCheck = False }) rest
      
      "-v" -> set (\cfg -> cfg { ccVerboseLevel = ccVerboseLevel cfg + 1 }) rest
      "--verbose" -> set (\cfg -> cfg { ccVerboseLevel = ccVerboseLevel cfg + 1 }) rest
      "--quiet" -> set (\cfg -> cfg { ccVerboseLevel = 0 }) rest
      
      "-o" -> case rest of
        (output:rest') -> set (\cfg -> cfg { ccOutputPath = Just output }) rest'
        [] -> Left "Expected output path after -o"
      
      "--output" -> case rest of
        (output:rest') -> set (\cfg -> cfg { ccOutputPath = Just output }) rest'
        [] -> Left "Expected output path after --output"
      
      "--work-dir" -> case rest of
        (dir:rest') -> set (\cfg -> cfg { ccWorkDirectory = Just dir }) rest'
        [] -> Left "Expected directory path after --work-dir"
      
      "--cpp-std" -> case rest of
        (std:rest') -> set (\cfg -> cfg { ccCppStandard = T.pack std }) rest'
        [] -> Left "Expected C++ standard after --cpp-std"
      
      "--cpp-compiler" -> case rest of
        (compiler:rest') -> set (\cfg -> cfg { ccCppCompiler = T.pack compiler }) rest'
        [] -> Left "Expected compiler path after --cpp-compiler"
      
      "--max-concurrency" -> case rest of
        (n:rest') -> case reads n of
          [(num, "")] -> set (\cfg -> cfg { ccMaxConcurrency = num }) rest'
          _ -> Left "Invalid number for --max-concurrency"
        [] -> Left "Expected number after --max-concurrency"
      
      "--include" -> case rest of
        (path:rest') -> set (\cfg -> cfg { ccIncludePaths = path : ccIncludePaths cfg }) rest'
        [] -> Left "Expected path after --include"
      
      "--library-path" -> case rest of
        (path:rest') -> set (\cfg -> cfg { ccLibraryPaths = path : ccLibraryPaths cfg }) rest'
        [] -> Left "Expected path after --library-path"
      
      "--link" -> case rest of
        (lib:rest') -> set (\cfg -> cfg { ccLinkedLibraries = T.pack lib : ccLinkedLibraries cfg }) rest'
        [] -> Left "Expected library name after --link"
      
      "--target" -> case rest of
        (target:rest') -> case parseTargetPlatform target of
          Just platform -> set (\cfg -> cfg { ccTargetPlatform = platform }) rest'
          Nothing -> Left $ "Unknown target platform: " ++ target
        [] -> Left "Expected target platform after --target"
      
      _ | "--" `isPrefixOf` arg ->
            Left $ "Unknown option: " ++ arg ++ ". Use --help to see available options."
      _ -> go modifier rest  -- Assume it's an input file
      where
        set modifyFn remaining = go (modifyFn . modifier) remaining

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
mergeConfigs :: CompilerConfig -> CompilerConfig -> CompilerConfig
mergeConfigs base override = CompilerConfig
  { ccSourceLanguage = ccSourceLanguage override
  , ccOptimizationLevel = ccOptimizationLevel override
  , ccTargetPlatform = ccTargetPlatform override
  , ccOutputPath = ccOutputPath override <|> ccOutputPath base
  , ccEnableInterop = ccEnableInterop override
  , ccEnableDebugInfo = ccEnableDebugInfo override
  , ccEnableProfiler = ccEnableProfiler override
  , ccEnableParallel = ccEnableParallel override
  , ccMaxConcurrency = ccMaxConcurrency override
  , ccIncludePaths = ccIncludePaths override ++ ccIncludePaths base
  , ccLibraryPaths = ccLibraryPaths override ++ ccLibraryPaths base
  , ccLinkedLibraries = ccLinkedLibraries override ++ ccLinkedLibraries base
  , ccCppStandard = ccCppStandard override
  , ccCppCompiler = ccCppCompiler override
  , ccVerboseLevel = ccVerboseLevel override
  , ccWorkDirectory = ccWorkDirectory override <|> ccWorkDirectory base
  , ccKeepIntermediates = ccKeepIntermediates override
  , ccStrictMode = ccStrictMode override
  , ccEnableAnalysis = ccEnableAnalysis override
  , ccStopAtCodegen = ccStopAtCodegen override
  , ccSkipCompilerCheck = ccSkipCompilerCheck override
  }

-- | Apply environment variable overrides
applyEnvironmentOverrides :: CompilerConfig -> IO CompilerConfig
applyEnvironmentOverrides config = do
  -- Check for common environment variables
  cppCompiler <- lookupEnv "CXX"
  cppStd <- lookupEnv "FLUXUS_CPP_STD"
  verboseLevel <- lookupEnv "FLUXUS_VERBOSE"
  enableInterop <- lookupEnv "FLUXUS_INTEROP"
  skipCompilerCheck <- lookupEnv "FLUXUS_SKIP_COMPILER_CHECK"
  
  resolvedVerboseLevel <- case verboseLevel of
    Nothing -> pure (ccVerboseLevel config)
    Just raw -> case readMaybe raw of
      Just level -> pure level
      Nothing -> do
        putStrLn $ "Warning: Ignoring invalid FLUXUS_VERBOSE value '" ++ raw ++ "' (keeping existing verbosity)."
        pure (ccVerboseLevel config)

  return config
    { ccCppCompiler = maybe (ccCppCompiler config) T.pack cppCompiler
    , ccCppStandard = maybe (ccCppStandard config) T.pack cppStd
    , ccVerboseLevel = resolvedVerboseLevel
    , ccEnableInterop = parseBoolOverride (ccEnableInterop config) enableInterop
    , ccSkipCompilerCheck = parseBoolOverride (ccSkipCompilerCheck config) skipCompilerCheck
    }
  where
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
    Left err -> return $ Left err
    Right _ -> return $ Right ()

-- | Check if system meets requirements for compilation
checkSystemRequirements :: CompilerConfig -> IO (Either String ())
checkSystemRequirements config = do
  let skipCompilerCheck = ccSkipCompilerCheck config
      stopAtCodegen = ccStopAtCodegen config
      shouldCheckCompiler = not skipCompilerCheck && not stopAtCodegen
  
  when skipCompilerCheck $
    putStrLn "Warning: Skipping C++ compiler requirement check (ccSkipCompilerCheck enabled)"
  when (stopAtCodegen && not skipCompilerCheck) $
    putStrLn "Skipping C++ compiler requirement check because stop-at-codegen is enabled"
  
  compilerCheckResult <-
    if shouldCheckCompiler
      then do
        maybeCompiler <- locateCompiler (T.unpack $ ccCppCompiler config)
        pure $ case maybeCompiler of
          Nothing -> Left $ "C++ compiler not found: " ++ T.unpack (ccCppCompiler config) ++ " (use --skip-compiler-check to bypass detection)"
          Just _ -> Right ()
      else pure (Right ())
  
  case compilerCheckResult of
    Left err -> pure $ Left err
    Right () -> do
      mapM_ checkIncludePath (ccIncludePaths config)
      mapM_ checkLibraryPath (ccLibraryPaths config)
      pure $ Right ()
  where
    locateCompiler compilerBinary = do
      directExists <- doesFileExist compilerBinary
      if directExists
        then pure (Just compilerBinary)
        else findExecutable compilerBinary
    checkIncludePath path = do
      exists <- doesDirectoryExist path
      unless exists $ putStrLn $ "Warning: Include path does not exist: " ++ path
    checkLibraryPath path = do
      exists <- doesDirectoryExist path
      unless exists $ putStrLn $ "Warning: Library path does not exist: " ++ path

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
  putStrLn $ "Static Analysis: " ++ show (ccEnableAnalysis config)
  putStrLn "=============================================="


