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
  , parseCommandLineArgs
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
import System.Process (readProcessWithExitCode)
import System.Exit
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import GHC.Generics (Generic)

import Fluxus.Compiler.Driver

-- | Load configuration from multiple sources with precedence:
-- 1. Command line arguments (highest)
-- 2. Environment variables
-- 3. Configuration file
-- 4. Defaults (lowest)
loadConfig :: [String] -> IO (Either String CompilerConfig)
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
    Left err -> return $ Left err
    Right cliConfig -> do
      let finalConfig = mergeConfigs configWithEnv cliConfig
      return $ Right finalConfig

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

-- | Parse command line arguments to compiler configuration
parseCommandLineArgs :: [String] -> Either String CompilerConfig
parseCommandLineArgs args = parseArgs defaultConfig args
  where
    parseArgs config [] = Right config
    parseArgs config (arg:rest) = case arg of
      "--python" -> parseArgs (config { ccSourceLanguage = Python }) rest
      "--go" -> parseArgs (config { ccSourceLanguage = Go }) rest
      
      "-O0" -> parseArgs (config { ccOptimizationLevel = O0 }) rest
      "-O1" -> parseArgs (config { ccOptimizationLevel = O1 }) rest
      "-O2" -> parseArgs (config { ccOptimizationLevel = O2 }) rest
      "-O3" -> parseArgs (config { ccOptimizationLevel = O3 }) rest
      "-Os" -> parseArgs (config { ccOptimizationLevel = Os }) rest
      
      "--enable-interop" -> parseArgs (config { ccEnableInterop = True }) rest
      "--disable-interop" -> parseArgs (config { ccEnableInterop = False }) rest
      
      "--enable-debug" -> parseArgs (config { ccEnableDebugInfo = True }) rest
      "--disable-debug" -> parseArgs (config { ccEnableDebugInfo = False }) rest
      
      "--enable-profiler" -> parseArgs (config { ccEnableProfiler = True }) rest
      "--disable-profiler" -> parseArgs (config { ccEnableProfiler = False }) rest
      
      "--enable-parallel" -> parseArgs (config { ccEnableParallel = True }) rest
      "--disable-parallel" -> parseArgs (config { ccEnableParallel = False }) rest
      
      "--strict" -> parseArgs (config { ccStrictMode = True }) rest
      "--no-strict" -> parseArgs (config { ccStrictMode = False }) rest
      
      "--keep-intermediates" -> parseArgs (config { ccKeepIntermediates = True }) rest
      "--clean-intermediates" -> parseArgs (config { ccKeepIntermediates = False }) rest
      
      "-v" -> parseArgs (config { ccVerboseLevel = ccVerboseLevel config + 1 }) rest
      "--verbose" -> parseArgs (config { ccVerboseLevel = ccVerboseLevel config + 1 }) rest
      "--quiet" -> parseArgs (config { ccVerboseLevel = 0 }) rest
      
      "-o" -> case rest of
        (output:rest') -> parseArgs (config { ccOutputPath = Just output }) rest'
        [] -> Left "Expected output path after -o"
      
      "--output" -> case rest of
        (output:rest') -> parseArgs (config { ccOutputPath = Just output }) rest'
        [] -> Left "Expected output path after --output"
      
      "--work-dir" -> case rest of
        (dir:rest') -> parseArgs (config { ccWorkDirectory = Just dir }) rest'
        [] -> Left "Expected directory path after --work-dir"
      
      "--cpp-std" -> case rest of
        (std:rest') -> parseArgs (config { ccCppStandard = T.pack std }) rest'
        [] -> Left "Expected C++ standard after --cpp-std"
      
      "--cpp-compiler" -> case rest of
        (compiler:rest') -> parseArgs (config { ccCppCompiler = T.pack compiler }) rest'
        [] -> Left "Expected compiler path after --cpp-compiler"
      
      "--max-concurrency" -> case rest of
        (n:rest') -> case reads n of
          [(num, "")] -> parseArgs (config { ccMaxConcurrency = num }) rest'
          _ -> Left "Invalid number for --max-concurrency"
        [] -> Left "Expected number after --max-concurrency"
      
      "--include" -> case rest of
        (path:rest') -> parseArgs (config { ccIncludePaths = path : ccIncludePaths config }) rest'
        [] -> Left "Expected path after --include"
      
      "--library-path" -> case rest of
        (path:rest') -> parseArgs (config { ccLibraryPaths = path : ccLibraryPaths config }) rest'
        [] -> Left "Expected path after --library-path"
      
      "--link" -> case rest of
        (lib:rest') -> parseArgs (config { ccLinkedLibraries = T.pack lib : ccLinkedLibraries config }) rest'
        [] -> Left "Expected library name after --link"
      
      "--target" -> case rest of
        (target:rest') -> case parseTargetPlatform target of
          Just platform -> parseArgs (config { ccTargetPlatform = platform }) rest'
          Nothing -> Left $ "Unknown target platform: " ++ target
        [] -> Left "Expected target platform after --target"
      
      "--help" -> Left "Usage: fluxus [options] <input-files>"
      "--version" -> Left "Fluxus Compiler v0.1.0"
      
      _ | "--" `isPrefixOf` arg -> Left $ "Unknown option: " ++ arg
      _ -> parseArgs config rest  -- Assume it's an input file

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
  , ccVerboseLevel = max (ccVerboseLevel base) (ccVerboseLevel override)
  , ccWorkDirectory = ccWorkDirectory override <|> ccWorkDirectory base
  , ccKeepIntermediates = ccKeepIntermediates override
  , ccStrictMode = ccStrictMode override
  , ccEnableAnalysis = ccEnableAnalysis override
  , ccStopAtCodegen = ccStopAtCodegen override
  }

-- | Apply environment variable overrides
applyEnvironmentOverrides :: CompilerConfig -> IO CompilerConfig
applyEnvironmentOverrides config = do
  -- Check for common environment variables
  cppCompiler <- lookupEnv "CXX"
  cppStd <- lookupEnv "FLUXUS_CPP_STD"
  verboseLevel <- lookupEnv "FLUXUS_VERBOSE"
  enableInterop <- lookupEnv "FLUXUS_INTEROP"
  
  return config
    { ccCppCompiler = maybe (ccCppCompiler config) T.pack cppCompiler
    , ccCppStandard = maybe (ccCppStandard config) T.pack cppStd
    , ccVerboseLevel = maybe (ccVerboseLevel config) (fromMaybe 0 . readMaybe) verboseLevel
    , ccEnableInterop = maybe (ccEnableInterop config) (== "1") enableInterop
    }
  where
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

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
  -- Check C++ compiler
  compilerExists <- doesFileExist (T.unpack $ ccCppCompiler config)
  if compilerExists
    then do
      -- Check include paths
      mapM_ checkIncludePath (ccIncludePaths config)
      -- Check library paths
      mapM_ checkLibraryPath (ccLibraryPaths config)
      return $ Right ()
    else do
      -- Try to find in PATH
      which <- readProcessWithExitCode "which" [T.unpack $ ccCppCompiler config] ""
      case which of
        (ExitSuccess, _, _) -> do
          -- Check include paths
          mapM_ checkIncludePath (ccIncludePaths config)
          -- Check library paths
          mapM_ checkLibraryPath (ccLibraryPaths config)
          return $ Right ()
        _ -> return $ Left $ "C++ compiler not found: " ++ T.unpack (ccCppCompiler config)
  where
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

-- | Convert target platform to string
showTargetPlatform :: TargetPlatform -> String
showTargetPlatform = \case
  Linux_x86_64 -> "linux-x86_64"
  Linux_ARM64 -> "linux-arm64"
  Darwin_x86_64 -> "darwin-x86_64"
  Darwin_ARM64 -> "darwin-arm64"
  Windows_x86_64 -> "windows-x86_64"

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
  putStrLn $ "Static Analysis: " ++ show (ccEnableAnalysis config)
  putStrLn "=============================================="

-- | JSON instances for configuration
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
    targetPlatform <- o .:? "target_platform" .!= ("linux-x86_64" :: String)
    
    CompilerConfig
      <$> pure (parseSourceLanguage sourceLang)
      <*> pure (parseOptLevel optLevel)
      <*> pure (fromMaybe Linux_x86_64 (parseTargetPlatform targetPlatform))
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
    where
      parseSourceLanguage "Python" = Python
      parseSourceLanguage "Go" = Go
      parseSourceLanguage _ = Python
      
      parseOptLevel "O0" = O0
      parseOptLevel "O1" = O1
      parseOptLevel "O2" = O2
      parseOptLevel "O3" = O3
      parseOptLevel "Os" = Os
      parseOptLevel _ = O2