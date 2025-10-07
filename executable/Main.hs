{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Main entry point for the HyperStatic/CXX compiler
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Control.Monad (when, unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (isPrefixOf, isSuffixOf)

import CLI.Driver
import CLI.Config
import AST.Common (SourceSpan(..), SourcePos(..))

-- | Main entry point
main :: IO ()
main = do
  args <- getArgs
  
  if null args
    then printUsage >> exitFailure
    else do
      -- Parse configuration from command line and config files
      configResult <- loadConfig args
      case configResult of
        Left err -> do
          hPutStrLn stderr $ "Configuration error: " ++ err
          exitFailure
        Right config -> do
          -- Validate configuration
          case validateConfig config of
            Left (ConfigurationError msg) -> do
              hPutStrLn stderr $ "Invalid configuration: " ++ T.unpack msg
              exitFailure
            Right validConfig -> runCompilerMain validConfig args

-- | Run the main compiler workflow
runCompilerMain :: CompilerConfig -> [String] -> IO ()
runCompilerMain config args = do
  when (ccVerboseLevel config >= 2) $
    printConfig config
  
  -- Extract input files from arguments
  let inputFiles = extractInputFiles args
  
  when (null inputFiles) $ do
    hPutStrLn stderr "Error: No input files specified"
    printUsage
    exitFailure
  
  -- Check system requirements
  sysCheckResult <- checkSystemRequirements config
  case sysCheckResult of
    Left err -> do
      hPutStrLn stderr $ "System requirement check failed: " ++ err
      exitFailure
    Right () -> return ()
  
  -- Run the compiler
  result <- runCompiler config $ do
    setupCompilerEnvironment
    
    case inputFiles of
      [singleFile] -> compileFile singleFile
      _ -> compileProject inputFiles
  
  case result of
    Left compilerError -> do
      hPutStrLn stderr $ "Compilation failed: " ++ formatCompilerError compilerError
      exitFailure
    Right (outputPath, finalState) -> do
      when (ccVerboseLevel config >= 1) $ do
        TIO.putStrLn $ "Compilation successful!"
        TIO.putStrLn $ "Output: " <> T.pack outputPath
        printCompilationStats finalState
      exitSuccess

-- | Extract input files from command line arguments
extractInputFiles :: [String] -> [FilePath]
extractInputFiles = filter isInputFile
  where
    isInputFile arg = not ("--" `isPrefixOf` arg) && 
                     not ("-" `isPrefixOf` arg) &&
                     (hasSupportedExtension arg)
    
    hasSupportedExtension file = 
      any (`isSuffixOf` file) [".py", ".go"]

-- | Format compiler error for display
formatCompilerError :: CompilerError -> String
formatCompilerError = \case
  ParseError msg span -> 
    "Parse error at " ++ formatSourceSpan span ++ ": " ++ T.unpack msg
  TypeError msg span -> 
    "Type error at " ++ formatSourceSpan span ++ ": " ++ T.unpack msg
  OptimizationError msg -> 
    "Optimization error: " ++ T.unpack msg
  CodeGenError msg -> 
    "Code generation error: " ++ T.unpack msg
  LinkError msg -> 
    "Link error: " ++ T.unpack msg
  FileSystemError msg path -> 
    "File system error with " ++ path ++ ": " ++ T.unpack msg
  ConfigurationError msg -> 
    "Configuration error: " ++ T.unpack msg
  RuntimeError msg -> 
    "Runtime error: " ++ T.unpack msg

-- | Format source span for display
formatSourceSpan :: SourceSpan -> String
formatSourceSpan span = 
  T.unpack (spanFilename span) ++ ":" ++ 
  show (posLine $ spanStart span) ++ ":" ++
  show (posColumn $ spanStart span)

-- | Print compilation statistics
printCompilationStats :: CompilerState -> IO ()
printCompilationStats state = do
  TIO.putStrLn "=== Compilation Statistics ==="
  TIO.putStrLn $ "Files processed: " <> T.pack (show $ csProcessedFiles state)
  TIO.putStrLn $ "Total files: " <> T.pack (show $ csTotalFiles state)
  TIO.putStrLn $ "Warnings: " <> T.pack (show $ length $ csWarnings state)
  TIO.putStrLn $ "Errors: " <> T.pack (show $ length $ csErrors state)
  
  -- Print warnings if any
  unless (null $ csWarnings state) $ do
    TIO.putStrLn "\nWarnings:"
    mapM_ printWarning (csWarnings state)
  
  TIO.putStrLn "==============================="

-- | Print a single warning
printWarning :: CompilerWarning -> IO ()
printWarning warning = case warning of
  TypeWarning msg span -> 
    TIO.putStrLn $ "  Warning: " <> msg <> " at " <> T.pack (formatSourceSpan span)
  OptimizationWarning msg -> 
    TIO.putStrLn $ "  Warning: " <> msg
  DeprecationWarning msg span -> 
    TIO.putStrLn $ "  Deprecation warning: " <> msg <> " at " <> T.pack (formatSourceSpan span)
  PerformanceWarning msg span -> 
    TIO.putStrLn $ "  Performance warning: " <> msg <> " at " <> T.pack (formatSourceSpan span)

-- | Print usage information
printUsage :: IO ()
printUsage = do
  putStrLn "Fluxus - High-performance hybrid C++ AOT compiler"
  putStrLn ""
  putStrLn "Usage: fluxus [options] <input-files>"
  putStrLn ""
  putStrLn "Source Language Options:"
  putStrLn "  --python              Compile Python source (default)"
  putStrLn "  --go                  Compile Go source"
  putStrLn ""
  putStrLn "Optimization Options:"
  putStrLn "  -O0                   No optimization (fast compilation)"
  putStrLn "  -O1                   Basic optimizations"
  putStrLn "  -O2                   Standard optimizations (default)"
  putStrLn "  -O3                   Aggressive optimizations"
  putStrLn "  -Os                   Size optimizations"
  putStrLn ""
  putStrLn "Target Options:"
  putStrLn "  --target PLATFORM     Target platform (linux-x86_64, darwin-x86_64, etc.)"
  putStrLn "  -o FILE               Output file path"
  putStrLn ""
  putStrLn "Feature Options:"
  putStrLn "  --enable-interop      Enable runtime interoperability (default)"
  putStrLn "  --disable-interop     Disable runtime interoperability"
  putStrLn "  --enable-debug        Include debug information"
  putStrLn "  --disable-debug       Exclude debug information (default)"
  putStrLn "  --enable-profiler     Enable profiling support"
  putStrLn "  --disable-profiler    Disable profiling support (default)"
  putStrLn "  --enable-parallel     Enable parallel execution (default)"
  putStrLn "  --disable-parallel    Disable parallel execution"
  putStrLn ""
  putStrLn "Compilation Options:"
  putStrLn "  --cpp-std STD         C++ standard (c++20, c++23) (default: c++20)"
  putStrLn "  --cpp-compiler CC     C++ compiler to use (default: clang++)"
  putStrLn "  --max-concurrency N   Maximum compilation concurrency (default: 4)"
  putStrLn "  --include PATH        Add include path"
  putStrLn "  --library-path PATH   Add library path"
  putStrLn "  --link LIB            Link with library"
  putStrLn ""
  putStrLn "Build Options:"
  putStrLn "  --work-dir DIR        Work directory for intermediate files"
  putStrLn "  --keep-intermediates  Keep intermediate files"
  putStrLn "  --clean-intermediates Remove intermediate files (default)"
  putStrLn "  --strict              Treat warnings as errors"
  putStrLn "  --no-strict           Allow warnings (default)"
  putStrLn ""
  putStrLn "Output Options:"
  putStrLn "  -v, --verbose         Increase verbosity level"
  putStrLn "  --quiet              Suppress all output except errors"
  putStrLn ""
  putStrLn "Information Options:"
  putStrLn "  --help               Show this help message"
  putStrLn "  --version            Show version information"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  fluxus --python -O2 main.py"
  putStrLn "  fluxus --go --enable-debug *.go"
  putStrLn "  fluxus --python -O3 --enable-interop -o fast_app app.py lib.py"
  putStrLn ""
  putStrLn "Environment Variables:"
  putStrLn "  CXX                   C++ compiler (overrides --cpp-compiler)"
  putStrLn "  FLUXUS_CPP_STD        C++ standard (overrides --cpp-std)"
  putStrLn "  FLUXUS_VERBOSE        Verbosity level (0-3)"
  putStrLn "  FLUXUS_INTEROP        Enable interop (1/0)"
  putStrLn ""
  putStrLn "Configuration File:"
  putStrLn "  The compiler looks for 'fluxus.yaml' in the current directory"
  putStrLn "  for default configuration settings."