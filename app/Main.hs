{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Main entry point for the Fluxus compiler
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Control.Monad (when, unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (isPrefixOf, isSuffixOf)

import Fluxus.Compiler.Driver
import Fluxus.Compiler.Config
import Fluxus.AST.Common (SourceSpan(..), SourcePos(..))

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
        Right LoadConfigHelp -> do
          printUsage
          exitSuccess
        Right (LoadConfigVersion versionText) -> do
          putStrLn versionText
          exitSuccess
        Right (LoadConfigSuccess config) -> do
          -- Validate configuration
          case validateConfig config of
            Left validationError -> do
              hPutStrLn stderr $ "Invalid configuration: " ++ formatCompilerError validationError
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
  
  when (ccSourceLanguage config == Go || any (".go" `isSuffixOf`) inputFiles) $
    hPutStrLn stderr "Warning: Go backend remains experimental; translation for constructs such as switch/select and struct literals is still incomplete and may fall back to runtime aborts."
  
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
    
    if length inputFiles == 1
      then compileFile (head inputFiles)
      else compileProject inputFiles
  
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
  ParseError msg srcSpan ->
    "Parse error at " ++ formatSourceSpan srcSpan ++ ": " ++ T.unpack msg
  TypeError msg srcSpan ->
    "Type error at " ++ formatSourceSpan srcSpan ++ ": " ++ T.unpack msg
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
formatSourceSpan sourceSpan =
  T.unpack (spanFilename sourceSpan) ++ ":" ++
  show (posLine $ spanStart sourceSpan) ++ ":" ++
  show (posColumn $ spanStart sourceSpan)

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
  TypeWarning msg srcSpan ->
    TIO.putStrLn $ "  Warning: " <> msg <> " at " <> T.pack (formatSourceSpan srcSpan)
  OptimizationWarning msg ->
    TIO.putStrLn $ "  Warning: " <> msg
  DeprecationWarning msg srcSpan ->
    TIO.putStrLn $ "  Deprecation warning: " <> msg <> " at " <> T.pack (formatSourceSpan srcSpan)
  PerformanceWarning msg srcSpan ->
    TIO.putStrLn $ "  Performance warning: " <> msg <> " at " <> T.pack (formatSourceSpan srcSpan)

-- | Print usage information
printUsage :: IO ()
printUsage = do
  putStrLn "Fluxus - High-performance hybrid C++ AOT compiler"
  putStrLn ""
  putStrLn "Usage: fluxus [options] <input-files>"
  putStrLn ""
  putStrLn "Source Language Options:"
  putStrLn "  --python              Compile Python source (default)"
  putStrLn "  --go                  Compile Go source (experimental)"
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
  putStrLn "Configuration Options:"
  putStrLn "  --config FILE        Load configuration overrides from FILE (repeatable; last wins)"
  putStrLn "  -c FILE              Shorthand for --config FILE"
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
  putStrLn "  --skip-compiler-check Skip probing for the configured C++ compiler during setup"
  putStrLn "  --require-compiler-check Ensure the compiler is probed during setup (default)"
  putStrLn "  --strict              Treat warnings as errors (default)"
  putStrLn "  --no-strict           Allow warnings"
  putStrLn "  --enable-analysis     Enable static analysis passes (default)"
  putStrLn "  --disable-analysis    Disable static analysis passes"
  putStrLn "  --stop-at-codegen     Stop after generating C++ source"
  putStrLn "  --full-pipeline       Run the full compilation pipeline (default)"
  putStrLn ""
  putStrLn "Output Options:"
  putStrLn "  -v, --verbose         Increase verbosity level"
  putStrLn "  --quiet               Suppress all output except errors"
  putStrLn ""
  putStrLn "Information Options:"
  putStrLn "  --help               Show this help message"
  putStrLn "  --version            Show version information"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  fluxus --python -O2 main.py"
  putStrLn "  fluxus --go --enable-debug *.go"
  putStrLn "  fluxus --python -O3 --enable-interop -o fast_app app.py lib.py"
  putStrLn "  fluxus --config production.yaml app.py"
  putStrLn ""
  putStrLn "Environment Variables:"
  putStrLn "  CXX                   C++ compiler (overrides --cpp-compiler)"
  putStrLn "  FLUXUS_CPP_STD        C++ standard (overrides --cpp-std)"
  putStrLn "  FLUXUS_VERBOSE        Verbosity level (0-3)"
  putStrLn "  FLUXUS_INTEROP        Enable interop (1/0)"
  putStrLn ""
  putStrLn "Configuration File:"
  putStrLn "  The compiler loads 'fluxus.yaml' from the current directory when present."
  putStrLn "  Use --config FILE to select an explicit configuration file."
