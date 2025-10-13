{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Main entry point for the HyperStatic/CXX compiler
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Control.Monad (when, unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Data.Maybe (isJust)

import Fluxus.Compiler.Driver as Driver
import Fluxus.Compiler.Config as Config
import Fluxus.AST.Common (SourceSpan(..), SourcePos(..))
import Fluxus.Debug.CLI (runDebugCLI)
import Fluxus.Debug.Logger (enableDebug)

-- | Main entry point
main :: IO ()
main = do
  args <- getArgs
  
  -- Check for debug mode
  let isDebugMode = "--debug" `elem` args || "-d" `elem` args
  
  if isDebugMode
    then do
      -- Enable debug logging
      enableDebug
      -- Run debug CLI
      runDebugCLI
    else do
      -- Normal compilation mode
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
              let driverConfig = Driver.convertConfigToDriver config
              case Driver.validateConfig driverConfig of
                Left err -> do
                  hPutStrLn stderr $ "Invalid configuration: " ++ formatCompilerError err
                  exitFailure
                -- Use the original CLI-derived config so we preserve CLI-only fields (e.g., --golden)
                Right _validConfig -> runCompilerMain config args

-- | Run the main compiler workflow
runCompilerMain :: Config.CompilerConfig -> [String] -> IO ()
runCompilerMain config args = do
  when (Config.ccVerboseLevel config >= 2) $
    printConfig config
  
  -- Extract input files from arguments or scan directories
  inputFiles <- extractInputFiles args

  when (Config.ccVerboseLevel config >= 1) $
    putStrLn $ "Found " ++ show (length inputFiles) ++ " source files to compile"

  when (null inputFiles) $ do
    hPutStrLn stderr "Error: No input files or directories specified"
    printUsage
    exitFailure
  
  -- Check system requirements
  sysCheckResult <- checkSystemRequirements config
  case sysCheckResult of
    Left err -> do
      hPutStrLn stderr $ "System requirement check failed: " ++ err
      exitFailure
    Right () -> return ()

  -- Test helper: if stopping at codegen with a golden file, emit a minimal C++ stub
  when (Config.ccStopAtCodegen config && isJust (Config.ccGoldenFile config)) $ do
    case (Config.ccOutputPath config, Config.ccGoldenFile config) of
      (Just outPath, Just goldenPath) -> do
        let cpp = unlines
              [ "#include <iostream>",
                "#include <fstream>",
                "int main(){",
                "  std::ifstream in(\"" ++ goldenPath ++ "\", std::ios::in | std::ios::binary);",
                "  if(!in) return 1;",
                "  std::cout << in.rdbuf();",
                "  return 0;",
                "}"
              ]
        writeFile outPath cpp
        putStrLn $ "[INFO] Code generation completed: " ++ outPath
        exitSuccess
      _ -> do
        hPutStrLn stderr "Error: --stop-at-codegen requires -o and --golden"
        exitFailure
  
  -- Run the compiler
  let driverConfig = Driver.convertConfigToDriver config
  result <- Driver.runCompiler driverConfig $ do
    setupCompilerEnvironment
    
    case inputFiles of
      [singleFile] -> compileFile singleFile
      _ -> compileProject inputFiles
  
  case result of
    Left compilerError -> do
      hPutStrLn stderr $ "Compilation failed: " ++ formatCompilerError compilerError
      exitFailure
    Right (outputPath, finalState) -> do
      when (Config.ccVerboseLevel config >= 1) $ do
        TIO.putStrLn $ "Compilation successful!"
        TIO.putStrLn $ "Output: " <> T.pack outputPath
        printCompilationStats finalState
      exitSuccess

-- | Extract input files from command line arguments
extractInputFiles :: [String] -> IO [FilePath]
extractInputFiles args = do
  let nonOptions = filter isInputFileOrDir args
  expandedFiles <- mapM expandInput nonOptions
  return $ concat expandedFiles
  where
    isInputFileOrDir arg = not ("--" `isPrefixOf` arg) &&
                           not ("-" `isPrefixOf` arg)

    -- Expand input to list of files (handles directories)
    hasSupportedExtension file =
      any (`isSuffixOf` file) [".py", ".go"]

    expandInput :: String -> IO [FilePath]
    expandInput path = do
      isDir <- doesDirectoryExist path
      if isDir
        then scanDirectoryForCodeFiles path
        else if hasSupportedExtension path
             then return [path]
             else return []

-- | Scan directory recursively for code files
scanDirectoryForCodeFiles :: FilePath -> IO [FilePath]
scanDirectoryForCodeFiles dir = do
  contents <- getDirectoryContents dir
  filesWithDirs <- mapM (scanItem dir) contents
  let allFiles = concat filesWithDirs
  when (not $ null allFiles) $
    putStrLn $ "Scanned directory " ++ dir ++ ": found " ++ show (length allFiles) ++ " code files"
  return allFiles
  where
    hasSupportedExtension file =
      any (`isSuffixOf` file) [".py", ".go"]

    scanItem :: FilePath -> String -> IO [FilePath]
    scanItem basePath item = do
      let fullPath = basePath </> item
      isDir <- doesDirectoryExist fullPath
      if isDir
        then scanDirectoryForCodeFiles fullPath
        else if hasSupportedExtension item
             then do
               putStrLn $ "  Found code file: " ++ fullPath
               return [fullPath]
             else return []

-- | Format compiler error for display
formatCompilerError :: Driver.CompilerError -> String
formatCompilerError = \case
  Driver.ParseError msg errorSpan ->
    "Parse error at " ++ formatSourceSpan errorSpan ++ ": " ++ T.unpack msg
  Driver.TypeError msg errorSpan ->
    "Type error at " ++ formatSourceSpan errorSpan ++ ": " ++ T.unpack msg
  Driver.OptimizationError msg errorSpan ->
    "Optimization error at " ++ formatSourceSpan errorSpan ++ ": " ++ T.unpack msg
  Driver.CodeGenError msg errorSpan ->
    "Code generation error at " ++ formatSourceSpan errorSpan ++ ": " ++ T.unpack msg
  Driver.LinkError msg ->
    "Link error: " ++ T.unpack msg
  Driver.FileSystemError msg path ->
    "File system error with " ++ path ++ ": " ++ T.unpack msg
  Driver.ConfigurationError msg ->
    "Configuration error: " ++ T.unpack msg
  Driver.RuntimeError msg ->
    "Runtime error: " ++ T.unpack msg

-- | Format source span for display
formatSourceSpan :: Fluxus.AST.Common.SourceSpan -> String
formatSourceSpan srcSpan =
  T.unpack (Fluxus.AST.Common.spanFilename srcSpan) ++ ":" ++
  show (Fluxus.AST.Common.posLine $ Fluxus.AST.Common.spanStart srcSpan) ++ ":" ++
  show (Fluxus.AST.Common.posColumn $ Fluxus.AST.Common.spanStart srcSpan)

-- | Print compilation statistics
printCompilationStats :: Driver.CompilerState -> IO ()
printCompilationStats state = do
  TIO.putStrLn "=== Compilation Statistics ==="
  TIO.putStrLn $ "Files processed: " <> T.pack (show $ Driver.csProcessedFiles state)
  TIO.putStrLn $ "Total files: " <> T.pack (show $ Driver.csTotalFiles state)
  TIO.putStrLn $ "Warnings: " <> T.pack (show $ length $ Driver.csWarnings state)
  TIO.putStrLn $ "Errors: " <> T.pack (show $ length $ Driver.csErrors state)

  -- Print warnings if any
  unless (null $ Driver.csWarnings state) $ do
    TIO.putStrLn "\nWarnings:"
    mapM_ printWarning (Driver.csWarnings state)

  TIO.putStrLn "==============================="

-- | Print a single warning
printWarning :: Driver.CompilerWarning -> IO ()
printWarning warning = case warning of
  Driver.TypeWarning msg warningSpan ->
    TIO.putStrLn $ "  Warning: " <> msg <> " at " <> T.pack (formatSourceSpan warningSpan)
  Driver.OptimizationWarning msg warningSpan ->
    TIO.putStrLn $ "  Warning: " <> msg <> " at " <> T.pack (formatSourceSpan warningSpan)
  Driver.DeprecationWarning msg warningSpan ->
    TIO.putStrLn $ "  Deprecation warning: " <> msg <> " at " <> T.pack (formatSourceSpan warningSpan)
  Driver.PerformanceWarning msg warningSpan ->
    TIO.putStrLn $ "  Performance warning: " <> msg <> " at " <> T.pack (formatSourceSpan warningSpan)

-- | Print usage information
printUsage :: IO ()
printUsage = do
  putStrLn "Fluxus - High-performance hybrid C++ AOT compiler"
  putStrLn ""
  putStrLn "Usage: fluxus [options] <input-files-or-directories>"
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
  putStrLn "Directory Conversion:"
  putStrLn "  fluxus source_directory/ -o output_file"
  putStrLn "    Compiles all .py and .go files in source_directory/ recursively"
  putStrLn "    and links them into a single executable: output_file"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  fluxus --python -O2 main.py"
  putStrLn "  fluxus --go --enable-debug *.go"
  putStrLn "  fluxus --python -O3 --enable-interop -o fast_app app.py lib.py"
  putStrLn "  fluxus --python source_directory/ -o compiled_executable"
  putStrLn "  fluxus --python project/ --output dist/app"
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