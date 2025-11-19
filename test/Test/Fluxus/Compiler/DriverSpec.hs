{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Compiler.DriverSpec (spec) where

import qualified Data.Text as T
import Data.Either (isRight)
import Data.List (isInfixOf, isPrefixOf)
import Control.Exception (bracket_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Test.Hspec
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getPermissions, removeFile, setPermissions, withCurrentDirectory, Permissions(..))
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>), addTrailingPathSeparator, takeDirectory, takeExtension, replaceExtension, normalise, takeFileName)
import System.IO.Temp (withSystemTempDirectory)

import qualified Test.Fluxus.CodeGen.CPP.Shared as Shared

import Fluxus.Compiler.Driver
  ( CompilerConfig(..)
  , CompilerState(..)
  , CompilerError(..)
  , OptimizationLevel(..)
  , TargetPlatform(..)
  , compileFile
  , compileProject
  , defaultConfig
  , detectCompilerBinary
  , resolveWorkPath
  , runCompiler
  , setupCompilerEnvironment
  , showTargetPlatform
  , validateConfig
  )

spec :: Spec
spec = describe "Fluxus.Compiler.Driver" $ do
  describe "setupCompilerEnvironment" $ do
    it "fails when the configured compiler is missing by default" $ do
      let config = defaultConfig
            { ccCppCompiler = "fluxus-nonexistent-compiler"
            , ccVerboseLevel = 0
            }
      result <- runCompiler config setupCompilerEnvironment
      case result of
        Left (ConfigurationError msg) ->
          T.isInfixOf "C++ compiler not found" msg `shouldBe` True
        other -> expectationFailure $ "expected configuration error, got " ++ show other

    it "succeeds when skipping the compiler check" $ do
      let config = defaultConfig
            { ccCppCompiler = "fluxus-nonexistent-compiler"
            , ccSkipCompilerCheck = True
            , ccVerboseLevel = 0
            }
      result <- runCompiler config setupCompilerEnvironment
      result `shouldSatisfy` isRight

    it "records the configured compiler when skipping the compiler check" $ do
      let configured = "fluxus-skipped-compiler"
          config = defaultConfig
            { ccCppCompiler = configured
            , ccSkipCompilerCheck = True
            , ccVerboseLevel = 0
            }
      result <- runCompiler config setupCompilerEnvironment
      case result of
        Right (_, finalState) -> do
          csResolvedCompiler finalState `shouldBe` Just configured
          csCompilerFallback finalState `shouldBe` False
        other ->
          expectationFailure $ "expected successful setup, got " ++ show other

    it "succeeds when stopping at code generation" $ do
      let config = defaultConfig
            { ccCppCompiler = "fluxus-nonexistent-compiler"
            , ccStopAtCodegen = True
            , ccVerboseLevel = 0
            }
      result <- runCompiler config setupCompilerEnvironment
      result `shouldSatisfy` isRight

    it "falls back to an available system compiler when clang++ is unavailable" $ do
      clangExecutable <- findExecutable "clang++"
      case clangExecutable of
        Just _ -> pendingWith "clang++ is available; fallback path not exercised"
        Nothing -> do
          maybeSystem <- Shared.findCppCompiler
          case maybeSystem of
            Nothing -> pendingWith "No alternative C++ compiler detected for fallback"
            Just systemCompiler -> do
              let config = defaultConfig { ccVerboseLevel = 0 }
              result <- runCompiler config setupCompilerEnvironment
              case result of
                Left err -> expectationFailure $ "expected fallback to succeed, but got " ++ show err
                Right (_, finalState) -> do
                  csCompilerFallback finalState `shouldBe` True
                  csResolvedCompiler finalState `shouldBe` Just (T.pack systemCompiler)

    it "creates the configured work directory when it does not exist" $ do
      withSystemTempDirectory "fluxus-work-base" $ \tmpRoot -> do
        let workDir = tmpRoot </> "nested" </> "build"
            config = defaultConfig
              { ccWorkDirectory = Just workDir
              , ccSkipCompilerCheck = True
              , ccVerboseLevel = 0
              }
        doesDirectoryExist workDir `shouldReturn` False
        result <- runCompiler config setupCompilerEnvironment
        case result of
          Right _ ->
            doesDirectoryExist workDir `shouldReturn` True
          Left err ->
            expectationFailure $ "expected setup to succeed, but got " ++ show err

    it "records the resolved compiler path when detection succeeds" $ do
      withFakeCompiler $ \compilerBinary _logPath -> do
        let config = defaultConfig
              { ccCppCompiler = T.pack compilerBinary
              , ccSkipCompilerCheck = False
              , ccStopAtCodegen = False
              , ccVerboseLevel = 0
              }
        result <- runCompiler config setupCompilerEnvironment
        case result of
          Right (_, finalState) -> do
            csResolvedCompiler finalState `shouldBe` Just (T.pack compilerBinary)
            csCompilerFallback finalState `shouldBe` False
          Left err ->
            expectationFailure $ "expected setup to succeed, but got " ++ show err

    it "reuses a resolved compiler across multiple environment setups" $ do
      withSystemTempDirectory "fluxus-cache-compiler" $ \tmpDir -> do
        let compilerBinary = tmpDir </> "custom-clang++"
            config = defaultConfig
              { ccCppCompiler = T.pack compilerBinary
              , ccSkipCompilerCheck = False
              , ccVerboseLevel = 0
              }
        createExecutable compilerBinary trivialCompilerScript
        result <- runCompiler config $ do
          setupCompilerEnvironment
          liftIO $ removeFile compilerBinary
          setupCompilerEnvironment
          st <- get
          pure (csResolvedCompiler st, csCompilerFallback st)
        case result of
          Right ((resolved, fallbackUsed), _) -> do
            resolved `shouldBe` Just (T.pack compilerBinary)
            fallbackUsed `shouldBe` False
          Left err ->
            expectationFailure $ "expected cached setup to succeed, but got " ++ show err

    it "creates relative work directories based on the current working directory" $ do
      withSystemTempDirectory "fluxus-relative-work" $ \tmpRoot -> do
        let relativeWorkDir = "relative-work" </> "nested" </> "build"
            config = defaultConfig
              { ccWorkDirectory = Just relativeWorkDir
              , ccSkipCompilerCheck = True
              , ccVerboseLevel = 0
              }
        withCurrentDirectory tmpRoot $ do
          doesDirectoryExist relativeWorkDir `shouldReturn` False
          result <- runCompiler config setupCompilerEnvironment
          case result of
            Right _ -> do
              doesDirectoryExist relativeWorkDir `shouldReturn` True
              doesDirectoryExist (tmpRoot </> relativeWorkDir) `shouldReturn` True
            Left err ->
              expectationFailure $ "expected setup to succeed, but got " ++ show err

  describe "resolveWorkPath" $ do
    it "keeps paths within the work directory unchanged" $ do
      withSystemTempDirectory "fluxus-work" $ \workDir -> do
        let config = defaultConfig { ccWorkDirectory = Just workDir }
            candidate = workDir </> "nested" </> "unit.cpp"
            expected = normalise candidate
        resolveWorkPath config candidate `shouldBe` expected

    it "remaps absolute paths outside the work directory into sanitized locations" $ do
      withSystemTempDirectory "fluxus-work" $ \workDir ->
        withSystemTempDirectory "fluxus-external" $ \externalDir -> do
          let config = defaultConfig { ccWorkDirectory = Just workDir }
              candidate = externalDir </> "module.cpp"
              resolved = resolveWorkPath config candidate
              expectedPrefix = addTrailingPathSeparator (normalise workDir)
              normalizedResolved = normalise resolved
          normalizedResolved `shouldSatisfy` (\path -> expectedPrefix `isPrefixOf` path)
          normalizedResolved `shouldNotBe` normalise candidate

    it "sanitizes parent directory traversal to keep outputs within the work directory" $ do
      withSystemTempDirectory "fluxus-work" $ \workDir -> do
        let config = defaultConfig { ccWorkDirectory = Just workDir }
            resolved = resolveWorkPath config ("../outside.cpp")
            expected = normalise (workDir </> "__parent__" </> "outside.cpp")
        resolved `shouldBe` expected

    it "sanitizes multi-level parent traversals to avoid escaping the work directory" $ do
      withSystemTempDirectory "fluxus-work" $ \workDir -> do
        let config = defaultConfig { ccWorkDirectory = Just workDir }
            resolved = resolveWorkPath config ("../../deep/module.cpp")
            expected = normalise (workDir </> "__parent__" </> "__parent__" </> "deep" </> "module.cpp")
        resolved `shouldBe` expected

    it "uses hashed fallback for sanitized root paths" $ do
      withSystemTempDirectory "fluxus-work" $ \workDir -> do
        let config = defaultConfig { ccWorkDirectory = Just workDir }
            resolved = resolveWorkPath config "/"
            expectedPrefix = addTrailingPathSeparator (normalise workDir)
            normalizedResolved = normalise resolved
        normalizedResolved `shouldSatisfy` (\path -> expectedPrefix `isPrefixOf` path)
        normalizedResolved `shouldNotBe` normalise "/"
        takeExtension normalizedResolved `shouldBe` ".artifact"

    it "sanitizes current directory references within candidate paths" $ do
      withSystemTempDirectory "fluxus-work" $ \workDir -> do
        let config = defaultConfig { ccWorkDirectory = Just workDir }
            expected = normalise (workDir </> "__current__")
        resolveWorkPath config "." `shouldBe` expected

    it "returns the candidate path unchanged when no work directory is configured" $ do
      let candidate = "/tmp/sample/module.cpp"
      resolveWorkPath defaultConfig candidate `shouldBe` candidate

    it "preserves the original filename when sanitizing absolute paths" $ do
      withSystemTempDirectory "fluxus-work" $ \workDir -> do
        let config = defaultConfig { ccWorkDirectory = Just workDir }
            candidate = "/opt/project/src/module.cpp"
            resolved = resolveWorkPath config candidate
            expectedPrefix = addTrailingPathSeparator (normalise workDir)
            normalizedResolved = normalise resolved
        normalizedResolved `shouldSatisfy` (\path -> expectedPrefix `isPrefixOf` path)
        takeFileName normalizedResolved `shouldBe` "module.cpp"

    it "produces identical sanitized paths for equivalent absolute inputs" $ do
      withSystemTempDirectory "fluxus-work" $ \workDir -> do
        let config = defaultConfig { ccWorkDirectory = Just workDir }
            canonical = "/tmp/sources/example.cpp"
            messy = "/tmp//sources///example.cpp"
            resolvedCanonical = resolveWorkPath config canonical
            resolvedMessy = resolveWorkPath config messy
        resolvedCanonical `shouldBe` resolvedMessy

    it "generates distinct sanitized directories for different absolute parents" $ do
      withSystemTempDirectory "fluxus-work" $ \workDir -> do
        let config = defaultConfig { ccWorkDirectory = Just workDir }
            firstPath = "/opt/project/src/main.cpp"
            secondPath = "/opt/other/src/main.cpp"
            firstResolved = resolveWorkPath config firstPath
            secondResolved = resolveWorkPath config secondPath
        firstResolved `shouldNotBe` secondResolved
        takeDirectory firstResolved `shouldNotBe` takeDirectory secondResolved
        takeFileName firstResolved `shouldBe` "main.cpp"
        takeFileName secondResolved `shouldBe` "main.cpp"

  describe "validateConfig" $ do
    it "rejects configurations without a compiler name" $ do
      let config = defaultConfig { ccCppCompiler = "" }
      validateConfig config `shouldBe` Left (ConfigurationError "C++ compiler not specified")

    it "rejects enabling debug info at O3 optimization" $ do
      let config = defaultConfig
            { ccOptimizationLevel = O3
            , ccEnableDebugInfo = True
            }
      validateConfig config `shouldBe` Left (ConfigurationError "Debug info not recommended with O3 optimization")

    it "rejects non-positive maximum concurrency values" $ do
      let config = defaultConfig { ccMaxConcurrency = 0 }
      validateConfig config `shouldBe` Left (ConfigurationError "Max concurrency must be positive")

    it "rejects enabling the interop runtime until it is implemented" $ do
      let config = defaultConfig { ccEnableInterop = True }
      validateConfig config `shouldBe`
        Left (ConfigurationError "Python/Go interop runtime is not implemented yet; remove --enable-interop or set enable_interop: false")

    it "rejects enabling static analysis while it remains disabled" $ do
      let config = defaultConfig { ccEnableAnalysis = True }
      validateConfig config `shouldBe`
        Left (ConfigurationError "Static analysis passes are experimental and currently disabled; remove --enable-analysis or set enable_analysis: false")

    it "rejects enabling experimental optimizations while analysis is unavailable" $ do
      let config = defaultConfig { ccEnableExperimentalOptimizations = True }
      validateConfig config `shouldBe`
        Left (ConfigurationError "Experimental optimizations depend on the analysis pipeline and are currently unavailable")

    it "accepts valid configuration overrides" $ do
      let config =
            defaultConfig
              { ccCppCompiler = "g++"
              , ccOptimizationLevel = O1
              , ccMaxConcurrency = 8
              }
      validateConfig config `shouldBe` Right config

  describe "detectCompilerBinary" $ do
    it "returns the configured compiler path when it exists" $ do
      withFakeCompiler $ \compilerBinary _logPath -> do
        detection <- detectCompilerBinary defaultConfig { ccCppCompiler = T.pack compilerBinary }
        detection `shouldBe` Right (T.pack compilerBinary, False)

    it "falls back to alternative compilers when the primary is missing" $ do
      withSystemTempDirectory "fluxus-detect-fallback" $ \tmpDir -> do
        let gppPath = tmpDir </> "g++"
        createExecutable gppPath trivialCompilerScript
        withTemporaryEnv "PATH" (Just tmpDir) $ do
          detection <- detectCompilerBinary defaultConfig
          case detection of
            Right (resolved, fallbackUsed) -> do
              fallbackUsed `shouldBe` True
              normalise (T.unpack resolved) `shouldBe` normalise gppPath
            Left err ->
              expectationFailure $ "expected fallback detection to succeed, but got " ++ T.unpack err

    it "reports a descriptive error when no compiler can be resolved" $ do
      withSystemTempDirectory "fluxus-detect-missing" $ \tmpDir ->
        withTemporaryEnv "PATH" (Just tmpDir) $ do
          detection <- detectCompilerBinary defaultConfig
          case detection of
            Left err -> do
              let rendered = T.unpack err
              rendered `shouldContain` "C++ compiler not found"
              rendered `shouldContain` "clang++"
            Right other ->
              expectationFailure $ "expected detection to fail, but got " ++ show other

    it "does not attempt fallback when a non-default compiler is missing" $ do
      withSystemTempDirectory "fluxus-detect-custom-missing" $ \tmpDir ->
        withTemporaryEnv "PATH" (Just tmpDir) $ do
          createExecutable (tmpDir </> "g++") trivialCompilerScript
          let config = defaultConfig { ccCppCompiler = "custom++" }
          detection <- detectCompilerBinary config
          case detection of
            Left err -> do
              let rendered = T.unpack err
              rendered `shouldContain` "custom++"
              rendered `shouldContain` "no alternative compilers were available"
            Right other ->
              expectationFailure $ "expected detection to fail without fallback, but got " ++ show other

    it "falls back to c++ when g++ is unavailable but c++ exists" $ do
      withSystemTempDirectory "fluxus-detect-cpp-only" $ \tmpDir ->
        withTemporaryEnv "PATH" (Just tmpDir) $ do
          let cppPath = tmpDir </> "c++"
          createExecutable cppPath trivialCompilerScript
          detection <- detectCompilerBinary defaultConfig
          case detection of
            Right (resolved, fallbackUsed) -> do
              fallbackUsed `shouldBe` True
              normalise (T.unpack resolved) `shouldBe` normalise cppPath
            Left err ->
              expectationFailure $ "expected fallback detection to succeed, but got " ++ T.unpack err

  describe "showTargetPlatform" $ do
    it "renders known target platforms" $ do
      showTargetPlatform Linux_x86_64 `shouldBe` "linux-x86_64"
      showTargetPlatform Linux_ARM64 `shouldBe` "linux-arm64"
      showTargetPlatform Darwin_x86_64 `shouldBe` "darwin-x86_64"
      showTargetPlatform Darwin_ARM64 `shouldBe` "darwin-arm64"
      showTargetPlatform Windows_x86_64 `shouldBe` "windows-x86_64"

  describe "compileFile" $ do
    it "emits intermediate files into the configured work directory" $ do
      withSystemTempDirectory "fluxus-work" $ \workDir ->
        withSystemTempDirectory "fluxus-src" $ \srcDir -> do
          let sourceFile = srcDir </> "example.py"
          writeFile sourceFile $ unlines
            [ "def main():"
            , "    return 42"
            ]
          let config = defaultConfig
                { ccStopAtCodegen = True
                , ccSkipCompilerCheck = True
                , ccWorkDirectory = Just workDir
                , ccVerboseLevel = 0
                }
          compileResult <- runCompiler config $ do
            setupCompilerEnvironment
            compileFile sourceFile
          case compileResult of
            Right (cppPath, _) -> do
              let expectedCpp = resolveWorkPath config (replaceExtension sourceFile ".cpp")
              cppPath `shouldBe` expectedCpp
              doesFileExist expectedCpp `shouldReturn` True
              doesFileExist (replaceExtension sourceFile ".cpp") `shouldReturn` False
            Left err -> expectationFailure $ "Compilation failed: " ++ show err

    it "removes intermediate files when keepIntermediates is disabled" $ do
      withFakeCompiler $ \compilerBinary logPath ->
        withSystemTempDirectory "fluxus-compile-artifacts" $ \tmpDir -> do
          let workDir = tmpDir </> "work"
              sourceDir = tmpDir </> "src"
              sourceFile = sourceDir </> "module.py"
              outputPath = workDir </> "bin" </> "module"
              includeDirs = [tmpDir </> "includes", tmpDir </> "vendor" </> "include"]
              libraryDirs = [tmpDir </> "libs"]
              linkedLibraries :: [T.Text]
              linkedLibraries = ["fluxrt", "math"]
              config = defaultConfig
                { ccCppCompiler = T.pack compilerBinary
                , ccSkipCompilerCheck = False
                , ccStopAtCodegen = False
                , ccWorkDirectory = Just workDir
                , ccOutputPath = Just outputPath
                , ccKeepIntermediates = False
                , ccEnableDebugInfo = True
                , ccEnableProfiler = True
                , ccStrictMode = True
                , ccIncludePaths = includeDirs
                , ccLibraryPaths = libraryDirs
                , ccLinkedLibraries = linkedLibraries
                , ccCppStandard = "c++23"
                , ccOptimizationLevel = O3
                , ccVerboseLevel = 0
                }
          createDirectoryIfMissing True sourceDir
          writeFile sourceFile $ unlines
            [ "def combine(a, b):"
            , "    return a + b"
            ]
          result <- runCompiler config $ do
            setupCompilerEnvironment
            compileFile sourceFile
          case result of
            Right (finalBinary, finalState) -> do
              finalBinary `shouldBe` outputPath
              csIntermediateFiles finalState `shouldBe` []
              let cppPath = resolveWorkPath config (replaceExtension sourceFile ".cpp")
                  objPath = replaceExtension cppPath ".o"
              doesFileExist cppPath `shouldReturn` False
              doesFileExist objPath `shouldReturn` False
              doesFileExist outputPath `shouldReturn` True
              logLines <- fmap lines (readFile logPath)
              case logLines of
                [compileArgs, linkArgs] -> do
                  compileArgs `shouldSatisfy` ("-std=c++23" `isInfixOf`)
                  compileArgs `shouldSatisfy` ("-O3" `isInfixOf`)
                  compileArgs `shouldSatisfy` ("-march=native" `isInfixOf`)
                  compileArgs `shouldSatisfy` ("-g" `isInfixOf`)
                  compileArgs `shouldSatisfy` ("-pg" `isInfixOf`)
                  compileArgs `shouldSatisfy` ("-Werror" `isInfixOf`)
                  mapM_ (\dir -> compileArgs `shouldSatisfy` (dir `isInfixOf`)) includeDirs
                  mapM_ (\dir -> linkArgs `shouldSatisfy` (dir `isInfixOf`)) libraryDirs
                  mapM_ (\lib -> linkArgs `shouldSatisfy` (("-l" ++ T.unpack lib) `isInfixOf`)) linkedLibraries
                  linkArgs `shouldSatisfy` ("-pg" `isInfixOf`)
                other ->
                  expectationFailure $ "expected two compiler invocations, got " ++ show other
            Left err ->
              expectationFailure $ "Compilation failed: " ++ show err

    it "preserves intermediate files when keepIntermediates is enabled" $ do
      withFakeCompiler $ \compilerBinary _logPath ->
        withSystemTempDirectory "fluxus-compile-keep" $ \tmpDir -> do
          let workDir = tmpDir </> "work"
              sourceDir = tmpDir </> "src"
              sourceFile = sourceDir </> "retain.py"
              outputPath = workDir </> "bin" </> "retain"
              config = defaultConfig
                { ccCppCompiler = T.pack compilerBinary
                , ccSkipCompilerCheck = False
                , ccStopAtCodegen = False
                , ccWorkDirectory = Just workDir
                , ccOutputPath = Just outputPath
                , ccKeepIntermediates = True
                , ccVerboseLevel = 0
                }
          createDirectoryIfMissing True sourceDir
          writeFile sourceFile $ unlines
            [ "def retain(value):"
            , "    return value"
            ]
          result <- runCompiler config $ do
            setupCompilerEnvironment
            compileFile sourceFile
          case result of
            Right (finalBinary, finalState) -> do
              finalBinary `shouldBe` outputPath
              let cppPath = resolveWorkPath config (replaceExtension sourceFile ".cpp")
                  objPath = replaceExtension cppPath ".o"
              doesFileExist cppPath `shouldReturn` True
              doesFileExist objPath `shouldReturn` True
              doesFileExist outputPath `shouldReturn` True
              csIntermediateFiles finalState `shouldSatisfy` (not . null)
              csIntermediateFiles finalState `shouldSatisfy` (objPath `elem`)
              csIntermediateFiles finalState `shouldSatisfy` (cppPath `elem`)
            Left err ->
              expectationFailure $ "Compilation failed: " ++ show err

    it "compiles and links successfully when skipping the compiler detection" $ do
      withFakeCompiler $ \compilerBinary logPath ->
        withSystemTempDirectory "fluxus-compile-skip" $ \tmpDir -> do
          let workDir = tmpDir </> "work"
              sourceDir = tmpDir </> "src"
              sourceFile = sourceDir </> "skip.py"
              outputPath = workDir </> "bin" </> "skip"
              config = defaultConfig
                { ccCppCompiler = T.pack compilerBinary
                , ccSkipCompilerCheck = True
                , ccStopAtCodegen = False
                , ccWorkDirectory = Just workDir
                , ccOutputPath = Just outputPath
                , ccVerboseLevel = 0
                }
              cppPath = resolveWorkPath config (replaceExtension sourceFile ".cpp")
              objPath = replaceExtension cppPath ".o"
          createDirectoryIfMissing True sourceDir
          writeFile sourceFile $ unlines
            [ "def skip(value):"
            , "    return value"
            ]
          result <- runCompiler config $ do
            setupCompilerEnvironment
            compileFile sourceFile
          case result of
            Right (finalBinary, finalState) -> do
              finalBinary `shouldBe` outputPath
              csResolvedCompiler finalState `shouldBe` Just (T.pack compilerBinary)
              csCompilerFallback finalState `shouldBe` False
              csIntermediateFiles finalState `shouldBe` []
              doesFileExist cppPath `shouldReturn` False
              doesFileExist objPath `shouldReturn` False
              doesFileExist outputPath `shouldReturn` True
              logLines <- fmap lines (readFile logPath)
              case logLines of
                [compileInvocation, linkInvocation] -> do
                  compileInvocation `shouldSatisfy` ("-c" `isInfixOf`)
                  compileInvocation `shouldSatisfy` (cppPath `isInfixOf`)
                  compileInvocation `shouldSatisfy` (objPath `isInfixOf`)
                  linkInvocation `shouldSatisfy` (outputPath `isInfixOf`)
                other ->
                  expectationFailure $ "expected compile and link invocations, got " ++ show other
            Left err ->
              expectationFailure $ "Compilation failed: " ++ show err

    it "fails gracefully when the C++ compiler exits with an error" $ do
      withFailingCompiler $ \compilerBinary logPath ->
        withSystemTempDirectory "fluxus-compile-failure" $ \tmpDir -> do
          let workDir = tmpDir </> "work"
              sourceDir = tmpDir </> "src"
              sourceFile = sourceDir </> "broken.py"
              outputPath = workDir </> "bin" </> "broken"
              config = defaultConfig
                { ccCppCompiler = T.pack compilerBinary
                , ccSkipCompilerCheck = False
                , ccStopAtCodegen = False
                , ccWorkDirectory = Just workDir
                , ccOutputPath = Just outputPath
                , ccVerboseLevel = 0
                }
          createDirectoryIfMissing True sourceDir
          writeFile sourceFile $ unlines
            [ "def broken():"
            , "    return 1 / 0"
            ]
          result <- runCompiler config $ do
            setupCompilerEnvironment
            compileFile sourceFile
          case result of
            Left (CodeGenError msg) -> do
              let rendered = T.unpack msg
              rendered `shouldContain` "compile failure"
              rendered `shouldContain` "exit code 64"
              doesFileExist outputPath `shouldReturn` False
              logLines <- fmap lines (readFile logPath)
              logLines `shouldSatisfy` (\entries -> length entries == 1)
            Left err ->
              expectationFailure $ "expected CodeGenError, got " ++ show err
            Right _ ->
              expectationFailure "Compilation unexpectedly succeeded"

  describe "compileProject" $ do
    it "routes generated C++ files into the work directory when stopping at code generation" $ do
      withSystemTempDirectory "fluxus-work" $ \workDir ->
        withSystemTempDirectory "fluxus-src" $ \srcDir -> do
          let firstSource = srcDir </> "pkg" </> "first.py"
              secondSource = srcDir </> "other" </> "second.py"
          createDirectoryIfMissing True (takeDirectory firstSource)
          createDirectoryIfMissing True (takeDirectory secondSource)
          writeFile firstSource $ unlines
            [ "def first():"
            , "    return 1"
            ]
          writeFile secondSource $ unlines
            [ "def second():"
            , "    return 2"
            ]
          let config = defaultConfig
                { ccStopAtCodegen = True
                , ccSkipCompilerCheck = True
                , ccWorkDirectory = Just workDir
                , ccVerboseLevel = 0
                }
          compileResult <- runCompiler config $ do
            setupCompilerEnvironment
            compileProject [firstSource, secondSource]
          case compileResult of
            Right (outputPath, _) -> do
              outputPath `shouldBe` workDir
              let firstCpp = resolveWorkPath config (replaceExtension firstSource ".cpp")
                  secondCpp = resolveWorkPath config (replaceExtension secondSource ".cpp")
              doesFileExist firstCpp `shouldReturn` True
              doesFileExist secondCpp `shouldReturn` True
              doesFileExist (replaceExtension firstSource ".cpp") `shouldReturn` False
              doesFileExist (replaceExtension secondSource ".cpp") `shouldReturn` False
            Left err -> expectationFailure $ "Project compilation failed: " ++ show err

    it "preserves directory structure in the work directory to avoid filename collisions" $ do
      withSystemTempDirectory "fluxus-work" $ \workDir ->
        withSystemTempDirectory "fluxus-src" $ \srcDir -> do
          let firstSource = srcDir </> "pkg" </> "main.py"
              secondSource = srcDir </> "tests" </> "main.py"
          createDirectoryIfMissing True (takeDirectory firstSource)
          createDirectoryIfMissing True (takeDirectory secondSource)
          writeFile firstSource $ unlines
            [ "def first():"
            , "    return 1"
            ]
          writeFile secondSource $ unlines
            [ "def second():"
            , "    return 2"
            ]
          let config = defaultConfig
                { ccStopAtCodegen = True
                , ccSkipCompilerCheck = True
                , ccWorkDirectory = Just workDir
                , ccVerboseLevel = 0
                }
          compileResult <- runCompiler config $ do
            setupCompilerEnvironment
            compileProject [firstSource, secondSource]
          case compileResult of
            Right _ -> do
              let firstCpp = resolveWorkPath config (replaceExtension firstSource ".cpp")
                  secondCpp = resolveWorkPath config (replaceExtension secondSource ".cpp")
              firstCpp `shouldNotBe` secondCpp
              takeDirectory firstCpp `shouldNotBe` takeDirectory secondCpp
              doesFileExist firstCpp `shouldReturn` True
              doesFileExist secondCpp `shouldReturn` True
            Left err -> expectationFailure $ "Project compilation failed: " ++ show err

    it "returns the configured output path when stopping at code generation" $ do
      withSystemTempDirectory "fluxus-project-output-work" $ \workDir ->
        withSystemTempDirectory "fluxus-project-output-src" $ \srcDir -> do
          let firstSource = srcDir </> "pkg" </> "alpha.py"
              secondSource = srcDir </> "pkg" </> "beta.py"
          createDirectoryIfMissing True (takeDirectory firstSource)
          createDirectoryIfMissing True (takeDirectory secondSource)
          writeFile firstSource $ unlines
            [ "def alpha():"
            , "    return 10"
            ]
          writeFile secondSource $ unlines
            [ "def beta():"
            , "    return 20"
            ]
          let outputPath = workDir </> "artifacts" </> "custom-binary"
              config = defaultConfig
                { ccStopAtCodegen = True
                , ccSkipCompilerCheck = True
                , ccWorkDirectory = Just workDir
                , ccOutputPath = Just outputPath
                , ccVerboseLevel = 0
                }
          compileResult <- runCompiler config $ do
            setupCompilerEnvironment
            compileProject [firstSource, secondSource]
          case compileResult of
            Right (finalPath, _) -> do
              finalPath `shouldBe` outputPath
              doesFileExist outputPath `shouldReturn` False
              let firstCpp = resolveWorkPath config (replaceExtension firstSource ".cpp")
                  secondCpp = resolveWorkPath config (replaceExtension secondSource ".cpp")
              doesFileExist firstCpp `shouldReturn` True
              doesFileExist secondCpp `shouldReturn` True
            Left err ->
              expectationFailure $ "Project compilation failed: " ++ show err

    it "links multiple modules into the default output when no output path is provided" $ do
      withFakeCompiler $ \compilerBinary logPath ->
        withSystemTempDirectory "fluxus-project-link" $ \tmpRoot -> do
          let workDir = tmpRoot </> "work"
              sourceDir = tmpRoot </> "src"
              firstSource = sourceDir </> "pkg" </> "alpha.py"
              secondSource = sourceDir </> "pkg" </> "beta.py"
              config = defaultConfig
                { ccCppCompiler = T.pack compilerBinary
                , ccSkipCompilerCheck = False
                , ccStopAtCodegen = False
                , ccWorkDirectory = Just workDir
                , ccKeepIntermediates = False
                , ccVerboseLevel = 0
                }
          createDirectoryIfMissing True (takeDirectory firstSource)
          createDirectoryIfMissing True (takeDirectory secondSource)
          writeFile firstSource $ unlines
            [ "def alpha():"
            , "    return 1"
            ]
          writeFile secondSource $ unlines
            [ "def beta():"
            , "    return 2"
            ]
          result <- runCompiler config $ do
            setupCompilerEnvironment
            compileProject [firstSource, secondSource]
          case result of
            Right (finalBinary, finalState) -> do
              let expectedOutput = workDir </> "fluxus_output"
              finalBinary `shouldBe` expectedOutput
              doesFileExist expectedOutput `shouldReturn` True
              csProcessedFiles finalState `shouldBe` 2
              csIntermediateFiles finalState `shouldBe` []
              logLines <- fmap lines (readFile logPath)
              logLines `shouldSatisfy` (\entries -> length entries == 3)
            Left err ->
              expectationFailure $ "Project compilation failed: " ++ show err

    it "respects an explicit output path when linking a project" $ do
      withFakeCompiler $ \compilerBinary logPath ->
        withSystemTempDirectory "fluxus-project-custom-link" $ \tmpRoot -> do
          let workDir = tmpRoot </> "work"
              sourceDir = tmpRoot </> "src"
              firstSource = sourceDir </> "pkg" </> "alpha.py"
              secondSource = sourceDir </> "pkg" </> "beta.py"
              outputPath = tmpRoot </> "artifacts" </> "bin" </> "app"
              config = defaultConfig
                { ccCppCompiler = T.pack compilerBinary
                , ccSkipCompilerCheck = False
                , ccStopAtCodegen = False
                , ccWorkDirectory = Just workDir
                , ccOutputPath = Just outputPath
                , ccKeepIntermediates = False
                , ccVerboseLevel = 0
                }
          createDirectoryIfMissing True (takeDirectory firstSource)
          createDirectoryIfMissing True (takeDirectory secondSource)
          writeFile firstSource $ unlines
            [ "def alpha():"
            , "    return 1"
            ]
          writeFile secondSource $ unlines
            [ "def beta():"
            , "    return 2"
            ]
          result <- runCompiler config $ do
            setupCompilerEnvironment
            compileProject [firstSource, secondSource]
          case result of
            Right (finalBinary, finalState) -> do
              finalBinary `shouldBe` outputPath
              doesFileExist outputPath `shouldReturn` True
              csIntermediateFiles finalState `shouldBe` []
              logLines <- fmap lines (readFile logPath)
              logLines `shouldSatisfy` (\entries -> length entries == 3)
            Left err ->
              expectationFailure $ "Project compilation failed: " ++ show err

    it "cleans up intermediate artifacts when compiling without a work directory" $ do
      withFakeCompiler $ \compilerBinary _logPath ->
        withSystemTempDirectory "fluxus-project-no-workdir" $ \tmpRoot -> do
          let sourceDir = tmpRoot </> "src"
              firstSource = sourceDir </> "alpha.py"
              secondSource = sourceDir </> "beta.py"
              outputPath = tmpRoot </> "bin" </> "app"
              config = defaultConfig
                { ccCppCompiler = T.pack compilerBinary
                , ccSkipCompilerCheck = False
                , ccStopAtCodegen = False
                , ccWorkDirectory = Nothing
                , ccOutputPath = Just outputPath
                , ccKeepIntermediates = False
                , ccVerboseLevel = 0
                }
          createDirectoryIfMissing True sourceDir
          writeFile firstSource $ unlines
            [ "def alpha():"
            , "    return 1"
            ]
          writeFile secondSource $ unlines
            [ "def beta():"
            , "    return 2"
            ]
          result <- runCompiler config $ do
            setupCompilerEnvironment
            compileProject [firstSource, secondSource]
          case result of
            Right (finalBinary, finalState) -> do
              finalBinary `shouldBe` outputPath
              doesFileExist outputPath `shouldReturn` True
              let cppPaths = map (`replaceExtension` ".cpp") [firstSource, secondSource]
                  objPaths = map (`replaceExtension` ".o") cppPaths
              mapM_ (\path -> doesFileExist path `shouldReturn` False) (cppPaths ++ objPaths)
              csIntermediateFiles finalState `shouldBe` []
              csProcessedFiles finalState `shouldBe` 2
            Left err ->
              expectationFailure $ "Project compilation failed: " ++ show err

    it "propagates profiler flags to compile and link invocations for projects" $ do
      withFakeCompiler $ \compilerBinary logPath ->
        withSystemTempDirectory "fluxus-project-profiler-flags" $ \tmpRoot -> do
          let workDir = tmpRoot </> "work"
              sourceDir = tmpRoot </> "src"
              outputPath = tmpRoot </> "bin" </> "app"
              firstSource = sourceDir </> "pkg" </> "alpha.py"
              secondSource = sourceDir </> "pkg" </> "beta.py"
              config = defaultConfig
                { ccCppCompiler = T.pack compilerBinary
                , ccSkipCompilerCheck = False
                , ccStopAtCodegen = False
                , ccWorkDirectory = Just workDir
                , ccOutputPath = Just outputPath
                , ccEnableProfiler = True
                , ccEnableDebugInfo = True
                , ccKeepIntermediates = False
                , ccVerboseLevel = 0
                }
          createDirectoryIfMissing True (takeDirectory firstSource)
          createDirectoryIfMissing True (takeDirectory secondSource)
          writeFile firstSource $ unlines
            [ "def alpha():"
            , "    return 1"
            ]
          writeFile secondSource $ unlines
            [ "def beta():"
            , "    return 2"
            ]
          result <- runCompiler config $ do
            setupCompilerEnvironment
            compileProject [firstSource, secondSource]
          case result of
            Right (finalBinary, finalState) -> do
              finalBinary `shouldBe` outputPath
              doesFileExist outputPath `shouldReturn` True
              csIntermediateFiles finalState `shouldBe` []
              logLines <- fmap lines (readFile logPath)
              logLines `shouldSatisfy` (\entries -> length entries == 3)
              let compileCommands = init logLines
                  linkCommand = last logLines
              length compileCommands `shouldBe` 2
              mapM_ (\cmd -> cmd `shouldSatisfy` ("-pg" `isInfixOf`)) compileCommands
              mapM_ (\cmd -> cmd `shouldSatisfy` ("-g" `isInfixOf`)) compileCommands
              linkCommand `shouldSatisfy` ("-pg" `isInfixOf`)
            Left err ->
              expectationFailure $ "Project compilation failed: " ++ show err

    it "retains intermediate artifacts when keepIntermediates is enabled for projects" $ do
      withFakeCompiler $ \compilerBinary _logPath ->
        withSystemTempDirectory "fluxus-project-keep" $ \tmpRoot -> do
          let workDir = tmpRoot </> "work"
              sourceDir = tmpRoot </> "src"
              firstSource = sourceDir </> "pkg" </> "alpha.py"
              secondSource = sourceDir </> "pkg" </> "beta.py"
              config = defaultConfig
                { ccCppCompiler = T.pack compilerBinary
                , ccSkipCompilerCheck = False
                , ccStopAtCodegen = False
                , ccWorkDirectory = Just workDir
                , ccKeepIntermediates = True
                , ccVerboseLevel = 0
                }
          createDirectoryIfMissing True (takeDirectory firstSource)
          createDirectoryIfMissing True (takeDirectory secondSource)
          writeFile firstSource $ unlines
            [ "def alpha():"
            , "    return 1"
            ]
          writeFile secondSource $ unlines
            [ "def beta():"
            , "    return 2"
            ]
          result <- runCompiler config $ do
            setupCompilerEnvironment
            compileProject [firstSource, secondSource]
          case result of
            Right (finalBinary, finalState) -> do
              let expectedOutput = workDir </> "fluxus_output"
              finalBinary `shouldBe` expectedOutput
              doesFileExist expectedOutput `shouldReturn` True
              let cppPaths = map (\src -> resolveWorkPath config (replaceExtension src ".cpp")) [firstSource, secondSource]
                  objPaths = map (\cpp -> replaceExtension cpp ".o") cppPaths
                  expectedArtifacts = cppPaths ++ objPaths
              mapM_ (\path -> doesFileExist path `shouldReturn` True) expectedArtifacts
              csIntermediateFiles finalState `shouldSatisfy` (\paths -> all (`elem` paths) expectedArtifacts)
            Left err ->
              expectationFailure $ "Project compilation failed: " ++ show err

    it "updates processed and total file counters during project compilation" $ do
      withSystemTempDirectory "fluxus-project-counters-work" $ \workDir ->
        withSystemTempDirectory "fluxus-project-counters-src" $ \srcDir -> do
          let sourcesWithBodies =
                [ (srcDir </> "pkg" </> "alpha.py", ["def alpha():", "    return 1"])
                , (srcDir </> "pkg" </> "beta.py", ["def beta():", "    return 2"])
                , (srcDir </> "other" </> "gamma.py", ["def gamma():", "    return 3"])
                ]
              sources = map fst sourcesWithBodies
          mapM_ (\(path, bodyLines) -> do
                    createDirectoryIfMissing True (takeDirectory path)
                    writeFile path (unlines bodyLines)
                ) sourcesWithBodies
          let config = defaultConfig
                { ccStopAtCodegen = True
                , ccSkipCompilerCheck = True
                , ccWorkDirectory = Just workDir
                , ccVerboseLevel = 0
                }
          result <- runCompiler config $ do
            setupCompilerEnvironment
            _ <- compileProject sources
            st <- get
            pure (csProcessedFiles st, csTotalFiles st)
          case result of
            Right ((processed, total), _) -> do
              processed `shouldBe` length sources
              total `shouldBe` length sources
            Left err ->
              expectationFailure $ "Project compilation failed: " ++ show err

    it "propagates linker failures with diagnostic messages" $ do
      withFailingLinkerCompiler $ \compilerBinary logPath ->
        withSystemTempDirectory "fluxus-project-link-failure" $ \tmpRoot -> do
          let workDir = tmpRoot </> "work"
              sourceDir = tmpRoot </> "src"
              firstSource = sourceDir </> "pkg" </> "alpha.py"
              secondSource = sourceDir </> "pkg" </> "beta.py"
              config = defaultConfig
                { ccCppCompiler = T.pack compilerBinary
                , ccSkipCompilerCheck = False
                , ccStopAtCodegen = False
                , ccWorkDirectory = Just workDir
                , ccKeepIntermediates = False
                , ccVerboseLevel = 0
                }
          createDirectoryIfMissing True (takeDirectory firstSource)
          createDirectoryIfMissing True (takeDirectory secondSource)
          writeFile firstSource $ unlines
            [ "def alpha():"
            , "    return 1"
            ]
          writeFile secondSource $ unlines
            [ "def beta():"
            , "    return 2"
            ]
          result <- runCompiler config $ do
            setupCompilerEnvironment
            compileProject [firstSource, secondSource]
          case result of
            Left (LinkError msg) -> do
              let rendered = T.unpack msg
              rendered `shouldContain` "Linking failed"
              rendered `shouldContain` "exit code 42"
              doesFileExist (workDir </> "fluxus_output") `shouldReturn` False
              logLines <- fmap lines (readFile logPath)
              logLines `shouldSatisfy` (\entries -> length entries == 3)
            Left err ->
              expectationFailure $ "expected LinkError, got " ++ show err
            Right _ ->
              expectationFailure "Linking unexpectedly succeeded"

    it "stops project compilation when a translation unit fails to compile" $ do
      withFailingCompiler $ \compilerBinary logPath ->
        withSystemTempDirectory "fluxus-project-compile-failure" $ \tmpRoot -> do
          let workDir = tmpRoot </> "work"
              sourceDir = tmpRoot </> "src"
              firstSource = sourceDir </> "pkg" </> "alpha.py"
              secondSource = sourceDir </> "pkg" </> "beta.py"
              config = defaultConfig
                { ccCppCompiler = T.pack compilerBinary
                , ccSkipCompilerCheck = False
                , ccStopAtCodegen = False
                , ccWorkDirectory = Just workDir
                , ccVerboseLevel = 0
                }
          createDirectoryIfMissing True (takeDirectory firstSource)
          createDirectoryIfMissing True (takeDirectory secondSource)
          writeFile firstSource $ unlines
            [ "def alpha():"
            , "    return 1"
            ]
          writeFile secondSource $ unlines
            [ "def beta():"
            , "    return 2"
            ]
          result <- runCompiler config $ do
            setupCompilerEnvironment
            compileProject [firstSource, secondSource]
          case result of
            Left (CodeGenError msg) -> do
              let rendered = T.unpack msg
              rendered `shouldContain` "compile failure"
              logLines <- fmap lines (readFile logPath)
              logLines `shouldSatisfy` (\entries -> length entries == 1)
            Left err ->
              expectationFailure $ "expected CodeGenError, got " ++ show err
            Right _ ->
              expectationFailure "Project compilation unexpectedly succeeded"

withTemporaryEnv :: String -> Maybe String -> IO a -> IO a
withTemporaryEnv key newValue action = do
  original <- lookupEnv key
  let setVar = maybe (unsetEnv key) (setEnv key) newValue
      restoreVar = maybe (unsetEnv key) (setEnv key) original
  bracket_ setVar restoreVar action

createExecutable :: FilePath -> String -> IO ()
createExecutable path contents = do
  writeFile path contents
  perms <- getPermissions path
  setPermissions path perms { executable = True }

trivialCompilerScript :: String
trivialCompilerScript =
  unlines
    [ "#!/usr/bin/env bash"
    , "exit 0"
    ]

withCustomCompiler :: (FilePath -> String) -> (FilePath -> FilePath -> IO a) -> IO a
withCustomCompiler mkScript action =
  withSystemTempDirectory "fluxus-custom-compiler" $ \tmpDir -> do
    let scriptPath = tmpDir </> "fake-compiler.sh"
        logPath = tmpDir </> "compiler-invocations.log"
    writeFile scriptPath (mkScript logPath)
    perms <- getPermissions scriptPath
    setPermissions scriptPath perms { executable = True }
    action scriptPath logPath

withFakeCompiler :: (FilePath -> FilePath -> IO a) -> IO a
withFakeCompiler = withCustomCompiler fakeCompilerScript

withFailingCompiler :: (FilePath -> FilePath -> IO a) -> IO a
withFailingCompiler = withCustomCompiler failingCompilerScript

withFailingLinkerCompiler :: (FilePath -> FilePath -> IO a) -> IO a
withFailingLinkerCompiler = withCustomCompiler failingLinkerScript

fakeCompilerScript :: FilePath -> String
fakeCompilerScript logPath =
  unlines
    [ "#!/usr/bin/env bash"
    , "echo \"$@\" >> \"" ++ logPath ++ "\""
    , "prev=\"\""
    , "out_file=\"\""
    , "for arg in \"$@\"; do"
    , "  if [ \"$prev\" = \"-o\" ]; then"
    , "    out_file=\"$arg\""
    , "  fi"
    , "  prev=\"$arg\""
    , "done"
    , "if [ -n \"$out_file\" ]; then"
    , "  mkdir -p \"$(dirname \"$out_file\")\""
    , "  touch \"$out_file\""
    , "fi"
    , "exit 0"
    ]

failingCompilerScript :: FilePath -> String
failingCompilerScript logPath =
  unlines
    [ "#!/usr/bin/env bash"
    , "echo \"$@\" >> \"" ++ logPath ++ "\""
    , "echo \"compile failure\" >&2"
    , "exit 64"
    ]

failingLinkerScript :: FilePath -> String
failingLinkerScript logPath =
  unlines
    [ "#!/usr/bin/env bash"
    , "echo \"$@\" >> \"" ++ logPath ++ "\""
    , "prev=\"\""
    , "out_file=\"\""
    , "mode=\"link\""
    , "for arg in \"$@\"; do"
    , "  if [ \"$arg\" = \"-c\" ]; then"
    , "    mode=\"compile\""
    , "  fi"
    , "  if [ \"$prev\" = \"-o\" ]; then"
    , "    out_file=\"$arg\""
    , "  fi"
    , "  prev=\"$arg\""
    , "done"
    , "if [ \"$mode\" = \"compile\" ]; then"
    , "  mkdir -p \"$(dirname \"$out_file\")\""
    , "  touch \"$out_file\""
    , "  exit 0"
    , "fi"
    , "exit 42"
    ]
