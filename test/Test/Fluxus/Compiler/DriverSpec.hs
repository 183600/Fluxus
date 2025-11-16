{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Compiler.DriverSpec (spec) where

import qualified Data.Text as T
import Data.Either (isRight)
import Data.List (isInfixOf, isPrefixOf)
import Control.Monad.State (get)
import Test.Hspec
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getPermissions, setPermissions, Permissions(..))
import System.FilePath ((</>), addTrailingPathSeparator, takeDirectory, takeExtension, replaceExtension, normalise)
import System.IO.Temp (withSystemTempDirectory)

import qualified Test.Fluxus.CodeGen.CPP.Shared as Shared

import Fluxus.Compiler.Driver
  ( CompilerConfig(..)
  , CompilerState(..)
  , CompilerError(..)
  , compileFile
  , compileProject
  , defaultConfig
  , runCompiler
  , setupCompilerEnvironment
  , resolveWorkPath
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
