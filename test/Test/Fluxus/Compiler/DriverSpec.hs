{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Compiler.DriverSpec (spec) where

import qualified Data.Text as T
import Data.Either (isRight)
import Data.List (isPrefixOf)
import Control.Monad.State (get)
import Test.Hspec
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable)
import System.FilePath ((</>), addTrailingPathSeparator, takeDirectory, replaceExtension, normalise)
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
