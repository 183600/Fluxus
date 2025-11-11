{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Compiler.DriverSpec (spec) where

import qualified Data.Text as T
import Data.Either (isRight)
import Test.Hspec
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory, replaceExtension)
import System.IO.Temp (withSystemTempDirectory)

import Fluxus.Compiler.Driver
  ( CompilerConfig(..)
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

    it "succeeds when stopping at code generation" $ do
      let config = defaultConfig
            { ccCppCompiler = "fluxus-nonexistent-compiler"
            , ccStopAtCodegen = True
            , ccVerboseLevel = 0
            }
      result <- runCompiler config setupCompilerEnvironment
      result `shouldSatisfy` isRight

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
