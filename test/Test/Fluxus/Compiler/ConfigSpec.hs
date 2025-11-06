module Test.Fluxus.Compiler.ConfigSpec (spec) where

import Test.Hspec
import Control.Exception (bracket_)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.Text as T

import Fluxus.Compiler.Driver (CompilerConfig(..), defaultConfig)
import Fluxus.Compiler.Config
  ( CLICommand(..)
  , CompilerConfigOverrides(..)
  , LoadConfigResult(..)
  , emptyOverrides
  , fluxusVersionString
  , loadConfig
  , mergeConfigs
  , parseCommandLineArgs
  )

spec :: Spec
spec = describe "Fluxus.Compiler.Config" $ do
  describe "parseCommandLineArgs" $ do
    it "recognizes --help as a help command" $ do
      case parseCommandLineArgs ["--help"] of
        Right CLICommandShowHelp -> pure ()
        _ -> expectationFailure "expected CLICommandShowHelp for --help"

    it "recognizes --version as a version command" $ do
      case parseCommandLineArgs ["--version"] of
        Right (CLICommandShowVersion versionText) ->
          versionText `shouldBe` fluxusVersionString
        _ -> expectationFailure "expected CLICommandShowVersion for --version"

    it "suggests using --help for unknown options" $ do
      case parseCommandLineArgs ["--unknown-option"] of
        Left err -> err `shouldContain` "--help"
        Right _ -> expectationFailure "expected parse error for unknown option"

    it "deduplicates include paths when the same CLI flag is provided multiple times" $ do
      case parseCommandLineArgs ["--include", "/custom", "--include", "/custom"] of
        Right (CLICommandModify modifier) -> do
          let updated = modifier defaultConfig
          ccIncludePaths updated `shouldBe` ["/custom", "/usr/include", "/usr/local/include"]
        Right _ -> expectationFailure "expected CLICommandModify for --include"
        Left err -> expectationFailure $ "failed to parse CLI args: " ++ err

  describe "mergeConfigs" $ do
    it "allows overriding include paths with an empty list" $ do
      let overrides = emptyOverrides { ccoIncludePaths = Just [] }
          merged = mergeConfigs defaultConfig overrides
      ccIncludePaths merged `shouldBe` []

    it "deduplicates list fields while preferring overrides when provided" $ do
      let baseConfig =
            defaultConfig
              { ccIncludePaths = ["/usr/include", "/usr/include"]
              , ccLibraryPaths = ["/usr/lib", "/usr/lib"]
              , ccLinkedLibraries = map T.pack ["stdc++", "stdc++"]
              }
          overrides =
            emptyOverrides
              { ccoIncludePaths = Just ["/usr/include", "/custom/include", "/custom/include"]
              , ccoLibraryPaths = Just ["/custom/lib", "/usr/lib", "/custom/lib"]
              , ccoLinkedLibraries = Just (map T.pack ["stdc++", "pthread", "stdc++"])
              }
          merged = mergeConfigs baseConfig overrides
      ccIncludePaths merged `shouldBe` ["/usr/include", "/custom/include"]
      ccLibraryPaths merged `shouldBe` ["/custom/lib", "/usr/lib"]
      ccLinkedLibraries merged `shouldBe` map T.pack ["stdc++", "pthread"]

  describe "loadConfig" $ do
    it "returns LoadConfigHelp when --help is provided" $ do
      result <- loadConfig ["--help"]
      result `shouldBe` Right LoadConfigHelp

    it "returns LoadConfigVersion when --version is provided" $ do
      result <- loadConfig ["--version"]
      result `shouldBe` Right (LoadConfigVersion fluxusVersionString)

    it "applies precedence CLI > environment > config file > defaults" $ do
      originalCwd <- getCurrentDirectory
      originalCxx <- lookupEnv "CXX"
      originalVerbose <- lookupEnv "FLUXUS_VERBOSE"
      withSystemTempDirectory "fluxus-config-test" $ \tmpDir -> do
        let configContent = unlines
              [ "cpp_standard: c++23"
              , "cpp_compiler: file-clang++"
              , "include_paths:"
              , "  - /file/include"
              , "library_paths:"
              , "  - /file/lib"
              ]
        writeFile (tmpDir </> "fluxus.yaml") configContent

        let restoreEnv name maybeValue =
              case maybeValue of
                Just value -> setEnv name value
                Nothing -> unsetEnv name

        bracket_
          (do setCurrentDirectory tmpDir
              setEnv "CXX" "env-clang++"
              setEnv "FLUXUS_VERBOSE" "2")
          (do setCurrentDirectory originalCwd
              restoreEnv "CXX" originalCxx
              restoreEnv "FLUXUS_VERBOSE" originalVerbose)
          (do result <- loadConfig ["--cpp-std", "c++26", "--cpp-compiler", "cli-clang++", "--include", "/cli/include"]
              case result of
                Right (LoadConfigSuccess finalConfig) -> do
                  ccCppCompiler finalConfig `shouldBe` T.pack "cli-clang++"
                  ccCppStandard finalConfig `shouldBe` T.pack "c++26"
                  ccVerboseLevel finalConfig `shouldBe` 2
                  ccIncludePaths finalConfig `shouldBe` ["/cli/include", "/file/include"]
                  ccLibraryPaths finalConfig `shouldBe` ["/file/lib"]
                Right other -> expectationFailure $ "unexpected non-success result: " ++ show other
                Left err -> expectationFailure $ "loadConfig failed: " ++ err)
