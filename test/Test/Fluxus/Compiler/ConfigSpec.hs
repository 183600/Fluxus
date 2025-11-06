module Test.Fluxus.Compiler.ConfigSpec (spec) where

import Test.Hspec

import Fluxus.Compiler.Config
  ( CLICommand(..)
  , LoadConfigResult(..)
  , fluxusVersionString
  , loadConfig
  , parseCommandLineArgs
  )

spec :: Spec
spec = describe "Fluxus.Compiler.Config CLI parsing" $ do
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

  describe "loadConfig" $ do
    it "returns LoadConfigHelp when --help is provided" $ do
      result <- loadConfig ["--help"]
      result `shouldBe` Right LoadConfigHelp

    it "returns LoadConfigVersion when --version is provided" $ do
      result <- loadConfig ["--version"]
      result `shouldBe` Right (LoadConfigVersion fluxusVersionString)
