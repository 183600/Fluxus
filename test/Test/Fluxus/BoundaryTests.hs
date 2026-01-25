{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.BoundaryTests (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Exception (try, SomeException)
import Data.List (replicate)

import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.Analysis.TypeInference
import Fluxus.AST.Common
import Fluxus.Compiler.Config (parseCommandLineArgs, CLICommand(..))
import Fluxus.AST.Python (PythonModule, pyModuleBody, PythonAST(..))
import Fluxus.AST.Common (Located, locatedValue)

spec :: Spec
spec = describe "Boundary Tests" $ do
  describe "Python Parser - Edge Cases" $ do
    it "handles empty module input" $ do
      let input = ""
      case runPythonLexer "empty.py" input of
        Left _ -> expectationFailure "Lexer should handle empty input"
        Right tokens -> tokens `shouldBe` []

    it "handles special characters in strings" $ do
      let input = "x = \"Hello\\nWorld!\""
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser should handle special chars: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 1

    it "handles deeply nested parentheses" $ do
      let depth = 100
          input = T.pack $ "x = " <> replicate depth '(' <> "1" <> replicate depth ')'
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser should handle deep nesting: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 1

    it "handles extremely long string literals" $ do
      let longString = T.pack $ replicate 10000 'a'
          input = T.concat ["x = \"", longString, "\""]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser should handle long strings: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 1

    it "handles empty list, dict, and set literals" $ do
      let input = T.unlines
            [ "empty_list = []"
            , "empty_dict = {}"
            , "empty_set = set()"
            ]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser should handle empty collections: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 3

    it "handles very large integer literals" $ do
      let largeInt = T.pack $ '1' : replicate 100 '0'
          input = T.concat ["x = ", largeInt]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser should handle large integers: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 1

    it "handles special Unicode identifiers" $ do
      let input = T.unlines
            [ "def 函数名():"
            , "    pass"
            , "变量_1 = 42"
            ]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser should handle Unicode identifiers: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 2

    it "handles simple assignment after comments" $ do
      let input = "# Comment\nx = 1"
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser should handle simple code: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 1

    it "handles extremely long source lines" $ do
      let longLine = T.pack $ concat (replicate 5000 "x + ") <> "1"
          input = T.concat ["result = ", longLine]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser should handle long lines: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 1

    it "handles many sequential expressions" $ do
      let statements = T.intercalate "\n" $ map (\i -> T.pack $ "x" <> show i <> " = " <> show i) [1..100]
      case parseModuleFrom statements of
        Left err -> expectationFailure $ "Parser should handle many statements: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 100

  describe "Type Inference - Edge Cases" $ do
    it "handles empty type environment" $ do
      let expr = CELiteral (LInt 42)
      case runTypeInference Map.empty (inferType expr) of
        Right inference -> resultType inference `shouldBe` TInt 32
        Left err -> expectationFailure $ "Type inference should handle empty env: " <> T.unpack err

    it "handles deeply nested type constraints" $ do
      let buildNestedAdd depth =
            if depth <= 0
            then CELiteral (LInt 1)
            else CEBinaryOp OpAdd (noLoc (buildNestedAdd (depth - 1))) (noLoc (buildNestedAdd (depth - 1)))
          expr = buildNestedAdd 10
      case runTypeInference Map.empty (inferType expr) of
        Right inference -> resultType inference `shouldBe` TInt 32
        Left err -> expectationFailure $ "Type inference should handle nested constraints: " <> T.unpack err

    it "handles very large list literals in type inference" $ do
      let largeList = CEList $ replicate 100 (noLoc (CELiteral (LInt 1)))
      case runTypeInference Map.empty (inferType largeList) of
        Right inference -> resultType inference `shouldBe` TList (TInt 32)
        Left err -> expectationFailure $ "Type inference should handle large lists: " <> T.unpack err

    it "handles conflicting type constraints gracefully" $ do
      let expr = CEBinaryOp OpAdd (noLoc (CELiteral (LInt 1))) (noLoc (CELiteral (LString "error")))
      case runTypeInference Map.empty (inferType expr) of
        Right _ -> expectationFailure "Type inference should detect conflicts"
        Left _ -> pure ()  -- Expected to fail

  describe "CLI Argument Parsing - Edge Cases" $ do
    it "handles empty command line arguments" $ do
      case parseCommandLineArgs [] of
        Right (CLICommandModify _ _) -> pure ()
        Left err -> expectationFailure $ "CLI should handle empty args: " <> err

    it "handles very long file paths in arguments" $ do
      let longPath = "/" <> concat (replicate 500 "path/") <> "file.py"
      case parseCommandLineArgs [longPath] of
        Right (CLICommandModify _ _) -> pure ()
        Left err -> expectationFailure $ "CLI should handle long paths: " <> err

    it "handles many repeated flags" $ do
      let args = concat $ replicate 50 ["--verbose"]
      case parseCommandLineArgs args of
        Right (CLICommandModify _ _) -> pure ()
        Left err -> expectationFailure $ "CLI should handle many flags: " <> err

    it "handles conflicting flag combinations" $ do
      case parseCommandLineArgs ["--verbose", "--quiet"] of
        Right (CLICommandModify _ _) -> pure ()  -- Should handle gracefully
        Left err -> expectationFailure $ "CLI should handle conflicting flags: " <> err

  describe "Memory and Performance Boundaries" $ do
    it "handles evaluation of deeply nested data structures" $ do
      let deepList = replicate 50 $ replicate 50 $ CELiteral (LInt 1)
          expr = CEList $ map (noLoc . CEList . map noLoc) deepList
      case runTypeInference Map.empty (inferType expr) of
        Right inference -> resultType inference `shouldBe` TList (TList (TInt 32))
        Left err -> expectationFailure $ "Type inference should handle deep structures: " <> T.unpack err

-- Helper function to parse module from text
parseModuleFrom :: T.Text -> Either String PythonModule
parseModuleFrom source =
  case runPythonLexer (T.pack "test.py") source of
    Left err -> Left (show err)
    Right tokens ->
      case runPythonParser (T.pack "test.py") tokens of
        Left perr -> Left (show perr)
        Right (PythonAST modu) -> Right modu
