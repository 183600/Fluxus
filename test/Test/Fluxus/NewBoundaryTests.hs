{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Fluxus.NewBoundaryTests (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Exception (try, SomeException, evaluate)
import Data.List (replicate)
import Control.DeepSeq (force)

import Fluxus.AST.Common
import Fluxus.Utils.Graph
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.Analysis.TypeInference
import Fluxus.Compiler.Config
import Fluxus.AST.Python (PythonModule, PythonAST(..), pyModuleBody)

spec :: Spec
spec = describe "New Boundary Tests" $ do
  describe "AST and Expression Boundary Tests" $ do
    it "handles empty expressions and literals" $ do
      let emptyList = CEList []
          emptyTuple = CETuple []
      
      case runTypeInference Map.empty (inferType emptyList) of
        Right inference -> resultType inference `shouldSatisfy` (\t -> case t of
          TList _ -> True
          _ -> False)
        Left err -> expectationFailure $ "Type inference failed on empty list: " <> T.unpack err
      
      case runTypeInference Map.empty (inferType emptyTuple) of
        Right inference -> resultType inference `shouldBe` TTuple []
        Left err -> expectationFailure $ "Type inference failed on empty tuple: " <> T.unpack err

    it "handles extremely deep expression nesting" $ do
      let buildNested depth =
            if depth <= 0
            then CELiteral (LInt 1)
            else CEBinaryOp OpAdd (noLoc (buildNested (depth - 1))) (noLoc (buildNested (depth - 1)))
          expr = buildNested 10
      case runTypeInference Map.empty (inferType expr) of
        Right inference -> resultType inference `shouldBe` TInt 32
        Left err -> expectationFailure $ "Type inference failed on deep nesting: " <> T.unpack err

    it "handles extreme numeric literal values" $ do
      let smallInt = CELiteral (LInt (-9223372036854775808))
          largeInt = CELiteral (LInt 9223372036854775807)
      
      case runTypeInference Map.empty (inferType smallInt) of
        Right inference -> resultType inference `shouldSatisfy` (\t -> t == TInt 32 || t == TInt 64)
        Left err -> expectationFailure $ "Type inference failed on small int: " <> T.unpack err
      
      case runTypeInference Map.empty (inferType largeInt) of
        Right inference -> resultType inference `shouldSatisfy` (\t -> t == TInt 32 || t == TInt 64)
        Left err -> expectationFailure $ "Type inference failed on large int: " <> T.unpack err

    it "handles Unicode and special characters in identifiers" $ do
      let unicodeCases = [
                ("x_函数_名", "Mixed ASCII and Chinese"),
                ("переменная", "Cyrillic variable"),
                ("α_β_γ", "Greek letters"),
                ("変数_名前", "Japanese characters"),
                ("____", "Underscores only"),
                ("a1b2c3_", "Numbers and letters")
              ]
          buildVar name = CEBinaryOp OpAdd (noLoc (CEVar (Identifier name))) (noLoc (CELiteral (LInt 1)))
      
      forM_ unicodeCases $ \(name, desc) -> do
        let expr = buildVar name
        case runTypeInference Map.empty (inferType expr) of
          Right _ -> pure ()  -- Success is expected
          Left err -> expectationFailure $ "Type inference failed on " <> desc <> ": " <> T.unpack err

  describe "Graph Algorithm Stress Tests" $ do
    it "handles massive graph with 1000 nodes" $ do
      let buildMassiveGraph n g = if n <= 0 then g else
            let (nid, g1) = addNode ("node_" ++ show n) g
                g2 = if n > 1 then addEdge nid (nid - 1) Nothing g1 else g1
            in buildMassiveGraph (n - 1) g2
          massiveGraph = buildMassiveGraph 1000 emptyGraph
          (startId, _) = addNode "start" massiveGraph
          (endId, _) = addNode "end" massiveGraph
      
      nodes massiveGraph `shouldSatisfy` (\ns -> length ns >= 1000)
      shortestPath startId endId massiveGraph `shouldSatisfy` (\maybePath ->
        case maybePath of
          Just path -> length path <= 1000
          Nothing -> True)

    it "handles graph with multiple cycles and complex topology" $ do
      let (n1, g1) = addNode "A" emptyGraph
          (n2, g2) = addNode "B" g1
          (n3, g3) = addNode "C" g2
          (n4, g4) = addNode "D" g3
          g5 = addEdge n1 n2 Nothing g4
          g6 = addEdge n2 n3 Nothing g5
          g7 = addEdge n3 n1 Nothing g6  -- Create cycle A->B->C->A
          g8 = addEdge n3 n4 Nothing g7
          g9 = addEdge n4 n2 Nothing g8  -- Create another cycle
      
      topologicalSort g9 `shouldBe` Nothing  -- Should detect cycle
      let sccs = stronglyConnectedComponents g9
      length sccs `shouldSatisfy` (\len -> len >= 1 && len <= 4)

    it "handles dominator analysis on disconnected graph" $ do
      let (n1, g1) = addNode "A" emptyGraph
          (n2, g2) = addNode "B" g1
          (n3, g3) = addNode "C" g2
          -- No edges added, graph is disconnected
      
      let doms = dominators n1 g3
      Map.size doms `shouldBe` 1  -- Only entry node dominates itself

  describe "Parser and Lexer Boundary Tests" $ do
    it "handles extremely long single-line code" $ do
      let longExpr = T.pack $ concat $ replicate 1000 "x + "
          input = T.concat ["result = ", longExpr, "1"]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser failed on long line: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 1

    it "handles deeply nested parentheses and brackets" $ do
      let depth = 30
          openParens = T.pack $ replicate depth "([{" 
          closeParens = T.pack $ replicate depth "}])"
          input = T.concat ["x = ", openParens, "1", closeParens]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser failed on nested brackets: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 1

    it "handles mixed quote and string delimiters" $ do
      let input = T.unlines [
                "s1 = 'single'",
                "s2 = \"double\"",
                "s3 = '''triple'\"quotes\"''",
                "s4 = \"\"\"mixed'quotes\"\"\"",
                "s5 = 'escaped\\'quote'",
                "s6 = \"escaped\\\"quote\""
            ]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser failed on mixed quotes: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 6

    it "handles source with only comments and whitespace" $ do
      let input = T.unlines [
                "# This is a comment",
                "   # Indented comment",
                "",
                "   ",
                "# Another comment"
            ]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser failed on comments only: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 0

  describe "Type System Edge Cases" $ do
    it "handles recursive and self-referential types" $ do
      let expr = CEList [noLoc $ CEList [noLoc $ CEList []]]
      case runTypeInference Map.empty (inferType expr) of
        Right inference -> case resultType inference of
          TList (TList (TList _)) -> pure ()
          _ -> expectationFailure "Expected nested list type"
        Left err -> expectationFailure $ "Type inference failed: " <> T.unpack err

    it "handles type inference with many type variables" $ do
      let buildPolyExpr depth =
            if depth <= 0
            then CELiteral (LInt 1)
            else CECall (noLoc (CEVar (Identifier ("f" ++ show depth)))) [noLoc (buildPolyExpr (depth - 1))]
          expr = buildPolyExpr 5
      case runTypeInference Map.empty (inferType expr) of
        Right inference -> resultType inference `shouldSatisfy` (\t -> t /= TError "unknown")
        Left err -> expectationFailure $ "Type inference failed on poly expr: " <> T.unpack err

    it "handles empty and singleton collections" $ do
      let emptyList = CEList []
          singletonList = CEList [noLoc (CELiteral (LInt 42))]
          emptyTuple = CETuple []
          singletonTuple = CETuple [noLoc (CELiteral (LString "single"))]
      
      case runTypeInference Map.empty (inferType emptyList) of
        Right inference -> resultType inference `shouldSatisfy` (\t -> case t of TList _ -> True; _ -> False)
        Left err -> expectationFailure $ "Type inference failed on empty list: " <> T.unpack err
      
      case runTypeInference Map.empty (inferType singletonList) of
        Right inference -> resultType inference `shouldBe` TList (TInt 32)
        Left err -> expectationFailure $ "Type inference failed on singleton list: " <> T.unpack err

  describe "Configuration and CLI Boundary Tests" $ do
    it "handles configuration with extreme numeric values" $ do
      let overrides = emptyOverrides {
                ccoMaxConcurrency = Just 999999,
                ccoOptimizationLevel = Just (OptimizationLevel 100),
                ccoCppStandard = Just "c++99"
            }
          config = mergeConfigs developmentConfig overrides
      ccoMaxConcurrency config `shouldBe` Just 999999
      ccoOptimizationLevel config `shouldBe` Just (OptimizationLevel 100)
      ccoCppStandard config `shouldBe` Just "c++99"

    it "handles configuration merges with conflicting values" $ do
      let overrides1 = emptyOverrides { ccoEnableDebugInfo = Just True }
          overrides2 = emptyOverrides { ccoEnableDebugInfo = Just False }
          config1 = mergeConfigs developmentConfig overrides1
          config2 = mergeConfigs config1 overrides2
      ccoEnableDebugInfo config2 `shouldBe` Just False

    it "handles empty and malformed command line arguments" $ do
      case parseCommandLineArgs [] of
        Right CLICommandShowHelp -> pure ()  -- Expected
        Right _ -> expectationFailure "Expected CLICommandShowHelp"
        Left err -> expectationFailure $ "CLI should handle empty args: " <> err

  describe "Memory and Performance Boundaries" $ do
    it "handles large data structures without stack overflow" $ do
      let buildLargeAST n = if n <= 0
                then CELiteral (LInt 0)
                else CEBinaryOp OpAdd (noLoc (buildLargeAST (n - 1))) (noLoc (CELiteral (LInt 1)))
          largeAST = buildLargeAST 50
      
      result <- try (evaluate $ force largeAST)
      case result of
        Right _ -> pure ()  -- Success
        Left (_ :: SomeException) -> expectationFailure "Stack overflow or memory error"

    it "handles repeated transformations on large expressions" $ do
      let buildExpr n = if n <= 0
                then CELiteral (LInt 1)
                else CEList [noLoc (buildExpr (n - 1))]
          expr = buildExpr 15
          transform e = case e of
                CEList xs -> CEList (map (fmap transform) xs)
                other -> other
      
      result <- try (evaluate $ force $ transform expr)
      case result of
        Right _ -> pure ()  -- Success
        Left (_ :: SomeException) -> expectationFailure "Transformation failed"

-- Helper functions
parseModuleFrom :: T.Text -> Either String PythonModule
parseModuleFrom source =
  case runPythonLexer (T.pack "test.py") source of
    Left err -> Left (show err)
    Right tokens ->
      case runPythonParser (T.pack "test.py") tokens of
        Left perr -> Left (show perr)
        Right (PythonAST modu) -> Right modu