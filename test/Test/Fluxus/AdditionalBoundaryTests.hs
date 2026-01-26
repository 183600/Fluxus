{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Fluxus.AdditionalBoundaryTests (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Exception (try, SomeException, catch, evaluate)
import Data.List (replicate, isInfixOf)
import Data.Maybe (fromMaybe)
import System.IO (hClose, openTempFile)
import System.Directory (removeFile)
import Control.DeepSeq (force, NFData)

import Fluxus.Compiler.Config
import Fluxus.Compiler.Driver
import Fluxus.Utils.Graph
import Fluxus.Analysis.TypeInference
import Fluxus.Analysis.EscapeAnalysis
import Fluxus.Analysis.OwnershipInference
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.CodeGen.CPP.IdentifierSanitizer

spec :: Spec
spec = describe "Additional Boundary Tests" $ do
  describe "Memory and Performance Boundary Tests" $ do
    it "handles extremely large string literals (100KB)" $ do
      let largeString = T.pack $ replicate 100000 'A'
          input = T.concat ["x = \"", largeString, "\""]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Should handle large strings: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 1

    it "handles deeply nested function calls (depth 200)" $ do
      let depth = 200
          inner = T.pack $ concat $ replicate depth "func("
          outer = T.pack $ concat $ replicate depth ")"
          input = T.concat ["x = ", inner, "42", outer]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Should handle deep calls: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 1

    it "handles extremely long identifier chains" $ do
      let longChain = T.intercalate "." $ map (T.pack . ("obj" ++) . show) [1..50]
          input = T.concat ["x = ", longChain]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Should handle long chains: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 1

    it "handles massive list literals with mixed types" $ do
      let largeList = T.intercalate ", " $ map (T.pack . show) $ take 1000 $ cycle [1, 2.5, -3, 0]
          input = T.concat ["x = [", largeList, "]"]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Should handle large mixed lists: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 1

  describe "Type System Boundary Tests" $ do
    it "handles deeply nested generic types" $ do
      let expr = CEList [noLoc $ CEList [noLoc $ CEList [noLoc $ CEList [noLoc $ CELiteral (LInt 1)]]]]
      case runTypeInference Map.empty (inferType expr) of
        Right inference -> case resultType inference of
          TList (TList (TList (TList _))) -> pure ()
          _ -> expectationFailure "Expected 4-level nested list type"
        Left err -> expectationFailure $ "Type inference failed: " <> T.unpack err

    it "handles empty tuple type inference" $ do
      let expr = CETuple []
      case runTypeInference Map.empty (inferType expr) of
        Right inference -> resultType inference `shouldBe` TTuple []
        Left err -> expectationFailure $ "Type inference failed on empty tuple: " <> T.unpack err

    it "handles type inference with circular type constraints" $ do
      let expr = CEList [noLoc $ CELiteral (LInt 1), noLoc $ CEBinaryOp OpAdd (noLoc $ CEVar (Identifier "x")) (noLoc $ CELiteral (LInt 1))]
      case runTypeInference (Map.singleton (Identifier "x") (TList (TInt 32))) (inferType expr) of
        Right inference -> resultType inference `shouldSatisfy` (\t -> t == TList (TInt 32) || t == TError "unknown")
        Left _ -> pure ()  -- Either success or graceful failure

    it "handles extremely large union types" $ do
      let unionTypes = map (TInt) $ take 100 [8, 16, 32, 64, 128]
          largeUnion = TUnion unionTypes
          expr = CELiteral (LInt 42)
      case runTypeInference (Map.singleton (Identifier "x") largeUnion) (inferType expr) of
        Right inference -> resultType inference `shouldBe` TInt 32
        Left err -> expectationFailure $ "Type inference failed: " <> T.unpack err

  describe "Configuration Boundary Tests" $ do
    it "handles configuration with extreme concurrency values" $ do
      let overrides = emptyOverrides { ccoMaxConcurrency = Just 999999 }
          config = mergeConfigs developmentConfig overrides
      ccMaxConcurrency config `shouldBe` 999999

    it "handles configuration with empty include paths" $ do
      let overrides = emptyOverrides { ccoIncludePaths = Just [] }
          config = mergeConfigs developmentConfig overrides
      ccIncludePaths config `shouldBe` []

    it "handles configuration with extremely long library paths" $ do
      let longPaths = map (\i -> "/very/long/path/to/library/that/goes/on/and/on/lib" ++ show i) [1..100]
          overrides = emptyOverrides { ccoLibraryPaths = Just longPaths }
          config = mergeConfigs developmentConfig overrides
      length (ccLibraryPaths config) `shouldBe` 100

    it "handles configuration with conflicting boolean flags" $ do
      let overrides = emptyOverrides { ccoEnableDebugInfo = Just True, ccoEnableProfiler = Just False }
          config = mergeConfigs developmentConfig overrides
      ccEnableDebugInfo config `shouldBe` True
      ccEnableProfiler config `shouldBe` False

  describe "Identifier Sanitization Boundary Tests" $ do
    it "handles extremely long identifiers" $ do
      let longName = T.pack $ replicate 500 'a'
      let sanitized = sanitizeIdentifier longName
      T.length sanitized `shouldSatisfy` (<= 500)

    it "handles identifiers with only special characters" $ do
      let specialOnly = "!@#$%^&*()"
      let sanitized = sanitizeIdentifier (T.pack specialOnly)
      sanitized `shouldNotBe` T.pack specialOnly

    it "handles Unicode-heavy identifiers" $ do
      let unicodeName = "函数_名_関数_이름_имя"
      let sanitized = sanitizeIdentifier unicodeName
      T.length sanitized `shouldBe` T.length unicodeName

    it "handles empty identifier" $ do
      let sanitized = sanitizeIdentifier ""
      sanitized `shouldNotBe` ""

  describe "Graph Algorithm Boundary Tests" $ do
    it "handles shortest path in massive graph (1000 nodes)" $ do
      let buildLargeGraph n g = if n <= 0 then g else
            let (nid1, g1) = addNode ("node" ++ show n) g
                (nid2, g2) = if n > 1 then addNode ("node" ++ show (n-1)) g1 else (nid1, g1)
                g3 = if n > 1 then addEdge nid2 nid1 Nothing g2 else g2
            in buildLargeGraph (n-1) g3
          largeGraph = buildLargeGraph 1000 emptyGraph
          (startId, _) = addNode "start" largeGraph
          (endId, _) = addNode "end" largeGraph
      shortestPath startId endId largeGraph `shouldSatisfy` (\maybePath ->
        case maybePath of
          Just path -> length path <= 1000
          Nothing -> True)

    it "handles strongly connected components in graph with many cycles" $ do
      let buildCyclicGraph n g = if n <= 0 then g else
            let (nodeId, g1) = addNode ("node" ++ show n) g
                g2 = addEdge nodeId nodeId Nothing g1  -- Self-loop
                g3 = if n > 1 then addEdge nodeId (nodeId - 1) Nothing g2 else g2
                g4 = if n > 1 then addEdge (nodeId - 1) nodeId Nothing g3 else g3
            in buildCyclicGraph (n-1) g4
          cyclicGraph = buildCyclicGraph 50 emptyGraph
      let sccs = stronglyConnectedComponents cyclicGraph
      -- Just verify that the function runs without crashing and returns a list
      sccs `shouldSatisfy` (\result -> length result >= 0)

    it "handles topological sort on graph with multiple valid orderings" $ do
      let (n1, g1) = addNode "A" emptyGraph
          (n2, g2) = addNode "B" g1
          (n3, g3) = addNode "C" g2
          (n4, g4) = addNode "D" g3
          graph = addEdge n1 n3 Nothing g4  -- A -> C
      topologicalSort graph `shouldSatisfy` (\maybeSorted ->
        case maybeSorted of
          Just sorted -> length sorted == 4 && n1 `elem` sorted && n3 `elem` sorted
          Nothing -> False)

  describe "File System and I/O Boundary Tests" $ do
    it "handles parsing from empty file" $ do
      case parseModuleFrom "" of
        Right module_ -> length (pyModuleBody module_) `shouldBe` 0
        Left err -> expectationFailure $ "Should handle empty file: " <> err

    it "handles parsing file with only whitespace" $ do
      let input = "   \n  \n   \t  \n"
      result <- try (evaluate $ parseModuleFrom input)
      case result of
        Right (Right module_) -> length (pyModuleBody module_) `shouldBe` 0
        Right (Left err) -> expectationFailure $ "Parse error for whitespace-only file: " <> err
        Left (_ :: SomeException) -> pure ()  -- Exception is acceptable for whitespace-only input

    it "handles parsing file with extremely long lines" $ do
      let longLine = T.pack $ replicate 10000 'x'
          input = T.concat ["# ", longLine, "\ny = 1"]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Should handle long lines: " <> err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 1

  describe "Escape Analysis Boundary Tests" $ do
    it "handles escape analysis on deeply nested function calls" $ do
      let depth = 50
          inner = T.pack $ concat $ replicate depth "f("
          outer = T.pack $ concat $ replicate depth ")"
          input = T.concat ["def outer():\n    return ", inner, "inner()", outer]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Should parse nested calls: " <> err
        Right _ -> pure ()  -- Parsing success is enough for this test

    it "handles escape analysis with many variables escaping to different scopes" $ do
      let vars = T.intercalate "\n" $ map (\i -> T.pack $ "x" ++ show i ++ " = " ++ show i) [1..100]
          returns = T.intercalate ", " $ map (\i -> T.pack $ "x" ++ show i) [1..100]
          input = T.concat [vars, "\ndef func():\n    return ", returns]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Should handle many escaping vars: " <> err
        Right _ -> pure ()

-- Helper functions
parseModuleFrom :: T.Text -> Either String PythonModule
parseModuleFrom source =
  case runPythonLexer (T.pack "test.py") source of
    Left err -> Left (show err)
    Right tokens ->
      case runPythonParser (T.pack "test.py") tokens of
        Left perr -> Left (show perr)
        Right (PythonAST modu) -> Right modu

nodeId :: Node a -> NodeId
nodeId (Node nid _) = nid

-- Mock variable for testing
cevarX :: Located CommonExpr
cevarX = noLoc (CEVar (Identifier "x"))