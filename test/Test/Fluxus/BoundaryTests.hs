{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.BoundaryTests (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Exception (try, SomeException)
import Data.List (replicate)

import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.Analysis.TypeInference
import Fluxus.AST.Common
import Fluxus.Compiler.Config (parseCommandLineArgs, CLICommand(..))
import Fluxus.AST.Python (PythonModule, pyModuleBody, PythonAST(..))
import Fluxus.AST.Common (Located, locatedValue)
import Fluxus.Utils.Graph
       ( Graph, Node(..), NodeId, Edge(..), emptyGraph, addNode, addEdge, nodes,
         edges, edgeExists, topologicalSort, dominators, shortestPath,
         reachableFrom, findPath )

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
        Right (CLICommandModify _ configPaths) -> null configPaths `shouldBe` True  -- Empty args => default config, no explicit config files
        Right CLICommandShowHelp -> expectationFailure "CLI should treat empty args as default config, not help"
        Right (CLICommandShowVersion _) -> expectationFailure "CLI should not show version for empty args"
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

  describe "Additional Boundary Tests - Graph Algorithms" $ do
    it "handles empty graph in topological sort" $ do
      topologicalSort emptyGraph `shouldBe` Just []

    it "handles single node graph in topological sort" $ do
      let (nodeId, graph) = addNode "single" emptyGraph
      topologicalSort graph `shouldBe` Just [nodeId]

    it "handles graph with self-loop in topological sort" $ do
      let (nodeId, graph1) = addNode "self-loop" emptyGraph
          graph2 = addEdge nodeId nodeId Nothing graph1
      topologicalSort graph2 `shouldBe` Nothing

    it "handles very long path in topological sort" $ do
      let buildChain n g = if n <= 0 then (0, g) else
            let (prevId, g1) = buildChain (n-1) g
                (currId, g2) = addNode ("node" ++ show n) g1
                g3 = if prevId == currId then g2 else addEdge prevId currId Nothing g2
            in (currId, g3)
          (_, graph) = buildChain 100 emptyGraph
      topologicalSort graph `shouldBe` Just (map nodeId (nodes graph))

    it "handles graph with multiple disconnected components" $ do
      let (n1, g1) = addNode "A" emptyGraph
          (n2, g2) = addNode "B" g1
          (n3, g3) = addNode "C" g2
          graph = addEdge n1 n2 Nothing g3
      length (nodes graph) `shouldBe` 3
      edgeExists n2 n3 graph `shouldBe` False

    it "handles dominator computation with missing entry node" $ do
      let (n1, g1) = addNode "A" emptyGraph
          (n2, g2) = addNode "B" g1
          graph = addEdge n1 n2 Nothing g2
      let doms = dominators 999 graph  -- Non-existent entry node
      Map.size doms `shouldBe` 0

    it "handles shortest path in disconnected graph" $ do
      let (n1, g1) = addNode "A" emptyGraph
          (n2, g2) = addNode "B" g1
          (n3, g3) = addNode "C" g2
          graph = addEdge n1 n2 Nothing g3
      shortestPath n1 n3 graph `shouldBe` Nothing

    it "handles reachableFrom with non-existent start node" $ do
      let (n1, g1) = addNode "A" emptyGraph
          (n2, g2) = addNode "B" g1
          graph = addEdge n1 n2 Nothing g2
      let reachable = reachableFrom 999 graph  -- Non-existent start node
      -- reachableFrom includes the start node itself, even if it doesn't exist in graph
      length (Set.toList reachable) `shouldBe` 1

    it "handles findPath with same start and end node" $ do
      let (n1, g1) = addNode "A" emptyGraph
      findPath n1 n1 g1 `shouldBe` Just [n1]

    it "handles graph with duplicate edges" $ do
      let (n1, g1) = addNode "A" emptyGraph
          (n2, g2) = addNode "B" g1
          g3 = addEdge n1 n2 (Just "label1") g2
          g4 = addEdge n1 n2 (Just "label2") g3
          g5 = addEdge n1 n2 Nothing g4
      length (edges g5) `shouldBe` 3

-- Helper function to parse module from text
parseModuleFrom :: T.Text -> Either String PythonModule
parseModuleFrom source =
  case runPythonLexer (T.pack "test.py") source of
    Left err -> Left (show err)
    Right tokens ->
      case runPythonParser (T.pack "test.py") tokens of
        Left perr -> Left (show perr)
        Right (PythonAST modu) -> Right modu
