{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Fluxus.ExtendedBoundaryTests (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Exception (try, SomeException)
import Data.List (replicate)
import Data.Maybe (fromMaybe)

import Fluxus.Compiler.Config
import Fluxus.Utils.Graph
import Fluxus.Analysis.TypeInference
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.AST.Common
import Fluxus.AST.Python

spec :: Spec
spec = describe "Extended Boundary Tests" $ do
  describe "Configuration System - Edge Cases" $ do
    it "handles parsing empty command line arguments" $ do
      case parseCommandLineArgs [] of
        Right (CLICommandModify _ files) -> files `shouldBe` []
        Left err -> expectationFailure $ "Should handle empty args: " ++ err

    it "handles extremely long file paths in command line" $ do
      let longPath = "/" <> concat (replicate 1000 "deep/") <> "file.py"
      case parseCommandLineArgs [longPath] of
        Right (CLICommandModify _ files) -> length (head files) `shouldBe` length longPath
        Left err -> expectationFailure $ "Should handle long paths: " ++ err

    it "handles conflicting boolean flags" $ do
      case parseCommandLineArgs ["--enable-debug", "--disable-debug", "--enable-debug"] of
        Right (CLICommandModify modifier _) -> do
          let config = modifier defaultConfig
          ccEnableDebugInfo config `shouldBe` True
        Left err -> expectationFailure $ "Should handle conflicting flags: " ++ err

    it "handles numeric overflow in concurrency settings" $ do
      case parseCommandLineArgs ["--max-concurrency", "999999999999999999"] of
        Left _ -> pure ()  -- Expected to fail or handle gracefully
        Right (CLICommandModify modifier _) -> do
          let config = modifier defaultConfig
          ccMaxConcurrency config `shouldSatisfy` (>= 0)

    it "handles malformed target platform" $ do
      case parseCommandLineArgs ["--target", "invalid-platform-name"] of
        Left _ -> pure ()  -- Expected to fail
        Right _ -> expectationFailure "Should reject invalid platform"

    it "handles empty configuration file content" $ do
      let emptyOverrides = emptyOverrides
      let merged = mergeConfigs defaultConfig emptyOverrides
      merged `shouldBe` defaultConfig

    it "handles configuration with all Nothing values" $ do
      let allNothing = CompilerConfigOverrides
            { ccoSourceLanguage = Nothing
            , ccoOptimizationLevel = Nothing
            , ccoTargetPlatform = Nothing
            , ccoOutputPath = Nothing
            , ccoEnableInterop = Nothing
            , ccoEnableDebugInfo = Nothing
            , ccoEnableProfiler = Nothing
            , ccoEnableParallel = Nothing
            , ccoMaxConcurrency = Nothing
            , ccoIncludePaths = Nothing
            , ccoLibraryPaths = Nothing
            , ccoLinkedLibraries = Nothing
            , ccoCppStandard = Nothing
            , ccoCppCompiler = Nothing
            , ccoVerboseLevel = Nothing
            , ccoWorkDirectory = Nothing
            , ccoKeepIntermediates = Nothing
            , ccoStrictMode = Nothing
            , ccoEnableAnalysis = Nothing
            , ccoEnableExperimentalOptimizations = Nothing
            , ccoStopAtCodegen = Nothing
            , ccoSkipCompilerCheck = Nothing
            }
      let merged = mergeConfigs defaultConfig allNothing
      merged `shouldBe` defaultConfig

  describe "Graph Algorithms - Extreme Cases" $ do
    it "handles topological sort on empty graph" $ do
      topologicalSort emptyGraph `shouldBe` Just []

    it "handles topological sort on single node graph" $ do
      let (nodeId, graph) = addNode "single" emptyGraph
      topologicalSort graph `shouldBe` Just [nodeId]

    it "handles topological sort on graph with self-loop" $ do
      let (n1, g1) = addNode "A" emptyGraph
          g2 = addEdge n1 n1 Nothing g1
      topologicalSort g2 `shouldBe` Nothing

    it "handles topological sort on large acyclic graph" $ do
      let buildGraph n g = if n <= 0 then g else
            let (_, g1) = addNode ("node" ++ show n) g
            in buildGraph (n-1) g1
          largeGraph = buildGraph 100 emptyGraph
      topologicalSort largeGraph `shouldSatisfy` (\maybeSorted ->
        case maybeSorted of
          Just sorted -> length sorted == 100
          Nothing -> False)

    it "handles strongly connected components on empty graph" $ do
      stronglyConnectedComponents emptyGraph `shouldBe` []

    it "handles strongly connected components on single node" $ do
      let (n1, g1) = addNode "A" emptyGraph
      let sccs = stronglyConnectedComponents g1
      length sccs `shouldBe` 1

    it "handles dominators with non-existent entry node" $ do
      let (_, g1) = addNode "A" emptyGraph
          (_, g2) = addNode "B" g1
          doms = dominators 999 g2  -- Non-existent node
      Map.size doms `shouldBe` 0

    it "handles shortest path in empty graph" $ do
      shortestPath 0 1 emptyGraph `shouldBe` Nothing

    it "handles shortest path between same node" $ do
      let (n1, g1) = addNode "A" emptyGraph
      shortestPath n1 n1 g1 `shouldBe` Just [n1]

    it "handles reachableFrom with negative node ID" $ do
      let (n1, g1) = addNode "A" emptyGraph
          g2 = addEdge n1 n1 Nothing g1
      let reachable = reachableFrom (-1) g2
      Set.size reachable `shouldBe` 1

  describe "Parser - Extreme Input Cases" $ do
    it "handles parsing extremely deeply nested expressions" $ do
      let depth = 50
          input = T.pack $ "x = " ++ replicate depth '(' ++ "1" ++ replicate depth ')'
      case runPythonLexer "deep.py" input of
        Left err -> expectationFailure $ "Lexer failed on deep nesting: " ++ show err
        Right tokens -> length tokens `shouldSatisfy` (> 0)

    it "handles parsing extremely long identifier names" $ do
      let longName = T.pack $ "a" ++ replicate 500 "verylong"
          input = T.concat [longName, " = 42"]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser failed on long identifier: " ++ err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 1

    it "handles parsing code with only comments" $ do
      let input = T.unlines ["# Comment 1", "# Comment 2", "# Comment 3"]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser failed on comments only: " ++ err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 0

    it "handles parsing mixed quote types in strings" $ do
      let input = T.unlines
            [ "s1 = 'single'"
            , "s2 = \"double\""
            , "s3 = '''triple'''"
            , "s4 = \"\"\"triple\"\"\""
            ]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser failed on mixed quotes: " ++ err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 4

    it "handles parsing empty string literals" $ do
      let input = T.unlines ["s1 = ''", "s2 = \"\""]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser failed on empty strings: " ++ err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 2

    it "handles parsing numeric literals at extremes" $ do
      let input = T.unlines
            [ "zero = 0"
            , "negative = -999999"
            , "float_small = 0.000001"
            , "float_large = 999999.999999"
            ]
      case parseModuleFrom input of
        Left err -> expectationFailure $ "Parser failed on numeric extremes: " ++ err
        Right module_ -> length (pyModuleBody module_) `shouldBe` 4

  describe "Type Inference - Complex Scenarios" $ do
    it "handles type inference with empty environment" $ do
      let expr = CELiteral (LInt 42)
      case runTypeInference Map.empty (inferType expr) of
        Right inference -> resultType inference `shouldBe` TInt 32
        Left err -> expectationFailure $ "Type inference failed: " ++ T.unpack err

    it "handles type inference with deeply nested conditionals" $ do
      let buildNestedIf depth =
            if depth <= 0
            then CELiteral (LInt 1)
            else CEIf (noLoc (CELiteral (LBool True)))
                       (noLoc (buildNestedIf (depth - 1)))
                       (noLoc (buildNestedIf (depth - 1)))
          expr = buildNestedIf 10
      case runTypeInference Map.empty (inferType expr) of
        Right inference -> resultType inference `shouldBe` TInt 32
        Left err -> expectationFailure $ "Type inference failed on nested conditionals: " ++ T.unpack err

    it "handles type inference with extremely large tuples" $ do
      let largeTuple = CETuple $ replicate 50 (noLoc (CELiteral (LInt 1)))
      case runTypeInference Map.empty (inferType largeTuple) of
        Right inference -> case resultType inference of
          TTuple types -> length types `shouldBe` 50
          _ -> expectationFailure "Expected tuple type"
        Left err -> expectationFailure $ "Type inference failed on large tuple: " ++ T.unpack err

    it "handles type inference with recursive type references" $ do
      let expr = CEList [noLoc (CEList [])]
      case runTypeInference Map.empty (inferType expr) of
        Right inference -> case resultType inference of
          TList (TList _) -> pure ()
          _ -> expectationFailure "Expected nested list type"
        Left err -> expectationFailure $ "Type inference failed: " ++ T.unpack err

    it "handles type checking with conflicting types in branches" $ do
      let expr = CEIf (noLoc (CELiteral (LBool True)))
                     (noLoc (CELiteral (LInt 1)))
                     (noLoc (CELiteral (LString "error")))
      case runTypeInference Map.empty (inferType expr) of
        Right _ -> expectationFailure "Should detect type conflict in branches"
        Left _ -> pure ()  -- Expected to fail

    it "handles type inference with empty collections" $ do
      let emptyList = CEList []
          emptyTuple = CETuple []
      case runTypeInference Map.empty (inferType emptyList) of
        Right inference -> resultType inference `shouldBe` TList TUnknown
        Left err -> expectationFailure $ "Type inference failed on empty list: " ++ T.unpack err

      case runTypeInference Map.empty (inferType emptyTuple) of
        Right inference -> resultType inference `shouldBe` TTuple []
        Left err -> expectationFailure $ "Type inference failed on empty tuple: " ++ T.unpack err

-- Helper functions
parseModuleFrom :: T.Text -> Either String PythonModule
parseModuleFrom source =
  case runPythonLexer (T.pack "test.py") source of
    Left err -> Left (show err)
    Right tokens ->
      case runPythonParser (T.pack "test.py") tokens of
        Left perr -> Left (show perr)
        Right (PythonAST modu) -> Right modu

defaultConfig :: CompilerConfig
defaultConfig = developmentConfig

noLoc :: a -> Located a
noLoc = Located (SourcePos "" 0 0)
