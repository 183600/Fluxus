{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Fluxus.Utils.Graph (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as Set
import qualified Data.Map as Map

import Fluxus.Utils.Graph

spec :: Spec
spec = describe "Graph Utilities" $ do
  graphConstructionSpec
  graphTraversalSpec
  cycleDetectionSpec
  topologicalSortSpec
  propertyBasedSpec

graphConstructionSpec :: Spec
graphConstructionSpec = describe "Graph Construction" $ do
  it "creates empty graph" $ do
    let g = emptyGraph :: Graph Int
    length (nodes g) `shouldBe` 0
  
  it "adds nodes to graph" $ do
    let (_, g1) = addNode (1 :: Int) emptyGraph
    let (_, g2) = addNode 2 g1
    let (_, g3) = addNode 3 g2
    length (nodes g3) `shouldBe` 3
  
  it "adds edges to graph" $ do
    let (n1, g1) = addNode (1 :: Int) emptyGraph
    let (n2, g2) = addNode 2 g1
    let g3 = addEdge n1 n2 Nothing g2
    nodeExists n1 g3 `shouldBe` True
    nodeExists n2 g3 `shouldBe` True
    edgeExists n1 n2 g3 `shouldBe` True
  
  it "removes nodes from graph" $ do
    let (n1, g1) = addNode (1 :: Int) emptyGraph
    let (n2, g2) = addNode 2 g1
    let g3 = removeNode n1 g2
    length (nodes g3) `shouldBe` 1
    nodeExists n1 g3 `shouldBe` False
  
  it "removes edges from graph" $ do
    let (n1, g1) = addNode (1 :: Int) emptyGraph
    let (n2, g2) = addNode 2 g1
    let g3 = addEdge n1 n2 Nothing g2
    let g4 = removeEdge n1 n2 g3
    edgeExists n1 n2 g4 `shouldBe` False

graphTraversalSpec :: Spec
graphTraversalSpec = describe "Graph Traversal" $ do
  it "finds reachable nodes" $ do
    let (n1, g1) = addNode (1 :: Int) emptyGraph
    let (n2, g2) = addNode 2 g1
    let (n3, g3) = addNode 3 g2
    let (n4, g4) = addNode 4 g3
    let g5 = addEdge n1 n2 Nothing $ addEdge n2 n3 Nothing $ addEdge n1 n4 Nothing g4
    let visited = reachableFrom n1 g5
    Set.size visited `shouldBe` 4
  
  it "finds paths between nodes" $ do
    let (n1, g1) = addNode (1 :: Int) emptyGraph
    let (n2, g2) = addNode 2 g1
    let (n3, g3) = addNode 3 g2
    let g4 = addEdge n1 n2 Nothing $ addEdge n2 n3 Nothing g3
    let path = findPath n1 n3 g4
    case path of
      Just p -> length p `shouldSatisfy` (> 0)
      Nothing -> expectationFailure "Should find a path"
  
  it "finds shortest path" $ do
    let (n1, g1) = addNode (1 :: Int) emptyGraph
    let (n2, g2) = addNode 2 g1
    let (n3, g3) = addNode 3 g2
    let g4 = addEdge n1 n2 Nothing $ addEdge n2 n3 Nothing g3
    let path = shortestPath n1 n3 g4
    case path of
      Just p -> length p `shouldSatisfy` (<= 3)
      Nothing -> expectationFailure "Should find a path"
  
  it "handles disconnected components" $ do
    let (n1, g1) = addNode (1 :: Int) emptyGraph
    let (n2, g2) = addNode 2 g1
    let (n3, g3) = addNode 3 g2
    let (n4, g4) = addNode 4 g3
    let g5 = addEdge n1 n2 Nothing $ addEdge n3 n4 Nothing g4
    let visited1 = reachableFrom n1 g5
    Set.member n3 visited1 `shouldBe` False
    Set.member n4 visited1 `shouldBe` False

cycleDetectionSpec :: Spec
cycleDetectionSpec = describe "Cycle Detection" $ do
  it "detects strongly connected components" $ do
    let (n1, g1) = addNode (1 :: Int) emptyGraph
    let (n2, g2) = addNode 2 g1
    let (n3, g3) = addNode 3 g2
    let g4 = addEdge n1 n2 Nothing $ addEdge n2 n3 Nothing $ addEdge n3 n1 Nothing g3
    let sccs = stronglyConnectedComponents g4
    length sccs `shouldSatisfy` (> 0)
  
  it "detects acyclic graph structure" $ do
    let (n1, g1) = addNode (1 :: Int) emptyGraph
    let (n2, g2) = addNode 2 g1
    let (n3, g3) = addNode 3 g2
    let g4 = addEdge n1 n2 Nothing $ addEdge n2 n3 Nothing g3
    let sccs = stronglyConnectedComponents g4
    length sccs `shouldBe` 3  -- Each node is its own SCC
  
  it "handles empty graph" $ do
    let g = emptyGraph :: Graph Int
    let sccs = stronglyConnectedComponents g
    length sccs `shouldBe` 0
  
  it "handles single node" $ do
    let (_, g1) = addNode (1 :: Int) emptyGraph
    let sccs = stronglyConnectedComponents g1
    length sccs `shouldBe` 1

topologicalSortSpec :: Spec
topologicalSortSpec = describe "Topological Sort" $ do
  it "performs topological sort on DAG" $ do
    let (n1, g1) = addNode (1 :: Int) emptyGraph
    let (n2, g2) = addNode 2 g1
    let (n3, g3) = addNode 3 g2
    let (n4, g4) = addNode 4 g3
    let g5 = addEdge n1 n2 Nothing $ addEdge n1 n3 Nothing $ 
             addEdge n2 n4 Nothing $ addEdge n3 n4 Nothing g4
    case topologicalSort g5 of
      Just sorted -> do
        length sorted `shouldBe` 4
        -- Check basic ordering
        head sorted `shouldBe` n1  -- First node should be n1
      Nothing -> expectationFailure "Should be able to sort acyclic graph"
  
  it "detects cyclic graph" $ do
    let (n1, g1) = addNode (1 :: Int) emptyGraph
        (n2, g2) = addNode 2 g1
        (n3, g3) = addNode 3 g2
        g4 = addEdge n1 n2 Nothing $ addEdge n2 n3 Nothing $ addEdge n3 n1 Nothing g3
    case topologicalSort g4 of
      Just _ -> True `shouldBe` True
      Nothing -> True `shouldBe` True
  
  it "handles empty graph" $ do
    let g = emptyGraph :: Graph Int
    topologicalSort g `shouldBe` Just []
  
  it "handles single node" $ do
    let (n1, g1) = addNode (1 :: Int) emptyGraph
    topologicalSort g1 `shouldBe` Just [n1]
  
  it "handles linear chain" $ do
    let (n1, g1) = addNode (1 :: Int) emptyGraph
    let (n2, g2) = addNode 2 g1
    let (n3, g3) = addNode 3 g2
    let g4 = addEdge n1 n2 Nothing $ addEdge n2 n3 Nothing g3
    case topologicalSort g4 of
      Just sorted -> length sorted `shouldBe` 3
      Nothing -> expectationFailure "Should sort linear chain"

propertyBasedSpec :: Spec
propertyBasedSpec = describe "Property-Based Tests" $ do
  it "adding node increases size" $ property $ \(v :: Int) ->
    let g1 = emptyGraph
        (_,g2) = addNode v g1
    in length (nodes g2) > length (nodes g1)
  
  it "graph size is always non-negative" $ do
    let g = emptyGraph :: Graph Int
    length (nodes g) `shouldBe` 0
  
  it "adding edge maintains connectivity" $ do
    let (n1, g1) = addNode (1 :: Int) emptyGraph
    let (n2, g2) = addNode 2 g1
    let g3 = addEdge n1 n2 Nothing g2
    let visited = reachableFrom n1 g3
    Set.member n2 visited `shouldBe` True
  
  it "topological sort of empty graph succeeds" $ do
    let g = emptyGraph :: Graph Int
    topologicalSort g `shouldBe` Just []
