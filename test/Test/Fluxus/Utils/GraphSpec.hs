module Test.Fluxus.Utils.GraphSpec (spec) where

import Data.List (sort)
import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Fluxus.Utils.Graph
  ( addEdge
  , addNode
  , buildCFG
  , buildDominatorTree
  , cfgEntry
  , cfgExit
  , dominatorFrontier
  , dominators
  , edges
  , emptyGraph
  , findPath
  , graphToDot
  , immediateDominator
  , neighbors
  , nodeExists
  , nodeId
  , nodes
  , postDominators
  , reachableFrom
  , removeEdge
  , removeNode
  , shortestPath
  , stronglyConnectedComponents
  , successors
  , predecessors
  , edgeExists
  , topologicalSort
  )

spec :: Spec
spec = do
  describe "Fluxus.Utils.Graph construction and queries" $ do
    it "addNode assigns sequential ids and addEdge connects nodes" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          g3 = addEdge aId bId (Just (T.pack "e1")) g2
      nodeExists aId g3 `shouldBe` True
      nodeExists bId g3 `shouldBe` True
      edgeExists aId bId g3 `shouldBe` True
      edgeExists bId aId g3 `shouldBe` False
      successors aId g3 `shouldBe` [bId]
      predecessors bId g3 `shouldBe` [aId]

    it "removeNode removes node and incident edges" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          g3 = addEdge aId bId Nothing g2
          g4 = removeNode bId g3
      nodeExists bId g4 `shouldBe` False
      edgeExists aId bId g4 `shouldBe` False
      nodeExists aId g4 `shouldBe` True

    it "removeEdge removes only the specified edge" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          g3 = addEdge aId bId Nothing . addEdge bId aId Nothing $ g2
          g4 = removeEdge aId bId g3
      edgeExists aId bId g4 `shouldBe` False
      edgeExists bId aId g4 `shouldBe` True

  describe "Fluxus.Utils.Graph.topologicalSort" $ do
    it "orders nodes respecting dependencies for acyclic graphs" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          (cId, g3) = addNode "C" g2
          g4 = addEdge aId bId Nothing g3
          g5 = addEdge bId cId Nothing g4
      topologicalSort g5 `shouldBe` Just [aId, bId, cId]

    it "detects cycles in the graph" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          g3 = addEdge aId bId Nothing g2
          g4 = addEdge bId aId Nothing g3
      topologicalSort g4 `shouldBe` Nothing

  describe "Fluxus.Utils.Graph.buildDominatorTree" $ do
    it "computes immediate dominators for converging control flow" $ do
      let (entryId, g1) = addNode "entry" emptyGraph
          (leftId, g2) = addNode "left" g1
          (rightId, g3) = addNode "right" g2
          (exitId, g4) = addNode "exit" g3
          g5 = addEdge entryId leftId Nothing g4
          g6 = addEdge entryId rightId Nothing g5
          g7 = addEdge leftId exitId Nothing g6
          g8 = addEdge rightId exitId Nothing g7
          domTree = buildDominatorTree entryId g8
          dominatorIds = map nodeId (nodes domTree)
      dominatorIds `shouldMatchList` [entryId, leftId, rightId, exitId]
      immediateDominator entryId domTree `shouldBe` Nothing
      immediateDominator leftId domTree `shouldBe` Just entryId
      immediateDominator rightId domTree `shouldBe` Just entryId
      immediateDominator exitId domTree `shouldBe` Just entryId

  describe "Fluxus.Utils.Graph.stronglyConnectedComponents" $ do
    it "returns each node as its own component for acyclic graph" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          (cId, g3) = addNode "C" g2
          g4 = addEdge aId bId Nothing . addEdge bId cId Nothing $ g3
      stronglyConnectedComponents g4 `shouldMatchList` [[aId], [bId], [cId]]

    it "identifies a single SCC for a 2-cycle" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          g3 = addEdge aId bId Nothing . addEdge bId aId Nothing $ g2
      sort (map sort (stronglyConnectedComponents g3)) `shouldBe` sort (map sort [[aId, bId]])

  describe "Fluxus.Utils.Graph.reachableFrom" $ do
    it "returns only start node when no outgoing edges" $ do
      let (aId, g1) = addNode "A" emptyGraph
      reachableFrom aId g1 `shouldBe` Set.singleton aId

    it "returns all nodes in a chain" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          (cId, g3) = addNode "C" g2
          g4 = addEdge aId bId Nothing . addEdge bId cId Nothing $ g3
      reachableFrom aId g4 `shouldBe` Set.fromList [aId, bId, cId]

  describe "Fluxus.Utils.Graph.findPath and shortestPath" $ do
    it "findPath returns path when one exists" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          (cId, g3) = addNode "C" g2
          g4 = addEdge aId bId Nothing . addEdge bId cId Nothing $ g3
      findPath aId cId g4 `shouldBe` Just [aId, bId, cId]

    it "findPath returns Nothing when no path exists" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          g3 = addEdge aId bId Nothing g2
      findPath bId aId g3 `shouldBe` Nothing

    it "shortestPath returns minimal path" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          (cId, g3) = addNode "C" g2
          g4 = addEdge aId bId Nothing . addEdge aId cId Nothing . addEdge bId cId Nothing $ g3
      shortestPath aId cId g4 `shouldBe` Just [aId, cId]

  describe "Fluxus.Utils.Graph.buildCFG" $ do
    it "buildCFG produces graph with entry and exit" $ do
      let cfg = buildCFG [T.pack "stmt1", T.pack "stmt2"]
      cfgEntry cfg `shouldSatisfy` maybe False (const True)
      cfgExit cfg `shouldSatisfy` maybe False (const True)
      length (nodes cfg) `shouldBe` 3
      length (edges cfg) `shouldBe` 2

  describe "Fluxus.Utils.Graph.dominatorFrontier" $ do
    it "computes dominator frontier (entry dominates all so frontier empty)" $ do
      let (entryId, g1) = addNode "entry" emptyGraph
          (leftId, g2) = addNode "left" g1
          (rightId, g3) = addNode "right" g2
          (exitId, g4) = addNode "exit" g3
          g5 = addEdge entryId leftId Nothing . addEdge entryId rightId Nothing
             . addEdge leftId exitId Nothing . addEdge rightId exitId Nothing $ g4
          domTree = buildDominatorTree entryId g5
      dominatorFrontier entryId g5 domTree `shouldBe` Set.empty

  describe "Fluxus.Utils.Graph.dominators" $ do
    it "returns empty map when entry node does not exist" $ do
      let (aId, g) = addNode "A" emptyGraph
      dominators (aId + 99) g `shouldBe` Map.empty

    it "returns singleton for entry when graph has only entry" $ do
      let (entryId, g) = addNode "entry" emptyGraph
      dominators entryId g `shouldBe` Map.singleton entryId (Set.singleton entryId)

    it "computes dominators for linear chain" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          (cId, g3) = addNode "C" g2
          g4 = addEdge aId bId Nothing . addEdge bId cId Nothing $ g3
          doms = dominators aId g4
      Map.lookup aId doms `shouldBe` Just (Set.singleton aId)
      Map.lookup bId doms `shouldBe` Just (Set.fromList [aId, bId])
      Map.lookup cId doms `shouldBe` Just (Set.fromList [aId, bId, cId])

  describe "Fluxus.Utils.Graph.postDominators" $ do
    it "computes post-dominators for linear chain with exit" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          (cId, g3) = addNode "C" g2
          g4 = addEdge aId bId Nothing . addEdge bId cId Nothing $ g3
          postDoms = postDominators cId g4
      Map.lookup cId postDoms `shouldBe` Just (Set.singleton cId)
      Map.lookup bId postDoms `shouldBe` Just (Set.fromList [bId, cId])
      Map.lookup aId postDoms `shouldBe` Just (Set.fromList [aId, bId, cId])

  describe "Fluxus.Utils.Graph.neighbors" $ do
    it "returns successors and predecessors combined" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          (cId, g3) = addNode "C" g2
          g4 = addEdge aId bId Nothing . addEdge bId cId Nothing $ g3
      sort (neighbors aId g4) `shouldBe` [bId]
      sort (neighbors bId g4) `shouldBe` [aId, cId]
      sort (neighbors cId g4) `shouldBe` [bId]

  describe "Fluxus.Utils.Graph.topologicalSort edge cases" $ do
    it "returns Just [] for empty graph" $ do
      topologicalSort emptyGraph `shouldBe` Just []

    it "returns single node for singleton graph" $ do
      let (aId, g) = addNode "A" emptyGraph
      topologicalSort g `shouldBe` Just [aId]

  describe "Fluxus.Utils.Graph.findPath and shortestPath edge cases" $ do
    it "findPath returns single-node path when start equals end" $ do
      let (aId, g) = addNode "A" emptyGraph
      findPath aId aId g `shouldBe` Just [aId]

    it "shortestPath returns single-node path when start equals end" $ do
      let (aId, g) = addNode "A" emptyGraph
      shortestPath aId aId g `shouldBe` Just [aId]

    it "shortestPath returns Nothing when end is unreachable" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          g3 = addEdge aId bId Nothing g2
      shortestPath bId aId g3 `shouldBe` Nothing

  describe "Fluxus.Utils.Graph.graphToDot" $ do
    it "produces valid DOT with nodes and edges" $ do
      let (aId, g1) = addNode "A" emptyGraph
          (bId, g2) = addNode "B" g1
          g3 = addEdge aId bId (Just (T.pack "e1")) g2
          dot = graphToDot g3
      T.unpack dot `shouldContain` "digraph G"
      T.unpack dot `shouldContain` show aId
      T.unpack dot `shouldContain` show bId
      T.unpack dot `shouldContain` "->"
      T.unpack dot `shouldContain` "e1"
