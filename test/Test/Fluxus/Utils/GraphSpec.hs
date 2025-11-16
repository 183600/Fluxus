module Test.Fluxus.Utils.GraphSpec (spec) where

import Test.Hspec

import Fluxus.Utils.Graph
  ( addEdge
  , addNode
  , buildDominatorTree
  , emptyGraph
  , immediateDominator
  , nodeId
  , nodes
  , topologicalSort
  )

spec :: Spec
spec = do
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
