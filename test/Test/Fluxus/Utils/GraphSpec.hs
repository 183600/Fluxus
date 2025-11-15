module Test.Fluxus.Utils.GraphSpec (spec) where

import Test.Hspec

import Fluxus.Utils.Graph
  ( addEdge
  , addNode
  , emptyGraph
  , topologicalSort
  )

spec :: Spec
spec = describe "Fluxus.Utils.Graph.topologicalSort" $ do
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
