{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Graph utilities for control flow and data flow analysis
module Fluxus.Utils.Graph
  ( -- * Graph types
    Graph(..)
  , Node(..)
  , Edge(..)
  , NodeId
    -- * Graph construction
  , emptyGraph
  , addNode
  , addEdge
  , removeNode
  , removeEdge
    -- * Graph queries
  , nodes
  , edges
  , successors
  , predecessors
  , neighbors
  , nodeExists
  , edgeExists
    -- * Graph algorithms
  , topologicalSort
  , stronglyConnectedComponents
  , dominators
  , postDominators
  , reachableFrom
  , findPath
  , shortestPath
    -- * Control flow graph
  , ControlFlowGraph
  , CFGNode(..)
  , buildCFG
  , cfgEntry
  , cfgExit
    -- * Data flow analysis
  , DataFlowProblem(..)
  , DataFlowDirection(..)
  , dataFlowAnalysis
  , forwardDataFlow
  , backwardDataFlow
    -- * Dominator analysis
  , DominatorTree
  , buildDominatorTree
  , immediateDominator
  , dominatorFrontier
    -- * Utilities
  , graphToDot
  , visualizeGraph
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.List (foldl')

-- | Node identifier
type NodeId = Int

-- | Graph node
data Node a = Node
  { nodeId :: !NodeId
  , nodeData :: !a
  } deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
    deriving anyclass (Hashable, NFData)

-- | Graph edge
data Edge = Edge
  { edgeFrom :: !NodeId
  , edgeTo   :: !NodeId
  , edgeLabel :: !(Maybe Text)
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Generic graph representation
data Graph a = Graph
  { graphNodes :: !(Map NodeId (Node a))
  , graphEdges :: !(Set Edge)
  , graphNextId :: !NodeId
  } deriving stock (Eq, Show, Generic, Functor)
    deriving anyclass (NFData)

-- | Empty graph
emptyGraph :: Graph a
emptyGraph = Graph Map.empty Set.empty 0

-- | Add a node to the graph
addNode :: a -> Graph a -> (NodeId, Graph a)
addNode nd graph = 
  let nid = graphNextId graph
      node = Node nid nd
      newNodes = Map.insert nid node (graphNodes graph)
      newGraph = graph 
        { graphNodes = newNodes
        , graphNextId = nid + 1
        }
  in (nid, newGraph)

-- | Add an edge to the graph
addEdge :: NodeId -> NodeId -> Maybe Text -> Graph a -> Graph a
addEdge from to label graph =
  let edge = Edge from to label
      newEdges = Set.insert edge (graphEdges graph)
  in graph { graphEdges = newEdges }

-- | Remove a node from the graph
removeNode :: NodeId -> Graph a -> Graph a
removeNode nid graph =
  let newNodes = Map.delete nid (graphNodes graph)
      newEdges = Set.filter (\e -> edgeFrom e /= nid && edgeTo e /= nid) (graphEdges graph)
  in graph { graphNodes = newNodes, graphEdges = newEdges }

-- | Remove an edge from the graph
removeEdge :: NodeId -> NodeId -> Graph a -> Graph a
removeEdge from to graph =
  let newEdges = Set.filter (\e -> not (edgeFrom e == from && edgeTo e == to)) (graphEdges graph)
  in graph { graphEdges = newEdges }

-- | Get all nodes in the graph
nodes :: Graph a -> [Node a]
nodes = Map.elems . graphNodes

-- | Get all edges in the graph
edges :: Graph a -> [Edge]
edges = Set.toList . graphEdges

-- | Get successors of a node
successors :: NodeId -> Graph a -> [NodeId]
successors nid graph = 
  [edgeTo e | e <- Set.toList (graphEdges graph), edgeFrom e == nid]

-- | Get predecessors of a node
predecessors :: NodeId -> Graph a -> [NodeId]
predecessors nid graph = 
  [edgeFrom e | e <- Set.toList (graphEdges graph), edgeTo e == nid]

-- | Get neighbors (successors and predecessors) of a node
neighbors :: NodeId -> Graph a -> [NodeId]
neighbors nid graph = successors nid graph ++ predecessors nid graph

-- | Check if a node exists in the graph
nodeExists :: NodeId -> Graph a -> Bool
nodeExists nid graph = Map.member nid (graphNodes graph)

-- | Check if an edge exists in the graph
edgeExists :: NodeId -> NodeId -> Graph a -> Bool
edgeExists from to graph = 
  any (\e -> edgeFrom e == from && edgeTo e == to) (Set.toList $ graphEdges graph)

-- | Topological sort using Kahn's algorithm
topologicalSort :: Graph a -> Maybe [NodeId]
topologicalSort graph = go (Set.fromList $ map nodeId $ nodes graph) [] []
  where
    inDegree nid = length $ predecessors nid graph
    
    go remaining result queue
      | Set.null remaining && null queue = Just (reverse result)
      | null queue = Nothing  -- Cycle detected
      | otherwise = 
          let (current, restQueue) = case queue of
                [] -> 
                  let noIncoming = Set.filter (\n -> inDegree n == 0) remaining
                  in if Set.null noIncoming 
                     then (Set.findMin remaining, [])  -- Force progress, might indicate cycle
                     else (Set.findMin noIncoming, Set.toList noIncoming)
                (x:xs) -> (x, xs)
              newRemaining = Set.delete current remaining
              newSuccessors = filter (`Set.member` newRemaining) (successors current graph)
              newQueue = restQueue ++ newSuccessors
          in go newRemaining (current : result) newQueue

-- | Find strongly connected components using Tarjan's algorithm
stronglyConnectedComponents :: Graph a -> [[NodeId]]
stronglyConnectedComponents graph = 
  let allNodes = map nodeId $ nodes graph
  in tarjan allNodes (Map.empty :: Map.Map NodeId Int) (Map.empty :: Map.Map NodeId Int) ([] :: [NodeId]) (0 :: Int) ([] :: [[NodeId]])
  where
    tarjan :: [NodeId] -> Map.Map NodeId Int -> Map.Map NodeId Int -> [NodeId] -> Int -> [[NodeId]] -> [[NodeId]]
    tarjan [] _ _ _ _ result = result
    tarjan (v:vs) indices lowlinks stack index result
      | Map.member v indices = tarjan vs indices lowlinks stack index result
      | otherwise = 
          let (newIndices, newLowlinks, newStack, newIndex, newResult) = 
                strongConnect v indices lowlinks stack index result
          in tarjan vs newIndices newLowlinks newStack newIndex newResult
    
    strongConnect v indices lowlinks stack index result =
      let indices' = Map.insert v index indices
          lowlinks' = Map.insert v index lowlinks
          stack' = v : stack
          index' = index + 1
      in foldl' (processSuccessor v) (indices', lowlinks', stack', index', result) (successors v graph)
    
    processSuccessor v (indices, lowlinks, stack, index, result) w
      | not (Map.member w indices) = 
          let (indices', lowlinks', stack', index', result') = strongConnect w indices lowlinks stack index result
              lowlinks'' = Map.adjust (min (lowlinks' Map.! w)) v lowlinks'
          in (indices', lowlinks'', stack', index', result')
      | w `elem` stack = 
          let lowlinks' = Map.adjust (min (indices Map.! w)) v lowlinks
          in (indices, lowlinks', stack, index, result)
      | otherwise = (indices, lowlinks, stack, index, result)

-- | Compute dominators using iterative algorithm
dominators :: NodeId -> Graph a -> Map NodeId (Set NodeId)
dominators entry graph = 
  let allNodes = Set.fromList $ map nodeId $ nodes graph
      initialDom = Map.fromList $ 
                   [(entry, Set.singleton entry)] ++
                   [(n, allNodes) | n <- map nodeId $ nodes graph, n /= entry]
  in fixpoint initialDom
  where
    fixpoint dom = 
      let newDom = Map.mapWithKey (updateDom dom) dom
      in if newDom == dom then dom else fixpoint newDom
    
    updateDom dom nid currentDom
      | nid == entry = Set.singleton entry
      | otherwise = 
          let preds = predecessors nid graph
              predDoms = [dom Map.! p | p <- preds, Map.member p dom]
          in if null predDoms 
             then currentDom
             else Set.insert nid (foldl1 Set.intersection predDoms)

-- | Compute post-dominators
postDominators :: NodeId -> Graph a -> Map NodeId (Set NodeId)
postDominators exit graph = 
  -- Reverse the graph and compute dominators
  let reversedGraph = reverseGraph graph
  in dominators exit reversedGraph
  where
    reverseGraph g = g { graphEdges = Set.map reverseEdge (graphEdges g) }
    reverseEdge (Edge from to label) = Edge to from label

-- | Find all nodes reachable from a given node
reachableFrom :: NodeId -> Graph a -> Set NodeId
reachableFrom start graph = go Set.empty [start]
  where
    go visited [] = visited
    go visited (current:queue)
      | Set.member current visited = go visited queue
      | otherwise = 
          let newVisited = Set.insert current visited
              newNodes = successors current graph
          in go newVisited (queue ++ newNodes)

-- | Find a path between two nodes (DFS)
findPath :: NodeId -> NodeId -> Graph a -> Maybe [NodeId]
findPath start end graph = dfs Set.empty start
  where
    dfs visited current
      | current == end = Just [current]
      | Set.member current visited = Nothing
      | otherwise = 
          let newVisited = Set.insert current visited
              paths = [dfs newVisited next | next <- successors current graph]
              validPaths = [path | Just path <- paths]
          in case validPaths of
               (path:_) -> Just (current : path)
               [] -> Nothing

-- | Find shortest path using BFS
shortestPath :: NodeId -> NodeId -> Graph a -> Maybe [NodeId]
shortestPath start end graph = bfs (Map.singleton start [start]) [start]
  where
    bfs _ [] = Nothing
    bfs paths (current:queue)
      | current == end = Map.lookup end paths
      | otherwise = 
          let currentPath = paths Map.! current
              newNodes = [n | n <- successors current graph, not (Map.member n paths)]
              newPaths = foldl' (\acc n -> Map.insert n (currentPath ++ [n]) acc) paths newNodes
          in bfs newPaths (queue ++ newNodes)

-- | Control flow graph node types
data CFGNode
  = CFGEntry
  | CFGExit
  | CFGBasicBlock !Text ![Text]  -- label, statements
  | CFGBranch !Text              -- condition
  | CFGLoop !Text                -- loop condition
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Control flow graph
type ControlFlowGraph = Graph CFGNode

-- | Build control flow graph (placeholder implementation)
buildCFG :: [Text] -> ControlFlowGraph
buildCFG statements = 
  let (entryId, graph1) = addNode CFGEntry emptyGraph
      (exitId, graph2) = addNode CFGExit graph1
      (blockId, graph3) = addNode (CFGBasicBlock "main" statements) graph2
      graph4 = addEdge entryId blockId Nothing graph3
      graph5 = addEdge blockId exitId Nothing graph4
  in graph5

-- | Get entry node of CFG
cfgEntry :: ControlFlowGraph -> Maybe NodeId
cfgEntry graph = 
  let entryNodes = [nodeId n | n <- nodes graph, nodeData n == CFGEntry]
  in case entryNodes of
       (nid:_) -> Just nid
       [] -> Nothing

-- | Get exit node of CFG
cfgExit :: ControlFlowGraph -> Maybe NodeId
cfgExit graph = 
  let exitNodes = [nodeId n | n <- nodes graph, nodeData n == CFGExit]
  in case exitNodes of
       (nid:_) -> Just nid
       [] -> Nothing

-- | Data flow analysis direction
data DataFlowDirection = Forward | Backward
  deriving stock (Eq, Show, Generic)

-- | Data flow problem specification
data DataFlowProblem a = DataFlowProblem
  { dfpDirection :: !DataFlowDirection
  , dfpInitial :: !a
  , dfpTransfer :: CFGNode -> a -> a
  , dfpMeet :: [a] -> a
  }

-- | Generic data flow analysis
dataFlowAnalysis :: Eq a => DataFlowProblem a -> ControlFlowGraph -> Map NodeId a
dataFlowAnalysis problem graph = 
  case dfpDirection problem of
    Forward -> forwardDataFlow problem graph
    Backward -> backwardDataFlow problem graph

-- | Forward data flow analysis
forwardDataFlow :: Eq a => DataFlowProblem a -> ControlFlowGraph -> Map NodeId a
forwardDataFlow problem graph = 
  let allNodes = map nodeId $ nodes graph
      initialMap = Map.fromList [(n, dfpInitial problem) | n <- allNodes]
  in fixpointIteration initialMap
  where
    fixpointIteration current = 
      let updated = Map.mapWithKey (updateNode current) current
      in if updated == current then current else fixpointIteration updated
    
    updateNode current nid _ = 
      let preds = predecessors nid graph
          predValues = [current Map.! p | p <- preds, Map.member p current]
          meetValue = if null predValues then dfpInitial problem else dfpMeet problem predValues
          ndata = case Map.lookup nid (graphNodes graph) of
            Nothing -> CFGBasicBlock "unknown" []
            Just node -> Fluxus.Utils.Graph.nodeData node
      in dfpTransfer problem ndata meetValue

-- | Backward data flow analysis
backwardDataFlow :: Eq a => DataFlowProblem a -> ControlFlowGraph -> Map NodeId a
backwardDataFlow problem graph = 
  let reversedProblem = problem { dfpDirection = Forward }
      reversedGraph = reverseGraph graph
  in forwardDataFlow reversedProblem reversedGraph
  where
    reverseGraph g = g { graphEdges = Set.map reverseEdge (graphEdges g) }
    reverseEdge (Edge from to label) = Edge to from label

-- | Dominator tree
type DominatorTree = Graph NodeId

-- | Build dominator tree
buildDominatorTree :: NodeId -> Graph a -> DominatorTree
buildDominatorTree entry graph = 
  let doms = dominators entry graph
      immDoms = Map.mapWithKey (findImmediateDominator doms) doms
      (_, emptyDomTree) = addNode entry emptyGraph
  in foldl' addDomEdge emptyDomTree (Map.toList immDoms)
  where
    findImmediateDominator allDoms nid nodeDoms
      | nid == entry = Nothing
      | otherwise = 
          let properDoms = Set.delete nid nodeDoms
              candidates = Set.toList properDoms
          in case candidates of
               [] -> Nothing
               _  -> case filter (\d -> all (\other -> Set.member d (allDoms Map.! other)) candidates) candidates of
                      (d:_) -> Just d
                      []    -> Nothing
    
    addDomEdge tree (nid, mImmDom) = 
      case mImmDom of
        Nothing    -> tree
        Just immDom -> addEdge immDom nid Nothing tree

-- | Find immediate dominator
immediateDominator :: NodeId -> DominatorTree -> Maybe NodeId
immediateDominator nid domTree = 
  let preds = predecessors nid domTree
  in case preds of
       [immDom] -> Just immDom
       _ -> Nothing

-- | Compute dominator frontier
dominatorFrontier :: NodeId -> Graph a -> DominatorTree -> Set NodeId
dominatorFrontier nid graph domTree = 
  let dominated = reachableFrom nid domTree
  in Set.fromList [n | d <- Set.toList dominated, n <- successors d graph, not (Set.member n dominated)]

-- | Generate DOT representation of graph
graphToDot :: Show a => Graph a -> Text
graphToDot graph = 
  let nodeLines = [T.pack $ "  " ++ show (nodeId n) ++ " [label=\"" ++ show (nodeData n) ++ "\"];" | n <- nodes graph]
      edgeLines = [T.pack $ "  " ++ show (edgeFrom e) ++ " -> " ++ show (edgeTo e) ++ 
                   maybe "" (\l -> " [label=\"" ++ T.unpack l ++ "\"]") (edgeLabel e) ++ ";" | e <- edges graph]
  in T.unlines $ ["digraph G {"] ++ nodeLines ++ edgeLines ++ ["}"]

-- | Visualize graph (placeholder - would integrate with graphviz)
visualizeGraph :: Show a => Graph a -> IO ()
visualizeGraph graph = do
  putStrLn "Graph visualization:"
  putStrLn $ T.unpack $ graphToDot graph