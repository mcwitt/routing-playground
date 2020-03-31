{-# LANGUAGE NamedFieldPuns #-}

module Routing.Lib
  ( Node
  , DGraph(DGraph)
  , Edge(Edge)
  , makeGraph
  , unDGraph
  , dijkstra
  , spDijkstra
  )
where

import           Data.Heap                      ( Heap )
import qualified Data.Heap                     as Heap
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import           Data.List                      ( find )
import           Data.Maybe                     ( fromMaybe )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

type Node = Int

class Graph a where
  neighbors :: Node -> a -> [(Node, Double)]

newtype DGraph = DGraph { unDGraph :: IntMap [(Node, Double)] }

instance Graph DGraph where
  neighbors n = fromMaybe [] . IntMap.lookup n . unDGraph

data Edge a = Edge { from   :: Node
                   , to     :: Node
                   , weight :: Double
                   , label  :: a
                   } deriving Show

makeGraph :: [Edge a] -> DGraph
makeGraph =
  DGraph
    . foldr
        (\Edge { from, to, weight } z ->
          IntMap.insertWith (++) from [(to, weight)] z
        )
        IntMap.empty

dijkstra
  :: Graph g => g -> Heap (Double, [Node]) -> Set Node -> [(Double, [Node])]
dijkstra gr = go
 where
  go heap visited = case Heap.uncons heap of
    Just (h@(wsum, x : xs), hs) ->
      let unvisited (x', _) = x `Set.notMember` visited
          additions = (\(x', w) -> (wsum + w, x' : x : xs))
            <$> filter unvisited (neighbors x gr)
      in  h : go (foldr Heap.insert hs additions) (Set.insert x visited)
    _ -> []

spDijkstra :: Graph g => Node -> Node -> g -> Maybe (Double, [Node])
spDijkstra o d gr =
  find
      (\(_, xs) -> case xs of
        x : _ -> x == d
        _     -> False
      )
    $ dijkstra gr (Heap.singleton (0, [o])) Set.empty

astar
  :: Graph g
  => g
  -> (Node -> Node -> Double)  -- ^ Function implementing heuristic:
                               --   destination -> node -> H_dest(node)
  -> Heap (Double, [Node])
  -> Set Node
  -> (Double, [Node])
astar origin dest gr = undefined
