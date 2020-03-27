{-# LANGUAGE NamedFieldPuns #-}

module Routing.Lib
  ( Node
  , DGraph(DGraph)
  , Edge(Edge)
  , makeGraph
  , unDGraph
  , dijkstra
  )
where

import qualified Data.Heap                     as Heap
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import qualified Data.List.NonEmpty            as N
import           Data.Maybe                     ( fromMaybe )
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
  :: Graph g
  => Node     -- ^ Origin node
  -> Node     -- ^ Destination node
  -> g
  -> Maybe (Double, [Node])
dijkstra origin dest gr = fmap N.head . N.nonEmpty $ filter (endsAt dest)
                                                            shortestPaths
 where
  endsAt n (_, x : _) = x == n
  endsAt _ _          = False
  shortestPaths = go (Heap.singleton (0, [origin])) Set.empty
  go heap visited = case Heap.uncons heap of
    Just (h@(wsum, x : xs), hs) ->
      let unvisited (x', _) = x `Set.notMember` visited
          additions = (\(x', w) -> (wsum + w, x' : x : xs))
            <$> filter unvisited (neighbors x gr)
      in  h : go (foldr Heap.insert hs additions) (Set.insert x visited)
    _ -> []

astar
  :: Graph g
  => (Node -> Node -> Double)  -- ^ Function implementing heuristic:
                               --   destination -> node -> H_dest(node)
  -> Node                      -- ^ Origin node
  -> Node                      -- ^ Destination node
  -> g
  -> (Double, [Node])          -- ^ Shortest path, total distance
astar origin dest gr = undefined
