{-# LANGUAGE RecordWildCards #-}

module Routing.Lib
  ( Node
  , AList(AList)
  , Edge(Edge)
  , fromEdges
  , fromEdgesSymmetric
  , unAList
  , generateDijkstra
  , spDijkstra
  , spDijkstraSimple
  )
where

import           Control.Monad.Loops
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Function                  ( (&) )
import           Data.Heap                      ( Heap )
import qualified Data.Heap                     as Heap
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import           Data.List                      ( find
                                                , nub
                                                , unfoldr
                                                )
import qualified Data.List.NonEmpty            as N
import           Data.List.NonEmpty             ( NonEmpty((:|))
                                                , (<|)
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

type Node = Int

-- | Interface for directed graphs.
class DGraph a where
  neighbors :: Node -> a -> [(Double, Node)]

-- | Adjacency list for representing a directed graph.
newtype AList = AList { unAList :: IntMap [(Double, Node)] }

-- | Adjacency list representation.
instance DGraph AList where
  neighbors n = fromMaybe [] . IntMap.lookup n . unAList

instance Semigroup AList where
  AList l <> AList r =
    AList
      $ foldr (uncurry $ IntMap.insertWith (\xs ys -> nub (xs ++ ys))) l
      $ IntMap.assocs r

-- | Edge in a directed graph.
data Edge a = Edge { from   :: Node
                   , to     :: Node
                   , weight :: Double
                   , label  :: a
                   } deriving Show

-- | Create an adjacency list representation from a list of edges
fromEdges :: [Edge a] -> AList
fromEdges =
  AList
    . foldr (\Edge {..} z -> IntMap.insertWith (++) from [(weight, to)] z)
            IntMap.empty

fromEdgesSymmetric :: [Edge a] -> AList
fromEdgesSymmetric es =
  fromEdges es <> fromEdges [ e { from = to, to = from } | e@Edge {..} <- es ]

-- | A path is a total distance and a non-empty list of nodes
type Path = (Double, NonEmpty Node)

endsAt :: Node -> Path -> Bool
endsAt d (_, x :| _) = x == d

-- | Simple, naive implementation of Dijkstra's algorithm.
-- We don't check whether neighbors have been previously visited
-- before adding to the queue (therefore we need to check the head
-- element).
dijkstraSimple
  :: DGraph g
  => g          -- ^ directed graph
  -> Heap Path  -- ^ initial pairs
  -> Set Node   -- ^ nodes visited so far
  -> [Path]     -- ^ shortest path tree
dijkstraSimple gr = go
 where
  go heap visited = case Heap.uncons heap of
    Just (h@(dist, xs@(xh :| _)), hs) -> if xh `Set.notMember` visited
      then
        let newPaths = [ (dist + d, x <| xs) | (d, x) <- neighbors xh gr ]
            hs'      = foldr Heap.insert hs newPaths
            visited' = Set.insert xh visited
        in  h : go hs' visited'
      else go hs visited
    Nothing -> []

spDijkstraSimple :: DGraph g => g -> Node -> Node -> Maybe Path
spDijkstraSimple gr o d =
  find (endsAt d) $ dijkstraSimple gr (Heap.singleton (0, o :| [])) Set.empty

-- | Single iteration of Dijkstra's algorithm.
dijkstra :: DGraph g => g -> State (Heap Path, Set Node) (Maybe Path)
dijkstra gr = get >>= \(heap, visited) -> case Heap.uncons heap of

  Just (h@(dist, xs@(xh :| _)), hs) ->
    let newPaths =
            [ (dist + d, x <| xs)
            | (d, x) <- neighbors xh gr
            , x `Set.notMember` visited
            ]

        hs' =
            hs
              -- remove all instances of this node from the heap
              & Heap.filter (endsAt xh)
              -- add paths to (unvisited) neighbors to the heap
              & flip (foldr Heap.insert) newPaths

        visited' = Set.insert xh visited
    in  put (hs', visited') >> return (Just h)

  _ -> return Nothing

generateDijkstra
  :: DGraph g
  => g          -- ^ directed graph
  -> Heap Path  -- ^ initial pairs
  -> Set Node   -- ^ nodes visited so far
  -> [Path]     -- ^ shortest path tree
generateDijkstra = curry . evalState . unfoldM . dijkstra

generateDijkstra'
  :: DGraph g
  => g          -- ^ directed graph
  -> Heap Path  -- ^ initial pairs
  -> Set Node   -- ^ nodes visited so far
  -> [(Path, (Heap Path, Set Node))]
generateDijkstra' gr =
  curry . unfoldr $ (\t@(mp, s) -> fmap (\p -> ((p, s), s)) mp) . runState
    (dijkstra gr)

-- | Shortest path between a pair of nodes, using Dijkstra's algorithm
spDijkstra
  :: DGraph g
  => g                       -- ^ directed graph
  -> Node                    -- ^ origin node
  -> Node                    -- ^ destination node
  -> Maybe (Double, [Node])  -- ^ shortest path from origin to destination, if one exists
spDijkstra gr o d = fmap (second N.toList) $ find (endsAt d) $ generateDijkstra
  gr
  (Heap.singleton (0, o :| []))
  Set.empty

-- | Shortest path between a pair of nodes, using the bidirectional Dijkstra algorithm
spBDD
  :: DGraph g
  => g                       -- ^ directed graph
  -> Node                    -- ^ origin node
  -> Node                    -- ^ destination node
  -> Maybe (Double, [Node])  -- ^ shortest path from origin to destination, if one exists
spBDD = undefined

-- | Shortest path between a pair of nodes, using the bidirectional Dijkstra algorithm
astar
  :: DGraph g
  => g                         -- ^ directed graph
  -> (Node -> Node -> Double)  -- ^ function implementing admissible heuristic
  -> Heap Path                 -- ^ initial pairs
  -> Set Node                  -- ^ nodes visited so far
  -> [Path]
astar origin dest gr = undefined
