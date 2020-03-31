{-# LANGUAGE NamedFieldPuns #-}

module Routing.Lib
  ( Node
  , AList(AList)
  , Edge(Edge)
  , makeDGraph
  , unAList
  , generateDijkstra
  , spDijkstra
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

class DGraph a where
  neighbors :: Node -> a -> [(Double, Node)]

newtype AList = AList { unAList :: IntMap [(Double, Node)] }

instance DGraph AList where
  neighbors n = fromMaybe [] . IntMap.lookup n . unAList

data Edge a = Edge { from   :: Node
                   , to     :: Node
                   , weight :: Double
                   , label  :: a
                   } deriving Show

makeDGraph :: [Edge a] -> AList
makeDGraph =
  AList
    . foldr
        (\Edge { from, to, weight } z ->
          IntMap.insertWith (++) from [(weight, to)] z
        )
        IntMap.empty

type Path = (Double, NonEmpty Node)

dijkstraSimple
  :: DGraph g
  => g          -- ^ directed graph
  -> Heap Path  -- ^ initial pairs
  -> Set Node   -- ^ nodes visited so far
  -> [Path]     -- ^ Dijkstra search tree
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

-- | Iteration of Dijkstra's algorithm
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
              & Heap.filter (\(_, x :| _) -> x /= xh)
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
  -> [Path]     -- ^ search tree
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
  => Node                    -- ^ origin node
  -> Node                    -- ^ destination node
  -> g                       -- ^ directed graph
  -> Maybe (Double, [Node])  -- ^ shortest path from origin to destination, if one exists
spDijkstra o d gr =
  fmap (second N.toList) $ find (\(_, (x :| _)) -> x == d) $ generateDijkstra
    gr
    (Heap.singleton (0, o :| []))
    Set.empty

-- | Shortest path between a pair of nodes, using the bidirectional Dijkstra algorithm
spBDD
  :: DGraph g
  => Node                    -- ^ origin node
  -> Node                    -- ^ destination node
  -> g                       -- ^ directed graph
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
