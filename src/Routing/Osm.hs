{-# LANGUAGE OverloadedStrings #-}

module Routing.Osm
  ( queryEdges
  , queryNodes
  , unOsmEdge
  , coords
  )
where

import           Control.Monad.Reader
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow

import           Routing.Lib                    ( Node
                                                , Edge(Edge)
                                                )

data OsmNode = OsmNode { nodeId :: Node
                       , coords :: (Double, Double)
                       }

data OsmEdgeLabel = OsmEdgeLabel { wayId :: Int
                                 , fromSequenceId :: Int
                                 , toSequenceId :: Int
                                 } deriving Show

newtype OsmEdge = OsmEdge { unOsmEdge :: Edge OsmEdgeLabel } deriving Show

instance FromRow OsmNode where
  fromRow = OsmNode <$> field <*> ((,) <$> field <*> field)

instance FromRow OsmEdge where
  fromRow =
    OsmEdge
      <$> (   Edge
          <$> field
          <*> field
          <*> field
          <*> (OsmEdgeLabel <$> field <*> field <*> field)
          )

queryEdges :: ReaderT Connection IO [OsmEdge]
queryEdges = ask >>= \conn -> lift $ query_
  conn
  "SELECT from_node_id, to_node_id , distance, way_id, from_sequence_id, to_sequence_id FROM sp.edges_sparse WHERE distance IS NOT NULL"

queryNodes :: [Node] -> ReaderT Connection IO [(Node, OsmNode)]
queryNodes ns = ask >>= \conn ->
  lift
    . (fmap . fmap) (\n@(OsmNode i _) -> (i, n))
    $ query conn "SELECT id, ST_X(geom), ST_Y(geom) FROM nodes WHERE id IN ?"
    $ Only (In ns)
