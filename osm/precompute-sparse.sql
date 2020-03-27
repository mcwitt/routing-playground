CREATE SCHEMA sp;

CREATE TABLE sp.waypoints (
    id serial primary key
  , way_id bigint
  , node_id bigint
  , sequence_id integer
  , distance double precision
);

INSERT INTO sp.waypoints (way_id, node_id, sequence_id, distance) (
  SELECT way_id
       , node_id
       , sequence_id
       , NULL AS distance
  FROM way_nodes
);

-- Set distance to zero for initial node in each way
UPDATE sp.waypoints
SET distance = 0
WHERE sequence_id = 0;

-- Update distances for remaining nodes using a cumulative sum of
-- pairwise distances partitioned by way_id and ordered by sequence_id
UPDATE sp.waypoints
SET distance = t.distance
FROM (
  WITH wp_geom AS (
    SELECT waypoints.id AS waypoint_id
         , waypoints.way_id
         , waypoints.sequence_id
         , nodes.geom
    FROM sp.waypoints
    JOIN nodes ON nodes.id = waypoints.node_id
  )
  SELECT b.waypoint_id
       , SUM(ST_Distance(a.geom :: geography, b.geom :: geography))
         OVER(PARTITION BY a.way_id ORDER BY a.sequence_id) AS distance
  FROM wp_geom a
  JOIN wp_geom b ON a.way_id = b.way_id AND a.sequence_id + 1 = b.sequence_id
) t
WHERE waypoints.id = t.waypoint_id;

-- Create "sparse" table filtering out nodes associated with only a
-- single way (these are not so interesting from a path-finding
-- perspective)
CREATE TABLE sp.waypoints_sparse AS (
  WITH node AS (
    SELECT node_id
         , count(way_id) AS way_count
    FROM way_nodes
    GROUP BY node_id
  )
  SELECT way_id
       , node_id
       , sequence_id
       , distance
       , row_number() OVER(PARTITION BY way_id ORDER BY sequence_id) AS sparse_sequence_id
  FROM sp.waypoints
  JOIN node USING (node_id)
  WHERE node.way_count > 1
);

CREATE TABLE sp.edges_sparse AS (
  SELECT a.way_id
       , a.sequence_id AS from_sequence_id
       , b.sequence_id AS to_sequence_id
       , a.node_id AS from_node_id
       , b.node_id AS to_node_id
       , b.distance - a.distance AS distance
  FROM sp.waypoints_sparse a
  JOIN sp.waypoints_sparse b
    ON a.way_id = b.way_id
   AND a.sparse_sequence_id + 1 = b.sparse_sequence_id
);
