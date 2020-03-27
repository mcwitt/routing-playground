with connected_nodes as (
select distinct nodes.id
from nodes
join sp.waypoints_sparse on waypoints_sparse.node_id = nodes.id
join ways on ways.id = waypoints_sparse.way_id
where ways.tags -> 'highway' = 'motorway'
),
poi as (
select * from (
values ('Mission Cliffs', 'SRID=4326;Point(-122.412572 37.7609875)' :: geometry)
     , ('Planet Granite', 'SRID=4326;Point(-122.468189 37.804171)')
) as t(id, geom)
),
poi_nodes as (
select poi.id as poi_id
     , nodes.id as node_id
     , ST_Distance(poi.geom, nodes.geom) as distance
from nodes
cross join poi
inner join connected_nodes cn on cn.id = nodes.id
)
select distinct on (poi.id)
       poi.id
     , nodes.id as node_id
     , tags -> 'name' as node_name_tag
     , distance
from poi_nodes
join poi on poi.id = poi_nodes.poi_id
join nodes on nodes.id = poi_nodes.node_id
-- where tags -> 'name' is not null
order by poi.id, distance asc
limit 5
;
