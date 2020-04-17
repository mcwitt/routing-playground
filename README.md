# routing-playground

Experiments with routing algorithms using OpenStreetMap data.

## Setup

### Haskell module

Use `stack` for developing the Haskell module. For example, to run tests:

``` shell
stack test
```

### Playground environment

Under `osm/` is a Nix environment consisting of the following
components:

1. PostgreSQL/PostGIS database loaded with OpenStreetMap data for San
   Francisco

2. JupyterLab server with IHaskell kernel; installed in the kernel
   environment are the `routing-playground` Haskell module, and
   various packages for analysis and visualization

To set up the environment, run
```shell
cd osm/
nix-shell
```
(Setup will likely take a long time on the first run.)

Inside the `nix-shell` environment, `$PGHOST` points to a running
postgres server with data preloaded in the `pgsnapshot` database. For
example, you should be able to run

```
$ psql -d pgsnapshot

pgsnapshot=# select * from sp.edges_sparse limit 5;
 way_id  | from_sequence_id | to_sequence_id | from_node_id | to_node_id |       distance
---------+------------------+----------------+--------------+------------+----------------------
 4786909 |                0 |              1 |     30677579 | 4908344443 | 0.000107050081741861
 4786909 |                1 |              2 |   4908344443 | 4908344440 | 0.000514158205224488
 4786909 |                2 |              3 |   4908344440 |  307105641 | 0.000182552923831145
 4786909 |                3 |              4 |    307105641 |   30677580 | 0.000162063104997168
 4786909 |                4 |              5 |     30677580 |  307105642 | 0.000110336304085102
(5 rows)
```

You can also start JupyterLab

``` shell
jupyter lab
```

See examples under `osm/notebooks/` to get started.
