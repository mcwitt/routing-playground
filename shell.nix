{ pkgs ? import <nixpkgs> { }, ... }:
let
  inherit (pkgs) stdenv callPackage;
  osmData = pkgs.callPackage ./osm-data { };
  osmosis = pkgs.callPackage ./osmosis { };
in stdenv.mkDerivation {
  name = "shortest-paths-devenv";
  buildInputs = with pkgs; [
    osmData
    osmosis
    adoptopenjdk-bin
    (postgresql.withPackages (ps: [ ps.postgis ]))
  ];
  shellHook = ''
    export PGDATA=$PWD/postgres_data
    export PGHOST=$PWD/postgres
    export LOG_PATH=$PWD/postgres/LOG
    export PGDATABASE=postgres
    export DATABASE_URL="postgresql:///postgres?host=$PGHOST"

    if [ ! -d $PGHOST ]; then
      mkdir -p $PGHOST
    fi
    if [ ! -d $PGDATA ]; then
      echo 'Initializing postgresql database...'
      initdb $PGDATA --auth=trust > /dev/null
    fi

    pg_ctl start -l $LOG_PATH -o "-c listen_addresses=localhost -c unix_socket_directories=$PGHOST"

    if ! psql -lqt | cut -d \| -f 1 | grep -qw pgsnapshot; then
      echo 'First run, setting up postgis, OSM schema...'
      createdb pgsnapshot
      psql -d pgsnapshot -c 'CREATE EXTENSION postgis; CREATE EXTENSION hstore;'
      psql -d pgsnapshot -f ${osmosis}/script/pgsnapshot_schema_0.6.sql
      ${osmosis}/bin/osmosis \
        --read-pbf ${osmData}/sf.osm.pbf \
        --log-progress \
        --write-pgsql database=pgsnapshot
    fi
  '';
}
