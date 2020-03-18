{ pkgs ? import <nixpkgs> { }, ... }:
let
  inherit (pkgs) stdenv callPackage;
  osm-data = callPackage ./osm-data { };
  osmosis = callPackage ./osmosis { };
  pg = pkgs.postgresql.withPackages (ps: [ ps.postgis ]);
  database = "pgsnapshot";

in pkgs.mkShell {
  buildInputs = with pkgs; [ adoptopenjdk-bin osm-data osmosis pg ];
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
      ${pg}/bin/initdb $PGDATA --auth=trust > /dev/null
    fi

    ${pg}/bin/pg_ctl start -l $LOG_PATH -o "-c listen_addresses=localhost -c unix_socket_directories=$PGHOST"
    trap "${pg}/bin/pg_ctl stop" EXIT

    if ! ${pg}/bin/psql -lqt | cut -d \| -f 1 | grep -qw ${database}; then
      echo 'First run, setting up postgis, OSM schema...'
      ${pg}/bin/createdb ${database}
      ${pg}/bin/psql -d ${database} -c 'CREATE EXTENSION postgis; CREATE EXTENSION hstore;'
      ${pg}/bin/psql -d ${database} -f ${osmosis}/script/pgsnapshot_schema_0.6.sql
      ${osmosis}/bin/osmosis --read-pbf ${osm-data}/sf.osm.pbf --log-progress --write-pgsql database=${database}

      echo 'Precomputing edge weights...'
      ${pg}/bin/psql -d ${database} -f ${./precompute-sparse.sql}
    fi
  '';
}
