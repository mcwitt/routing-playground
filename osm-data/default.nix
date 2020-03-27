{ pkgs ? import <nixpkgs> { } }:
with pkgs;
let
  osmosis = callPackage ../osmosis { };

  bbox = {
    s = "37.694";
    w = "-122.525";
    n = "37.815";
    e = "-122.35";
  };

  osm-xml = fetchurl {
    name = "osm-xml";
    url = let endpoint = "https://lz4.overpass-api.de/api/interpreter";
    in "${endpoint}?data=%28node%28${bbox.s}%2C${bbox.w}%2C${bbox.n}%2C${bbox.e}%29%3B%3C%3B%29%3Bout%20meta%3B";
    sha256 = "17062w6vyfjsgjaq2idahdw88z91nn9n0j6k6bfqvy2kb5a3f36z";
  };

in runCommand "osm-data" { buildInputs = [ osmosis adoptopenjdk-jre-bin ]; } ''
  mkdir -p $out
  cp ${osm-xml} $out
  osmosis --read-xml ${osm-xml} --write-pbf $out/sf.osm.pbf
''
