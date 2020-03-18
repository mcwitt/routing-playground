{ pkgs }:
with pkgs;
let
  osmosis = callPackage ../osmosis { };
  token = import ./token.nix;
  extractId = "san-francisco-bay_california";

  osmExtract = let
    host = "app.interline.io";
    endpoint = "osm_extracts/download_latest";
  in fetchurl {
    name = "osm-extract--${extractId}";
    url =
      "https://${host}/${endpoint}?data_format=pbf&string_id=${extractId}&api_token=${token}";
    sha256 = "1ghzwplsd4drn86m8b52b3aidmxq7n08qrl9ipbzmp3ywnlidrml";
  };

in runCommand "osm-data-sf" {
  buildInputs = [ osmosis adoptopenjdk-jre-bin ];
} ''
  mkdir -p $out
  cp ${osmExtract} $out
  osmosis \
    --read-pbf ${osmExtract} \
    --bounding-box top=37.815 left=-122.525 bottom=37.694 right=-122.350 \
    --write-pbf $out/sf.osm.pbf
''
