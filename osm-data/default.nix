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
    sha256 = "0ra8d51dnmpxqf7b469pwyncnbjlx0174ra1l9ghvr5m05nyrv59";
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
