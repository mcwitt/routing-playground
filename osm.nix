{ pkgs, token, extractId ? "san-francisco-bay_california" }:
with pkgs;
let
  osmosis = callPackage ./osmosis { };

  osmPbf = let
    host = "app.interline.io";
    endpoint = "osm_extracts/download_latest";
  in fetchurl {
    name = "${extractId}.osm.pbf";
    url =
      "https://${host}/${endpoint}?data_format=pbf&string_id=${extractId}&api_token=${token}";
    sha256 = "1k5fscgp1diyissw6yh9sr60ff93v34idhlgfizr2gzwf6pjs2sx";
  };

in runCommand "osm-xml_sfbay" {
  buildInputs = [ osmosis adoptopenjdk-jre-bin ];
} ''
  mkdir -p $out
  cp ${osmPbf} $out
  osmosis --read-pbf ${osmPbf} --write-xml $out/${extractId}.osm.xml
''
