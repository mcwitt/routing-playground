{ pkgs ? import <nixpkgs> { } }:
with pkgs;
let
  buildOsmosis = callPackage ./osmosis/gradle-env.nix { };

  osmosis = buildOsmosis {
    envSpec = ./osmosis/gradle-env.json;

    src = fetchFromGitHub {
      owner = "openstreetmap";
      repo = "osmosis";
      rev = "tags/0.47";
      sha256 = "10fm7fy79bfnrln6xjvzx382gfy37zwnh5pkaqb7lky8d4qqhflg";
    };

    nativeBuildInputs = [ git ];
    gradleFlags = [ "assemble" ];

    installPhase = ''
      mkdir -p $out
      cp -r package/* $out
    '';
  };

  planetutils = let
    version = "0.4.10";
    python3 = python37;

  in python3.pkgs.buildPythonPackage {
    pname = "planetutils";
    version = version;

    src = fetchFromGitHub {
      owner = "interline-io";
      repo = "planetutils";
      rev = "tags/v${version}";
      sha256 = "0picw4kjcn1f4lza5ygkh7jgjy7im08p8f6cvnh53rzxixg6l4hr";
    };

    pythonPath = with python3.pkgs; [ future requests ];
    propagatedBuildInputs = [ osmosis ];
    doCheck = false;
  };

  pythonEnv = python3.withPackages (ps: [ planetutils ]);

  download-osm = writeShellScriptBin "download-osm" ''
    osm_extract_download \
      --api-token=$1 \
      --data-format=pbf \
      san-francisco-bay_california
  '';

in mkShell {
  buildInputs = [ pythonEnv adoptopenjdk-jre-bin osmosis download-osm ];
}
