{ pkgs, ... }:
with pkgs;
let buildOsmosis = callPackage ./gradle-env.nix { };

in buildOsmosis {
  envSpec = ./gradle-env.json;

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
}
