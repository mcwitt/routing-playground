{ pkgs ? import <nixpkgs> { } }:
with pkgs;
let
  bbox = {
    s = "37.694";
    w = "-122.525";
    n = "37.815";
    e = "-122.35";
  };

in fetchurl {
  name = "san-francisco-osm-xml";
  url = let endpoint = "https://lz4.overpass-api.de/api/interpreter";
  in "${endpoint}?data=%28node%28${bbox.s}%2C${bbox.w}%2C${bbox.n}%2C${bbox.e}%29%3B%3C%3B%29%3Bout%20meta%3B";
  sha256 = "0b4r1g1k1f3x9m6flyc7x33lzh5kxdnvb4dsb2r5j48kplbjrxac";
}
