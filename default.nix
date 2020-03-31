{ mkDerivation, base, containers, heaps, hpack, hspec, monad-loops
, mtl, postgresql-simple, stdenv
}:
mkDerivation {
  pname = "routing-playground";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers heaps monad-loops mtl postgresql-simple
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base containers heaps monad-loops mtl postgresql-simple
  ];
  testHaskellDepends = [
    base containers heaps hspec monad-loops mtl postgresql-simple
  ];
  prePatch = "hpack";
  homepage = "https://github.com/mcwitt/routing-playground#readme";
  license = stdenv.lib.licenses.bsd3;
}
