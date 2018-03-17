{ mkDerivation, base, containers, deepseq, lineageflow-algorithm
, lineageflow-io-cbor, lineageflow-prelude, lineageflow-statistics
, qhull-simple, stdenv
}:
mkDerivation {
  pname = "lineageflow-triangulations";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers deepseq lineageflow-prelude lineageflow-statistics
    qhull-simple
  ];
  executableHaskellDepends = [
    base containers deepseq lineageflow-algorithm lineageflow-io-cbor
    lineageflow-prelude lineageflow-statistics qhull-simple
  ];
  description = "Manipulation of triangulations for LineageFlow";
  license = stdenv.lib.licenses.agpl3;
}
