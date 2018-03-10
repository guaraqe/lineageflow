{ mkDerivation, base, containers, lineageflow-algorithm
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
    base containers lineageflow-prelude lineageflow-statistics
    qhull-simple
  ];
  executableHaskellDepends = [
    base containers lineageflow-algorithm lineageflow-io-cbor
    lineageflow-prelude lineageflow-statistics qhull-simple
  ];
  description = "Estimation of forces from trajectories";
  license = stdenv.lib.licenses.agpl3;
}
