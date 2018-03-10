{ mkDerivation, base, clustering, harpack, lineageflow-algorithm
, lineageflow-io-cbor, lineageflow-prelude, lineageflow-statistics
, matrices, stdenv
}:
mkDerivation {
  pname = "lineageflow-clustering";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base clustering harpack lineageflow-prelude lineageflow-statistics
    matrices
  ];
  executableHaskellDepends = [
    base clustering harpack lineageflow-algorithm lineageflow-io-cbor
    lineageflow-prelude lineageflow-statistics matrices
  ];
  description = "Clustering of cell trajectories";
  license = stdenv.lib.licenses.agpl3;
}
