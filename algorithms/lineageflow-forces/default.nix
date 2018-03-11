{ mkDerivation, base, containers, eigen, erf, LATS
, lineageflow-algorithm, lineageflow-homogenization
, lineageflow-io-cbor, lineageflow-prelude
, lineageflow-triangulations, stdenv
}:
mkDerivation {
  pname = "lineageflow-forces";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers eigen erf LATS lineageflow-homogenization
    lineageflow-prelude lineageflow-triangulations
  ];
  executableHaskellDepends = [
    base containers eigen erf LATS lineageflow-algorithm
    lineageflow-homogenization lineageflow-io-cbor lineageflow-prelude
    lineageflow-triangulations
  ];
  description = "Estimation of forces from cell trajectories";
  license = stdenv.lib.licenses.agpl3;
}
