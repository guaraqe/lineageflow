{ mkDerivation, base, exact-combinatorics, lens
, lineageflow-algorithm, lineageflow-io-cbor, lineageflow-prelude
, stdenv
}:
mkDerivation {
  pname = "lineageflow-derivatives";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base exact-combinatorics lens lineageflow-prelude
  ];
  executableHaskellDepends = [
    base exact-combinatorics lens lineageflow-algorithm
    lineageflow-io-cbor lineageflow-prelude
  ];
  description = "Discrete derivatives on cell trajectories";
  license = stdenv.lib.licenses.agpl3;
}
