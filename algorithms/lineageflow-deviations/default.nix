{ mkDerivation, base, containers, lens, lineageflow-algorithm
, lineageflow-derivatives, lineageflow-io-cbor, lineageflow-prelude
, lineageflow-statistics, semialign, statistics, stdenv
, storable-record, these
}:
mkDerivation {
  pname = "lineageflow-deviations";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers lens lineageflow-derivatives lineageflow-prelude
    lineageflow-statistics semialign statistics storable-record these
  ];
  executableHaskellDepends = [
    base containers lens lineageflow-algorithm lineageflow-derivatives
    lineageflow-io-cbor lineageflow-prelude lineageflow-statistics
    statistics storable-record these
  ];
  description = "Deviations of neighboring cell trajectories";
  license = stdenv.lib.licenses.agpl3;
}
