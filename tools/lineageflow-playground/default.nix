{ mkDerivation, base, foreign-store, lens, lineageflow-algorithm
, lineageflow-database-sqlite, lineageflow-derivatives
, lineageflow-io-cbor, lineageflow-plot, lineageflow-prelude
, lineageflow-statistics, statistics, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "lineageflow-playground";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base foreign-store lens lineageflow-algorithm
    lineageflow-database-sqlite lineageflow-derivatives
    lineageflow-io-cbor lineageflow-plot lineageflow-prelude
    lineageflow-statistics statistics text unordered-containers
  ];
  description = "Helpers for interactive development for LineageFlow";
  license = stdenv.lib.licenses.agpl3;
}
