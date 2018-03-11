{ mkDerivation, base, bytestring, cassava, filepath
, lineageflow-algorithm, lineageflow-database-sqlite
, lineageflow-io-cbor, lineageflow-prelude, lineageflow-tracking
, linear, optparse-applicative, stdenv, text, vector
, vector-algorithms
}:
mkDerivation {
  pname = "lineageflow-export";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava lineageflow-prelude lineageflow-tracking
    vector vector-algorithms
  ];
  executableHaskellDepends = [
    base bytestring filepath lineageflow-algorithm
    lineageflow-database-sqlite lineageflow-io-cbor lineageflow-prelude
    linear optparse-applicative text
  ];
  description = "Utilities for exporting selections for LineageFlow";
  license = stdenv.lib.licenses.agpl3;
}
