{ mkDerivation, base, bytestring, cassava, deepseq, filepath
, lineageflow-database-sqlite, lineageflow-io-cbor
, lineageflow-prelude, lineageflow-tracking, linear
, optparse-applicative, stdenv, temporary, text, vector
, vector-algorithms
}:
mkDerivation {
  pname = "lineageflow-import";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava lineageflow-io-cbor lineageflow-prelude
    lineageflow-tracking vector vector-algorithms
  ];
  executableHaskellDepends = [
    base bytestring deepseq filepath lineageflow-database-sqlite
    lineageflow-io-cbor linear optparse-applicative temporary text
  ];
  description = "Utilities for importing tracking files in LineageFlow";
  license = stdenv.lib.licenses.agpl3;
}
