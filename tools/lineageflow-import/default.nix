{ mkDerivation, base, bytestring, cassava, filepath
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
    base bytestring filepath lineageflow-database-sqlite
    lineageflow-io-cbor linear optparse-applicative temporary text
  ];
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.gpl3;
}
