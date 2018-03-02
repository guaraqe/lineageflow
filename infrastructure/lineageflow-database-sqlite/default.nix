{ mkDerivation, aeson, base, bytestring, cryptonite, directory
, exceptions, filepath, lineageflow-database, selda, selda-sqlite
, stdenv, temporary, text
}:
mkDerivation {
  pname = "lineageflow-database-sqlite";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring cryptonite directory exceptions filepath
    lineageflow-database selda selda-sqlite temporary text
  ];
  description = "A database for LineageFlow implemented with SQLite";
  license = stdenv.lib.licenses.gpl3;
}
