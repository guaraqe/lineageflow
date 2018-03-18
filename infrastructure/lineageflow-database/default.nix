{ mkDerivation, base, lineageflow-query, stdenv, text }:
mkDerivation {
  pname = "lineageflow-database";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base lineageflow-query text ];
  description = "Database interface for LineageFlow";
  license = stdenv.lib.licenses.agpl3;
}
