{ mkDerivation, base, lineageflow-query, stdenv, text }:
mkDerivation {
  pname = "lineageflow-executable";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base lineageflow-query text ];
  description = "Synopsis";
  license = stdenv.lib.licenses.gpl3;
}
