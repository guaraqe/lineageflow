{ mkDerivation, base, stdenv, text }:
mkDerivation {
  pname = "lineageflow-io";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base text ];
  description = "Synopsis";
  license = stdenv.lib.licenses.gpl3;
}
