{ mkDerivation, aeson, base, lens, lineageflow-base, stdenv, text
}:
mkDerivation {
  pname = "lineageflow-declaration";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base lens lineageflow-base text ];
  description = "Synopsis";
  license = stdenv.lib.licenses.gpl3;
}
