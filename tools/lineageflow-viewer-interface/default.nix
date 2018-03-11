{ mkDerivation, aeson, base, lens, lineageflow-query, stdenv, text
}:
mkDerivation {
  pname = "lineageflow-viewer-interface";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base lens lineageflow-query text ];
  description = "Synopsis";
  license = stdenv.lib.licenses.gpl3;
}
