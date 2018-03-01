{ mkDerivation, aeson, base, containers, lens, lineageflow-base
, stdenv, text
}:
mkDerivation {
  pname = "lineageflow-query";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers lens lineageflow-base text
  ];
  description = "Synopsis";
  license = stdenv.lib.licenses.gpl3;
}
