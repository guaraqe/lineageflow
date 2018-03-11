{ mkDerivation, aeson, base, containers, lens
, lineageflow-declaration, lineageflow-query, rank2classes, stdenv
, text
}:
mkDerivation {
  pname = "lineageflow-script";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers lens lineageflow-declaration
    lineageflow-query rank2classes text
  ];
  description = "Script definitions for LineageFlow";
  license = stdenv.lib.licenses.agpl3;
}
