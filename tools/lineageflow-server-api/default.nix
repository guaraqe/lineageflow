{ mkDerivation, aeson, base, lineageflow-declaration
, lineageflow-query, lineageflow-script
, lineageflow-viewer-interface, servant, stdenv, text
}:
mkDerivation {
  pname = "lineageflow-server-api";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base lineageflow-declaration lineageflow-query
    lineageflow-script lineageflow-viewer-interface servant text
  ];
  description = "Synopsis";
  license = stdenv.lib.licenses.gpl3;
}
