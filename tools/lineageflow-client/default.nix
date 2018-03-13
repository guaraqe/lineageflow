{ mkDerivation, aeson, base, bimap, containers, file-embed, jsaddle
, jsaddle-dom, lens, lineageflow-script, lineageflow-server-api
, lineageflow-viewer-interface, mtl, profunctors, reflex
, reflex-dom, reflex-elm, servant, servant-reflex, stdenv, text
, time
}:
mkDerivation {
  pname = "lineageflow-client";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bimap containers file-embed jsaddle jsaddle-dom lens
    lineageflow-script lineageflow-server-api
    lineageflow-viewer-interface mtl profunctors reflex reflex-dom
    reflex-elm servant servant-reflex text time
  ];
  description = "Synopsis";
  license = stdenv.lib.licenses.gpl3;
}
