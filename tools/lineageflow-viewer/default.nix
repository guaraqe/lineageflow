{ mkDerivation, base, containers, FTGL, gtk, gtkglext, inline-c
, lens, lineageflow-algorithm, lineageflow-io-cbor
, lineageflow-prelude, lineageflow-statistics
, lineageflow-triangulations, lineageflow-viewer-interface, OpenGL
, optparse-applicative, scientific, statistics, stdenv, text, yaml
}:
mkDerivation {
  pname = "lineageflow-viewer";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    base containers FTGL gtk gtkglext inline-c lens
    lineageflow-algorithm lineageflow-io-cbor lineageflow-prelude
    lineageflow-statistics lineageflow-triangulations
    lineageflow-viewer-interface OpenGL optparse-applicative scientific
    statistics text yaml
  ];
  description = "Visualisation of measurements for LineageFlow";
  license = stdenv.lib.licenses.agpl3;
}
