{ mkDerivation, base, deepseq, lens, lineageflow-algorithm
, lineageflow-io-cbor, lineageflow-prelude, stdenv
}:
mkDerivation {
  pname = "lineageflow-trajectories";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base deepseq lens lineageflow-prelude ];
  executableHaskellDepends = [
    base deepseq lens lineageflow-algorithm lineageflow-io-cbor
    lineageflow-prelude
  ];
  description = "Synopsis";
  license = stdenv.lib.licenses.agpl3;
}
