{ mkDerivation, base, containers, deepseq, LATS, lens
, lineageflow-algorithm, lineageflow-io-cbor, lineageflow-prelude
, stdenv
}:
mkDerivation {
  pname = "lineageflow-homogenization";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers deepseq LATS lens lineageflow-prelude
  ];
  executableHaskellDepends = [
    base containers deepseq LATS lens lineageflow-algorithm
    lineageflow-io-cbor lineageflow-prelude
  ];
  description = "Spatial and temporal homogenization for LineageFlow";
  license = stdenv.lib.licenses.gpl3;
}
