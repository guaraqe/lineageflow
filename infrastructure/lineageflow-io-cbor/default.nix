{ mkDerivation, base, lineageflow-io, lineageflow-prelude
, serialise, stdenv
}:
mkDerivation {
  pname = "lineageflow-io-cbor";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base lineageflow-io lineageflow-prelude serialise
  ];
  description = "An IO method for LineageFlow implemented with CBOR files";
  license = stdenv.lib.licenses.gpl3;
}
