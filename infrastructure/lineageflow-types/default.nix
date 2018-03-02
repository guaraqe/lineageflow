{ mkDerivation, base, generic-storable, lens, linear, mtl, newtype
, stdenv, storable-record, storable-tuple, transformers
}:
mkDerivation {
  pname = "lineageflow-types";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base generic-storable lens linear mtl newtype storable-record
    storable-tuple transformers
  ];
  description = "Fundamental types used in LineageFlow algorithms";
  license = stdenv.lib.licenses.gpl3;
}
