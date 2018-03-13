{ mkDerivation, base, base-prelude, constraint-classes, containers
, deepseq, foldl, generic-storable, LATS, lens, lineageflow-types
, linear, newtype, phantom-index, stdenv, storable-record
, storable-tuple, vector
}:
mkDerivation {
  pname = "lineageflow-prelude";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base base-prelude constraint-classes containers deepseq foldl
    generic-storable LATS lens lineageflow-types linear newtype
    phantom-index storable-record storable-tuple vector
  ];
  description = "A prelude adapted for the manipulation of spatio-temporal cell lineages within LineageFlow. More details in the README.";
  license = stdenv.lib.licenses.gpl3;
}
