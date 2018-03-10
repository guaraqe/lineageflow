{ mkDerivation, base, containers, lens, lineageflow-algorithm
, lineageflow-io-cbor, lineageflow-prelude, nonempty-alternative
, optparse-applicative, rebase, rlist, serialise, stdenv
, storable-record, unordered-containers
}:
mkDerivation {
  pname = "lineageflow-tracking";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers lens lineageflow-algorithm lineageflow-io-cbor
    lineageflow-prelude nonempty-alternative rebase rlist serialise
    storable-record
  ];
  executableHaskellDepends = [
    base containers lens lineageflow-algorithm lineageflow-io-cbor
    lineageflow-prelude nonempty-alternative optparse-applicative rlist
    serialise storable-record unordered-containers
  ];
  description = "A framework for lineages evolving on time";
  license = stdenv.lib.licenses.gpl3;
}
