{ mkDerivation, aeson, async, base, bytestring, file-embed, lens
, lineageflow-declaration, lineageflow-io, lineageflow-types
, optparse-applicative, process, stdenv, template-haskell
, temporary, text, yaml
}:
mkDerivation {
  pname = "lineageflow-algorithm";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring file-embed lens lineageflow-declaration
    lineageflow-io lineageflow-types optparse-applicative process
    template-haskell temporary text yaml
  ];
  description = "Algorithm interface generator for LineageFlow algorithms";
  license = stdenv.lib.licenses.gpl3;
}
