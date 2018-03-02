{ mkDerivation, aeson, async, base, bytestring, file-embed, lens
, lineageflow-declaration, lineageflow-io, lineageflow-types
, optparse-applicative, stdenv, template-haskell, text, yaml
}:
mkDerivation {
  pname = "lineageflow-executable";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring file-embed lens lineageflow-declaration
    lineageflow-io lineageflow-types optparse-applicative
    template-haskell text yaml
  ];
  description = "Synopsis";
  license = stdenv.lib.licenses.gpl3;
}
