{ mkDerivation, aeson, base, Chart, colour, data-default-class
, lens, lineageflow-query, stdenv
}:
mkDerivation {
  pname = "lineageflow-plot-interface";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base Chart colour data-default-class lens lineageflow-query
  ];
  description = "Interface for plots in LineageFlow";
  license = stdenv.lib.licenses.agpl3;
}
