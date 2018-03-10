{ mkDerivation, base, lens, lineageflow-prelude, statistics, stdenv
}:
mkDerivation {
  pname = "lineageflow-statistics";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base lens lineageflow-prelude statistics
  ];
  description = "Statistics for LineageFlow";
  license = stdenv.lib.licenses.agpl3;
}
