{ mkDerivation, aeson, base, containers, lens, rank2classes, stdenv
, text, unordered-containers
}:
mkDerivation {
  pname = "lineageflow-base";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers lens rank2classes text unordered-containers
  ];
  description = "Synopsis";
  license = stdenv.lib.licenses.gpl3;
}
