{ mkDerivation, base, bytestring, containers
, lineageflow-clustering, lineageflow-derivatives
, lineageflow-export, lineageflow-homogenization
, lineageflow-import, lineageflow-prelude, lineageflow-tracking
, lineageflow-trajectories, lineageflow-triangulations
, optparse-applicative, stdenv
}:
mkDerivation {
  pname = "bioemergences-clustering";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers lineageflow-clustering
    lineageflow-derivatives lineageflow-export
    lineageflow-homogenization lineageflow-import lineageflow-prelude
    lineageflow-tracking lineageflow-trajectories
    lineageflow-triangulations optparse-applicative
  ];
  description = "Clustering pipeline for BioEmergences";
  license = stdenv.lib.licenses.agpl3;
}
