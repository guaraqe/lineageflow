{ mkDerivation, aeson, base, bytestring, Chart, Chart-cairo, colour
, data-default-class, directory, filepath, foldl, lens
, lineageflow-io-cbor, lineageflow-plot-interface
, lineageflow-prelude, lineageflow-statistics, optparse-applicative
, process, statistics, stdenv, temporary, vector, yaml
}:
mkDerivation {
  pname = "lineageflow-plot";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base Chart Chart-cairo colour data-default-class directory filepath
    foldl lens lineageflow-plot-interface lineageflow-prelude
    lineageflow-statistics process statistics temporary vector
  ];
  executableHaskellDepends = [
    aeson base bytestring Chart Chart-cairo colour data-default-class
    directory filepath foldl lens lineageflow-io-cbor
    lineageflow-plot-interface lineageflow-prelude
    lineageflow-statistics optparse-applicative process statistics
    temporary vector yaml
  ];
  description = "Plotting for LineageFlow";
  license = stdenv.lib.licenses.agpl3;
}
