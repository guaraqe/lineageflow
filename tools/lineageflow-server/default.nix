{ mkDerivation, aeson, base, blaze-html, containers, directory
, exceptions, filepath, lens, lineageflow-algorithm
, lineageflow-database-sqlite, lineageflow-script
, lineageflow-server-api, lineageflow-viewer-interface, markdown
, mtl, process, random, servant, servant-server, stdenv, temporary
, text, transformers, unix, wai, wai-cors, wai-extra, warp, yaml
}:
mkDerivation {
  pname = "lineageflow-server";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html containers directory exceptions filepath lens
    lineageflow-algorithm lineageflow-database-sqlite
    lineageflow-script lineageflow-server-api
    lineageflow-viewer-interface markdown mtl process random servant
    servant-server temporary text transformers unix wai wai-cors
    wai-extra warp yaml
  ];
  description = "A server for LineageFlow algorithms";
  license = stdenv.lib.licenses.agpl3;
}
