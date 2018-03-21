{ nixpkgs ? import <nixpkgs> {} }:

let
  reflex-platform = nixpkgs.fetchFromGitHub {
    owner  = "reflex-frp";
    repo   = "reflex-platform";
    rev    = "f003577699ad5a47f8275dad4f05cdb15c4bcdf5";
    sha256 = "1fwg9cfz6p6zrlk1j5648r9hc5s2m62cwwv036sc7byb3pdhlxdr";
  };
in
  (import reflex-platform {}).project ({ pkgs, ... }: {
    overrides = import ./nix/client-overrides.nix pkgs;

    withHoogle = false;

    packages = {
      lineageflow-base = ./infrastructure/lineageflow-base;
      lineageflow-declaration = ./infrastructure/lineageflow-declaration;
      lineageflow-query = ./infrastructure/lineageflow-query;
      lineageflow-server-api = ./tools/lineageflow-server-api;
      lineageflow-client = ./tools/lineageflow-client;
      lineageflow-script = ./tools/lineageflow-script;
      lineageflow-viewer-interface = ./tools/lineageflow-viewer-interface;
    };

    shells = {
      ghcjs = [
        "lineageflow-base"
        "lineageflow-client"
        "lineageflow-declaration"
        "lineageflow-query"
        "lineageflow-server-api"
        "lineageflow-script"
        "lineageflow-viewer-interface"
      ];
    };
  })
