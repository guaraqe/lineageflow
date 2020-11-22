with import ../../nix/nixpkgs.nix;
let
  bioemergences-clustering =
    haskell.lib.justStaticExecutables (haskellPackages.callPackage ./. {});
in
  dockerTools.buildImage {
    name = "bioemergences-clustering";
    config = {
      Cmd = [
        "${bioemergences-clustering}/bin/bioemergences-clustering"
      ];
    };
  }
