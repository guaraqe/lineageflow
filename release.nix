let
  pkgs = import <nixpkgs> {
    overlays = [ (import ./nix/overlay.nix) ];
  };
  call = pkgs.haskellPackages.callPackage;
in
  import ./nix/lineageflow.nix call
