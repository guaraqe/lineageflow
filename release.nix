let
  pkgs = import ./nix/nixpkgs.nix;
in
  import ./nix/lineageflow.nix pkgs.haskellPackages.callPackage
