{ pkgs ? import ./nix/nixpkgs.nix }:

let
  inherit (pkgs) stdenv;
in
  stdenv.mkDerivation {
    name = "lineageflow";
    buildInputs = with (import ./release.nix);
      [
        lineageflow-tracking
        lineageflow-deviations
        lineageflow-derivatives
        lineageflow-homogenization
        lineageflow-triangulations
        lineageflow-trajectories
        lineageflow-clustering
        lineageflow-server
        lineageflow-plot
        lineageflow-viewer
        lineageflow-import
        lineageflow-export
      ];
  }
