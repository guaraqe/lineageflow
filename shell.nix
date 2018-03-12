{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) stdenv;
in
  stdenv.mkDerivation {
    name = "lineageflow";
    buildInputs = with (import ./release.nix);
      [
        lineageflow-tracking
        lineageflow-derivatives
        lineageflow-homogenization
        lineageflow-triangulations
        lineageflow-trajectories
        lineageflow-clustering
        lineageflow-server
        lineageflow-plot
        lineageflow-viewer
      ];
  }
