with (
  import <nixpkgs> {
    overlays = [ (import ../../lineageflow.nix) ];
  }
);

(haskellPackages.callPackage ./. {}).env
