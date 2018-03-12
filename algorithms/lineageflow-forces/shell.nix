with (
  import <nixpkgs> {
    overlays = [ (import ../../nix/overlay.nix) ];
  }
);

(haskellPackages.callPackage ./. {}).env
