with (import ../../nix/nixpkgs.nix);

(haskellPackages.callPackage ./. {}).env
