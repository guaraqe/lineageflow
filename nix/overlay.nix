selfPkgs: superPkgs:
{
  haskellPackages = superPkgs.haskellPackages.override {
    overrides = self: super:
    let
      lineageflow = import ./lineageflow.nix self.callPackage;
      extra = import ./overrides.nix selfPkgs superPkgs self super;
    in
      extra // lineageflow ;
  };
}
