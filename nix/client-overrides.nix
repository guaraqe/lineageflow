pkgs: self: super:
  let
    reflex-elm-src = pkgs.fetchFromGitHub {
      owner  = "guaraqe";
      repo   = "reflex-elm";
      rev    = "60c9ec1393efbd9b1a6307a2a71b434e791b10ff";
      sha256 = "05jm5ny1d9ljj5h5nbmj2dabfsyn06h6pzdgnxnvm2wv3s7qj2v0";
    };
  in
    {
      rank2classes = pkgs.haskell.lib.dontCheck super.rank2classes;
      reflex-elm = self.callPackage reflex-elm-src {};
      servant-reflex = pkgs.haskell.lib.doJailbreak super.servant-reflex;
    }
