selfPkgs: superPkgs: self: super:
let
  dontCheck = superPkgs.haskell.lib.dontCheck;
  doJailbreak = superPkgs.haskell.lib.doJailbreak;
  arpackCompat = superPkgs.arpack.override { openblas = selfPkgs.openblasCompat; };

  cborg-src = superPkgs.fetchFromGitHub {
    owner  = "well-typed";
    repo   = "cborg";
    rev    = "99ec2b038538b279625c10640e3d5e8d80cd7a1d";
    sha256 = "0fpwi6wbhkc3xqfw2sg0zr74w0a8p3dwb86wawdg97g6dyqlhpg0";
  };

  eigen-src = superPkgs.fetchFromGitHub {
    owner  = "osidorkin";
    repo   = "haskell-eigen";
    rev    = "0f00b83482e93dc2f376d7fdf8af471153a3fc45";
    sha256 = "0bz3gsy8x6qrjki352f83ysv6fgscmkpy7i9x67sz67rsgaqlnly";
  };

  phantom-index-src = superPkgs.fetchFromGitHub {
    owner  = "guaraqe";
    repo   = "phantom-index";
    rev    = "c196517df20060a9985faad45f5aa5df4e5df596";
    sha256 = "18b424pbzj8191i558xdr4lxbccnw5bqqqsi0drbka84ilxj0qsl";
  };

  constraint-classes-src = superPkgs.fetchFromGitHub {
    owner  = "guaraqe";
    repo   = "constraint-classes";
    rev    = "c14899404e25f932c976905879e95d0cb5a856b";
    sha256 = "1rvk81q0faashkbfg02ql6bdn6rhm3clk76q2xi9602yqgxpwbsb";
  };

  LATS-src = superPkgs.fetchFromGitHub {
    owner  = "guaraqe";
    repo   = "LATS";
    rev    = "d6fb3b23162db83c96e27bfcca9f42094dd0499f";
    sha256 = "0c5qp5fm3a2mz375r7fwl21ghq3xk5pczmng01n6qs7z2j1xwdnp";
  };

  harpack-src = superPkgs.fetchFromGitHub {
    owner  = "guaraqe";
    repo   = "harpack";
    rev    = "da5b0d13072fb9e4cc27dd89bfc4772c8a022644";
    sha256 = "0vhsh3yjlpphsf4mpky7cs3yv5mvxm49ix322ycx8l5nybh9abwk";
  };

  qhull-simple-src = superPkgs.fetchFromGitHub {
    owner  = "guaraqe";
    repo   = "qhull-simple";
    rev    = "b46f873cac72d4236e779139002190e5aa7b53f4";
    sha256 = "0kflxsrqm8n7v7x4pzwx4zzd0lyd5lq70ry3r0zv1wiy13glf1i6";
  };

in
  {
    clustering = dontCheck super.clustering;
    rank2classes = dontCheck super.rank2classes;

    constraint-classes =
      self.callCabal2nix "constraint-classes" constraint-classes-src {};
    phantom-index =
      self.callCabal2nix "phantom-index" phantom-index-src {};
    LATS =
      self.callCabal2nix "LATS" LATS-src {};

    generic-storable =
      doJailbreak (dontCheck super.generic-storable);
    harpack =
      self.callCabal2nix "harpack" harpack-src { arpack = arpackCompat; };
    qhull-simple =
      self.callCabal2nix "qhull-simple" qhull-simple-src {};

    gtkglext =
      self.callPackage ./deps/gtkglext.nix {
        gtkglext = selfPkgs.gnome2.gtkglext;
        gtk_system = selfPkgs.gnome2.gtk;
        libXmu = selfPkgs.xorg.libXmu;
      };

    cborg =
      self.callCabal2nix "cborg" (cborg-src + "/cborg") {};
    serialise =
      self.callCabal2nix "serialise" (cborg-src + "/serialise") {};

    eigen =
      self.callCabal2nix "eigen" eigen-src {};
  }
