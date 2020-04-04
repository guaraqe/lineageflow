selfPkgs: superPkgs: self: super:
let
  dontCheck = superPkgs.haskell.lib.dontCheck;
  doJailbreak = superPkgs.haskell.lib.doJailbreak;
  arpackCompat = superPkgs.arpack.override { openblas = selfPkgs.openblasCompat; };

  eigen-src = superPkgs.fetchFromGitHub {
    owner  = "osidorkin";
    repo   = "haskell-eigen";
    rev    = "0f00b83482e93dc2f376d7fdf8af471153a3fc45";
    sha256 = "0bz3gsy8x6qrjki352f83ysv6fgscmkpy7i9x67sz67rsgaqlnly";
  };

  phantom-index-src = superPkgs.fetchFromGitHub {
    owner  = "guaraqe";
    repo   = "phantom-index";
    rev    = "9841a8d438134d83df033ee1995520e526b04ff5";
    sha256 = "13ibvgf5sllhw3sa022k3qb8yy71nrs34ifqar2k0wra3w5qrjj4";
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
    rev    = "89ee8b68d8ec204414a0455a3c2030ddcf3d41f4";
    sha256 = "18ravs2g9iw5bp2glw6g49k2r3vz9yzlpp252rkyy0a9blvjh0x9";
  };

  harpack-src = superPkgs.fetchFromGitHub {
    owner  = "guaraqe";
    repo   = "harpack";
    rev    = "f7c4ab6b7ee429c0dd49b94b164713b4c8992011";
    sha256 = "02ix2vsm3zrk1mq1cdv0fwsrskdh34462gcq08dhq98fjp45wv19";
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

    gtkglext = doJailbreak (super.gtkglext);
     
       # (self.callPackage ./deps/gtkglext.nix {
       #   gtkglext = selfPkgs.gnome2.gtkglext;
       #   gtk_system = selfPkgs.gnome2.gtk;
       #   libXmu = selfPkgs.xorg.libXmu;
       # });

    eigen =
      self.callCabal2nix "eigen" eigen-src {};

  }
