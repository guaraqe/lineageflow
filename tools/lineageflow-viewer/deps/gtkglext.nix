{ mkDerivation, base, Cabal, glib, gtk, gtk2hs-buildtools, gtkglext
, pango, gtk_system, libXmu, mesa, zlib, stdenv
}:
mkDerivation {
  pname = "gtkglext";
  version = "0.13.1.1";
  sha256 = "15v40f21xlg5r2zidh77cfiq6ink1dxljbl59mf5sqyq5pjbdw3h";
  setupHaskellDepends = [ base Cabal gtk2hs-buildtools ];
  libraryHaskellDepends = [ base glib gtk pango ];
  libraryPkgconfigDepends = [ gtkglext gtk_system libXmu mesa zlib ];
  homepage = "http://projects.haskell.org/gtk2hs/";
  description = "Binding to the GTK+ OpenGL Extension";
  license = stdenv.lib.licenses.lgpl21;
}
