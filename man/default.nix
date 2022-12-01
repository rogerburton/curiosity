{ nixpkgs ? import (import ../nix/sources.nix {}).nixpkgs {}
}:

let
  sources = import ../nix/sources.nix {};
  nix-filter = import sources.nix-filter;

in
{
  man-pages = nixpkgs.stdenv.mkDerivation {
    name = "man-pages";
    src = nix-filter {
      root = ../.;
      include = with nix-filter; [
        "man"
        "scripts/doc.Makefile"
      ];
    };
    nativeBuildInputs = [ nixpkgs.pandoc ];
    installPhase = ''
      # Make sure we don't use an already built _site/.
      rm -rf _site

      make -f scripts/doc.Makefile man

      mkdir -p $out/share/man/man{1,7}
      mv cty.1.gz $out/share/man/man1/
      mv curiosity.7.gz $out/share/man/man7/
    '';
  };
}
