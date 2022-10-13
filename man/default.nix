{ nixpkgs ? <nixpkgs>
}:

let
  pkgs = import nixpkgs {};
  sources = import ../nix/sources.nix {};
  nix-filter = import sources.nix-filter;

in
{
  man-pages = pkgs.stdenv.mkDerivation {
    name = "man";
    src = nix-filter {
      root = ../.;
      include = with nix-filter; [
        "man"
        "scripts"
      ];
    };
    nativeBuildInputs = [ pkgs.pandoc ];
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
