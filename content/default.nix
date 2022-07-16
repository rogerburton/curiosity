{ nixpkgs ? <nixpkgs>
}:

let
  pkgs = import nixpkgs {};

in
{
  html.all = pkgs.stdenv.mkDerivation {
    name = "content";
    # TODO We only need content/, man/, scripts/
    src = ../.;
    nativeBuildInputs = [ pkgs.mandoc pkgs.pandoc ];
    installPhase = ''
      # Make sure we don't use an already built _site/.
      rm -rf _site

      make -f scripts/doc.Makefile
      mv _site $out
    '';
  };
}
