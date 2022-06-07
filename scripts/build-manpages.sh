#! /usr/bin/env nix-shell
#! nix-shell -i bash -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.pandoc])"

set -e

pandoc --standalone --to man man/curiosity.7.md -o curiosity.7
pandoc --standalone --to man man/cty.1.md -o cty.1
