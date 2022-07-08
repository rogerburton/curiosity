#! /usr/bin/env nix-shell
#! nix-shell -i bash -p entr gnumake pandoc"

make -f scripts/doc.Makefile
