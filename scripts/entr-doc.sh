#! /usr/bin/env nix-shell
#! nix-shell -i bash -p entr gnumake pandoc"

# Render .md files to .html as soon as they are modified.

make -f scripts/doc.Makefile entr
