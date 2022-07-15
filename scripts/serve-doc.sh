 #! /usr/bin/env bash

# Serve the _site/ directory with browser-sync.

cd _site/

# Use a more recent nixpkgs, which contains browser-sync, than my host one.
NIX_PATH="nixpkgs=/home/thu/projects/nixpkgs/" nix-shell -p nodePackages.browser-sync \
  --run 'browser-sync start --server --files "*.html" --watch --no-open'
