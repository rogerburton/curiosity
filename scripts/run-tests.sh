#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

# This script runs the Haskell library test suite.

cabal test --test-show-details=direct --test-option=--format=checks
