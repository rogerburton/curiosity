#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

# This script runs the Haskell library test suite and the scenarios.

cabal test --test-show-details=direct
