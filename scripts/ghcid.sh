#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghcid --command 'cabal repl prototype-hs-example-exe'

# This starts faster but I guess that script will get obsolete quickly.
#
#   ghcid --command ./ghci.sh
