#! /usr/bin/env bash

# Show, without building them, the Nix store paths of our default.nix
# attributes.

echo "content:"
nix-store -q --outputs $(nix-instantiate default.nix -A content 2>/dev/null)
