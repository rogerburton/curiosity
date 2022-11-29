#! /usr/bin/env bash

# Show, without building them, the Nix store paths of our default.nix
# attributes.

echo "content:"
nix-store -q --outputs $(nix-instantiate default.nix -A content 2>/dev/null)
echo "data:"
nix-store -q --outputs $(nix-instantiate default.nix -A data 2>/dev/null)
echo "man-pages:"
nix-store -q --outputs $(nix-instantiate default.nix -A man-pages 2>/dev/null)
echo "scenarios:"
nix-store -q --outputs $(nix-instantiate default.nix -A scenarios 2>/dev/null)
