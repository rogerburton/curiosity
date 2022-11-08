#!/usr/bin/env bash

scriptdir=$(dirname $0)
nix-shell "$scriptdir"/../default.nix -A shell
