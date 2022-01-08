# Central overlay that supplies all overlays that:
# 1. Make this package available.
# 2. Provide this particular package with a fixed point of overlayed packages, if they become needed.
let

  sources = import ./sources.nix;

  # We can overlay haskell packages here.
  haskellOverlays = [ ];

in haskellOverlays ++ [ (import ./overlay.nix) ]
