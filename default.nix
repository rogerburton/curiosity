let sources = import ./nix/sources.nix;
    nixpkgs = import sources.nixpkgs { inherit overlays; };
    overlays = import ./nix/overlays.nix; 
in  with nixpkgs.haskellPackages;
  { inherit
      prototype-hs-lib
      prototype-hs-exe
      prototype-hs-example;
  } 
