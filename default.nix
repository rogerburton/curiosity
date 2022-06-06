let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };
  overlays = import ./nix/overlays.nix;

  os = import "${toString sources.nixpkgs}/nixos/lib/eval-config.nix" {
    modules = [
      ./machine/configuration.nix
      "${toString sources.nixpkgs}/nixos/modules/virtualisation/digital-ocean-image.nix"
      ./machine/disk-size.nix
    ];
  };

  qemu = import "${toString sources.nixpkgs}/nixos/lib/eval-config.nix" {
    modules = [
      ./machine/configuration.nix
      "${toString sources.nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
      ./machine/no-gui.nix
    ];
  };
in with nixpkgs.haskellPackages;
  { inherit
      prototype-hs-lib
      prototype-hs-exe;

    # Build with nix-build -A <attr>
    binaries = prototype-hs-exe;
    image = os.config.system.build.digitalOceanImage;
    runvm = qemu.config.system.build.vm;
  }
