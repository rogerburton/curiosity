let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };
  overlays = import ./nix/overlays.nix;

  os = import "${toString sources.nixpkgs}/nixos/lib/eval-config.nix" {
    modules = [
      ./machine/configuration.nix
      "${toString sources.nixpkgs}/nixos/modules/virtualisation/digital-ocean-image.nix"
      ./machine/disk-size.nix
      ./machine/https.nix
    ];
  };

  qemu = import "${toString sources.nixpkgs}/nixos/lib/eval-config.nix" {
    modules = [
      ./machine/configuration.nix
      "${toString sources.nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
      ./machine/no-gui.nix
    ];
  };
in rec
  {
    # Build with nix-build -A <attr>
    # binaries + haddock are also available as binaries.all.
    binaries = nixpkgs.haskellPackages.curiosity;
    haddock = nixpkgs.haskellPackages.curiosity.doc;
    content = (import ./content {}).html.all;
    data = (import ./content {}).data;
    man-pages = (import ./man {}).man-pages;
    toplevel = os.config.system.build.toplevel;
    image = os.config.system.build.digitalOceanImage;
    runvm = qemu.config.system.build.vm;

    # A shell to try out our binaries
    # TODO Can this be defined in shell.nix instead ?
    # Run with nix-shell default.nix -A shell
    shell = nixpkgs.mkShell {
      buildInputs = [
        binaries
        man-pages # This works in environment.systemPackages but here we need
                  # to set MANPATH below.
                  # I guess it would work if it was packaged with the binaries.
      ];
      shellHook = ''
        source <(cty             --bash-completion-script `which cty`)
        source <(cty-sock        --bash-completion-script `which cty-sock`)
        export MANPATH=${man-pages}/share/man
      '';
    };
  }
