{ config, lib, pkgs,
  ... }:
{
  services.sshd.enable = true;

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  users.users.root.password = "nixos";
  services.openssh.permitRootLogin = lib.mkDefault "yes";
  services.mingetty.autologinUser = lib.mkDefault "root";

  imports = [
    ../modules/app.nix
    ../modules/nginx.nix
  ];

  environment.systemPackages = [
    (import ../.).binaries
    (import ../.).man-pages # TODO Man pages should come with .binaries ?
  ];
}
