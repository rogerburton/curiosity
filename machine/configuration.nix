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

  programs.bash.loginShellInit = ''
    echo "Welcome to the Curiosity environment."
    echo "Run \`man curiosity\` for the manual."
  '';

  programs.bash.interactiveShellInit = ''
    source <(cty             --bash-completion-script `which cty`)
    source <(cty-interactive --bash-completion-script `which cty-interactive`)
    source <(cty-parse       --bash-completion-script `which cty-parse`)
    source <(cty-repl        --bash-completion-script `which cty-repl`)
    source <(cty-repl-2      --bash-completion-script `which cty-repl-2`)
    source <(cty-serve       --bash-completion-script `which cty-serve`)
    source <(cty-sock        --bash-completion-script `which cty-sock`)
  '';
}
