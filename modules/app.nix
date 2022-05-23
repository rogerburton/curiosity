{ config, lib, pkgs, ... }:
{
  systemd.services.app = {
    wantedBy = [ "multi-user.target" ];
    script = ''
      ${(import ../.).prototype-hs-exe}/bin/app \
        --server-port 9000
    '';
  };
}
