{ config, lib, pkgs, ... }:
{
  systemd.services.app = {
    wantedBy = [ "multi-user.target" ];
    script = ''
      ${(import ../.).prototype-hs-exe}/bin/cty-serve \
        --server-port 9000
    '';
  };
}
