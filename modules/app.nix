{ config, lib, pkgs, ... }:
{
  systemd.services.app = {
    wantedBy = [ "multi-user.target" ];
    script = ''
      ${(import ../.).prototype-hs-example}/bin/prototype-hs-example-exe \
        --server-port 9000
    '';
  };
}
