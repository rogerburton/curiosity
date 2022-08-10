{ config, lib, pkgs, ... }:
{
  systemd.services.app = {
    wantedBy = [ "multi-user.target" ];
    script = ''
      ${(import ../.).binaries}/bin/cty --user TODO serve \
        --server-port 9000 \
        --static-dir ${(import ../.).content} \
        --data-dir ${(import ../.).data}
    '';
  };
}
