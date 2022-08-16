{ config, lib, pkgs, ... }:
{
  systemd.services.curiosity = {
    wantedBy = [ "multi-user.target" ];
    script = ''
      # TODO system is meaningless for now.
      ${(import ../.).binaries}/bin/cty \
        --memory \
        --user system \
        serve \
        --server-port 9100 \
        --static-dir ${(import ../.).content} \
        --data-dir ${(import ../.).data}
    '';
  };
}
