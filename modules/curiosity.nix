{ config, lib, pkgs, ... }:
{
  systemd.services.curiosity = {
    wantedBy = [ "multi-user.target" ];
    script = ''
      # TODO system is meaningless for now.
      ${(import ../.).binaries}/bin/cty \
        --user system \
        serve \
        --server-port 9000 \
        --static-dir ${(import ../.).content} \
        --data-dir ${(import ../.).data}
    '';
  };
}
