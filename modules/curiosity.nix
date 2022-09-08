{ config, lib, pkgs, ... }:
{
  systemd.services.curiosity = {
    wantedBy = [ "multi-user.target" ];
    script = ''
      # Run from a state file, reset to a known state.
      # TODO system is meaningless for now.
      rm -f /tmp/state.json
      ${(import ../.).binaries}/bin/cty \
        --state /tmp/state.json \
        --user system \
        init
      ${(import ../.).binaries}/bin/cty \
        --state /tmp/state.json \
        --user system \
        run ${(import ../.).scenarios}/state-0.txt

      ${(import ../.).binaries}/bin/cty \
        --state /tmp/state.json \
        --user system \
        serve \
        --server-port 9100 \
        --static-dir ${(import ../.).content} \
        --data-dir ${(import ../.).data} \
        --log /tmp/curiosity.log
    '';
  };
}
