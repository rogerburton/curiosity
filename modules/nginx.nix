{ config, lib, pkgs, ... }:
{
  services.nginx = {
    enable = true;
    virtualHosts."smartcoop.sh" = {
      locations = {
        "/".proxyPass = "http://127.0.0.1:9100";
        "/documentation" = {
          proxyPass = "http://127.0.0.1:9100";
          extraConfig = "ssi on;";
        };
        "/static/" = {
          alias = (import ../.).static + "/";
        };
        # TODO How to avoid hard-coding this 0.1.0.0 path ?
        "/haddock/".alias = (import ../.).haddock + "/share/doc/curiosity-0.1.0.0/html/";
      };
    };
  };
}
