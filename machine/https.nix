{ pkgs, ... }:
{
  services.nginx = {
    virtualHosts."smartcoop.sh" = {
      forceSSL = true;
      enableACME = true;
    };
  };

  security.acme.acceptTerms = true;
  security.acme.certs = {
    "smartcoop.sh".email = "noteed@gmail.com";
  };
}
