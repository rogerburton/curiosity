{ nixpkgs ? import (import ../nix/sources.nix {}).nixpkgs {}}:
let
  binaries = (import ../.).binaries;
  man-pages = (import ../.).man-pages;

  bash-wrapper = nixpkgs.writeTextFile {
    name = "bash-wrapper";
    executable = true;
    destination = "/bin/bash-wrapper";
    text = ''
      #!${nixpkgs.bashInteractive}/bin/bash
      # TODO Running the following line manually works, but here, the exec
      # call on the last line will no retain its effect.
      source <(cty --bash-completion-script /bin/cty)
      echo "Welcome to the Curiosity environment."
      echo "Run \`man curiosity\` for the manual."
      exec ${nixpkgs.bashInteractive}/bin/bash
    '';
  };
in

nixpkgs.dockerTools.buildImage {
  name = "curiosity";
  # cat (from coreutils) and gzip are necessary for man
  contents = [
    bash-wrapper
    binaries
    nixpkgs.bashInteractive
    nixpkgs.coreutils
    nixpkgs.gzip
    nixpkgs.man
  ];
  config = {
    Cmd = [
      "${bash-wrapper}/bin/bash-wrapper"
    ];
    Env = [
        "MANPATH=${man-pages}/share/man"
        "CURIOSITY_STATIC_DIR=${(import ../.).content}"
        "CURIOSITY_DATA_DIR=${(import ../.).data}"
        "CURIOSITY_SCENARIOS_DIR=${(import ../.).scenarios}"
    ];
    ExposedPorts = {
      "9000/tcp" = {};
    };
  };
  runAsRoot = ''
    # /tmp is for the curiosity.log file
    mkdir -p /tmp
  '';
}
