self: super:
let

  lib = super.lib;
  sources = import ./sources.nix;
  contents = import ./contents.nix { nixpkgs = super; };

  inherit (super.lib.attrsets) mapAttrs;
  inherit (import sources.gitignore { inherit lib; }) gitignoreSource;

  ourOverrides = selfh: superh:
    let

      callCabalOn = name: dir:
        selfh.callCabal2nix "${name}" (gitignoreSource dir) { };

    in mapAttrs callCabalOn contents.pkgList;
       # // { type-list = selfh.callHackage "type-list" "0.5.0.0" {};
       #      singletons = selfh.callHackage "singletons" "2.2" {};
       #      th-desugar = selfh.callHackage "th-desugar" "1.6" {};
       #    };

in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) ourOverrides;
  });
}
