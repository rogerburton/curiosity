self: super:
let

  lib = super.lib;
  sources = import ./sources.nix;
  contents = import ./contents.nix { nixpkgs = super; };

  inherit (super.lib.attrsets) mapAttrs;
  inherit (import sources.gitignore { inherit lib; }) gitignoreFilter;

  ourOverrides = selfh: superh:
    let

      customFilter = src:
        let
          # IMPORTANT: use the following let binding to memoize info about the
          # Git directories.
          # (The comment is from the original example at
          # https://github.com/hercules-ci/gitignore.nix/blob/master/docs/gitignoreFilter.md)
          gitIgnoredSrc = gitignoreFilter src;
        in
          path: type: gitIgnoredSrc path type;

      cleanedSrc = src: lib.sources.sourceFilesBySuffices
        ( lib.cleanSourceWith {
            filter = customFilter src;
            src = src;
            name = "source";
          })
        [".cabal" ".hs" ".json"]; # Keep JSON files for the test suite.

      callCabalOn = name: dir:
        selfh.callCabal2nix "${name}" (cleanedSrc dir) { };

    in mapAttrs callCabalOn contents.pkgList;

in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides =
      lib.composeExtensions (old.overrides or (_: _: { })) ourOverrides;
  });
}
