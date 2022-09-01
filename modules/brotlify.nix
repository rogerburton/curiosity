# This is a copy of the file presented in the blog post
# https://dee.underscore.world/blog/brotlifying-nginx/
# Differences:
#   - Added "svg".
#   - Commented out version and pname.
{ stdenvNoCC
, lib
, brotli
, element-web }:
{ src
, fileExtensions ? [
  "html" "js" "css" "json" "txt" "ttf" "ico" "svg" "wasm"
]}:
let
  findQuery = lib.flatten (lib.intersperse "-o"
    (map (ext: [ "-iname" (lib.escapeShellArg "*.${ext}") ]) fileExtensions));
in stdenvNoCC.mkDerivation {
  inherit src;
  name = "brotlified";
  # inherit (src) version;
  # pname = "${src.pname}-brotlified";

  nativeBuildInputs = [
    brotli
  ];

  buildPhase = ''
    find . -type f \( \
      ${lib.concatStringsSep " " findQuery} \
      \) -print0 | xargs -0 -P $NIX_BUILD_CORES -I{} brotli -vZ {}
  '';

  installPhase = ''
    find . -type f -iname '*.br' -exec install -m444 -D {} $out/{} \;
  '';
}
