{ docs, pkgs }:
let
  staticSite = pkgs.callPackage ./static-site.nix { };
  playgroundStatic = pkgs.callPackage ./playground-static.nix { inherit staticSite; docs = docs.site; };
in
{
  # We're disabling this because we don't want Bitte to host the plutus-apps docs website anymore.
  # plutus-playground-client-entrypoint = playgroundStatic {
  #   variant = "plutus";
  # };
}
