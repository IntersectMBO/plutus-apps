{ docs, pkgs }:
let
  staticSite = pkgs.callPackage ./static-site.nix { };
  playgroundStatic = pkgs.callPackage ./playground-static.nix { inherit staticSite; docs = docs.site; };
in
{
  plutus-playground-client-entrypoint = playgroundStatic {
    variant = "plutus";
  };
}
