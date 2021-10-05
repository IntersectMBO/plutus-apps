{
  description = "Generate PureScript data types from Haskell data types";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          purescript-bridge =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.purescript-bridge.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."purescript-bridge:test:purescript-bridge";
      devShell = pkgs.purescript-bridge.shellFor {
        withHoogle = true;
        tools = {
          cabal = "latest";
          hlint = "latest";
          haskell-language-server = "latest";
        };

        exactDeps = true;

        buildInputs = with pkgs; [
          nixpkgs-fmt
          ghcid
        ];
      };
    });
}
