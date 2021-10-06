{
  description = "Generate a PureScript API client for you servant API";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.easy-ps = {
    url = "github:justinwoo/easy-purescript-nix";
    flake = false;
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, easy-ps }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          servant-purescript =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.servant-purescript.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."servant-purescript:test:servant-purescript";
      devShell = pkgs.servant-purescript.shellFor {
        withHoogle = true;
        tools = {
          cabal = "latest";
          hlint = "latest";
          haskell-language-server = "latest";
        };
        exactDeps = true;
        buildInputs = with pkgs; with import easy-ps { inherit pkgs; }; [
          ghcid
          nixpkgs-fmt
          purs
          spago
        ];
      };
    });
}
