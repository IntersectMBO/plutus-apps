# NOTE: This flake is only provided as interface to `bitte` and shouldn't be used otherwise
#
# Occasionally building flake builds will segfault. The workaround for this is to
# disable the garbage collector `GC_DONT_GC=1  nix build .#docs
#
# In case you are not sure if you should be using this flake, the answer is: No.
{
  description = "plutus-apps flake for pinning sources and bitte deployments";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    # We intentionally import nixpkgs and haskell.nix as non-flakes, to match the
    # flake-free normal build workflow exactly.
    nixpkgs = {
      url = "github:NixOS/nixpkgs";
      flake = false;
    };
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      flake = false;
    };
    cardano-repo-tool = {
      url = "github:input-output-hk/cardano-repo-tool";
      flake = false;
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat/fixes";
      flake = false;
    };
    gitignore-nix = {
      url = "github:hercules-ci/gitignore.nix";
      flake = false;
    };
    hackage-nix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    haskell-language-server = {
      url = "github:haskell/haskell-language-server?ref=1.8.0.0";
      flake = false;
    };
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      flake = false;
    };
    plutus-core = {
      url = "github:input-output-hk/plutus";
      flake = false;
    };
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      flake = false;
    };
    spago2nix = {
      url = "github:justinwoo/spago2nix";
      flake = false;
    };
    sphinxcontrib-haddock = {
      url = "github:michaelpj/sphinxcontrib-haddock";
      flake = false;
    };
    stackage-nix = {
      url = "github:input-output-hk/stackage.nix";
      flake = false;
    };
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    tullia = {
      url = "github:input-output-hk/tullia";
      # Can't follow since nixpkgs is set to flake=false here
      # inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, flake-utils, tullia, ... }@inputs:
    (flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        topLevel = import ./. {
          inherit system;
          sources = inputs;
        };
      in
      {
        packages = topLevel.bitte-packages;
        legacyPackages = topLevel;

        hydraJobs = import ./hydraJobs.nix { inherit system; };

      } //

      tullia.fromSimple system (import ./nix/tullia.nix)
    ));

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
