{
  description = "Plutus Apps";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/e14f9fb57315f0d4abde222364f19f88c77d2b79";
    };
    std = {
      url = "github:divnix/std/94a90eedb9cfc115b12ae8f6622d9904788559e4";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat/7da118186435255a30b5ffeabba9629c344c0bec";
      flake = false;
    };
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix/e601c9ce609af07a78edf1c57f5985931788aeb2";
      inputs = {
        hackage.follows = "hackage-nix";
        nixpkgs.follows = "nixpkgs";
      };
    };
    hackage-nix = {
      url = "github:input-output-hk/hackage.nix/adbe11fcbabc1f192b98776c7c8b9f26d4e831c0";
      flake = false;
    };
    sphinxcontrib-haddock = {
      url = "github:michaelpj/sphinxcontrib-haddock/f3956b3256962b2d27d5a4e96edb7951acf5de34";
      flake = false;
    };
    gitignore-nix = {
      url = "github:hercules-ci/gitignore.nix/a20de23b925fd8264fd7fad6454652e142fd7f73";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix/7fc7625a9ab2ba137bc70ddbc89a13d3fdb78c8b";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix/ab608394886fb04b8a5df3cb0bab2598400e3634";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=85510200dd0dc758d72bc1ada11ff5855e5d46b7";
      flake = false;
    };
    haskell-language-server = {
      # TODO Bump to 1.9.0.0 once plutus-apps hits GHC 9.2
      url = "github:haskell/haskell-language-server?ref=1.8.0.0";
      flake = false;
    };
    plutus-core = {
      url = "github:input-output-hk/plutus/ab3d01280109c1b5d1522f9bae475f5984fbc055";
      flake = false;
    };
  };

  outputs = inputs:
    inputs.std.growOn
      {
        inherit inputs;
        cellsFrom = ./__std__;
        cellBlocks = [
          (inputs.std.devshells "devshells")
          (inputs.std.installables "packages")
          (inputs.std.installables "devcontainer")
          (inputs.std.functions "library")
          (inputs.std.installables "hydra-jobs")
        ];
      }
      {
        devShells = inputs.std.harvest inputs.self [ "plutus-apps" "devshells" ];
        packages = inputs.std.harvest inputs.self [ "plutus-apps" "packages" ];
      }
      {
        packages = inputs.std.harvest inputs.self [ "plutus-apps" "devcontainer" ];
      }
      {
        hydraJobs = inputs.std.harvest inputs.self [ "automation" "hydra-jobs" ];
      };

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
