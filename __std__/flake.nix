{
  description = "Plutus Apps";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs";
    };
    std = {
      url = "github:divnix/std";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat/fixes";
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
      url = "github:input-output-hk/hackage.nix/10428b0a63b30782f26b1e01d6c3d383db75ae43";
      flake = false;
    };
    sphinxcontrib-haddock = {
      url = "github:michaelpj/sphinxcontrib-haddock";
      flake = false;
    };
    gitignore-nix = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    haskell-language-server = {
      url = "github:haskell/haskell-language-server?ref=1.8.0.0";
      flake = false;
    };
    plutus-core = {
      url = "github:input-output-hk/plutus";
      flake = false;
    };
    cardano-node = {
      url = "github:input-output-hk/cardano-node/1.35.4";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.std.growOn
      {
        inherit inputs;
        cellsFrom = ./cells;
        cellBlocks = [
          (inputs.std.devshells "devshells")
          (inputs.std.installables "packages")
          (inputs.std.functions "library")
          (inputs.std.installables "hydraJobs")
        ];
      }
      {
        devShells = inputs.std.harvest inputs.self [ "plutus-apps" "devshells" ];
        packages = inputs.std.harvest inputs.self [ "plutus-apps" "packages" ];
      }
      {
        hydraJobs = inputs.std.harvest inputs.self [ "automation" "hydraJobs" ];
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
