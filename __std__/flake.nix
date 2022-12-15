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
      url = "github:haskell/haskell-language-server?ref=1.5.1";
      flake = false;
    };
    # TODO(std) unique to plutus-apps vvvvv
    cardano-repo-tool = {
      url = "github:input-output-hk/cardano-repo-tool";
      flake = false;
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
    npmlock2nix = {
      url = "github:tweag/npmlock2nix";
      flake = false;
    };
    plutus-core = {
      url = "github:input-output-hk/plutus";
      flake = false;
    };
    spago2nix = {
      url = "github:justinwoo/spago2nix";
      flake = false;
    };
    web-common = {
      url = "github:input-output-hk/purescript-web-common";
      flake = false;
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
          (inputs.std.installables "ciJobs")
        ];
      }
      {
        devShells = inputs.std.harvest inputs.self [ "plutus-apps" "devshells" ];
        packages = inputs.std.harvest inputs.self [ "plutus-apps" "packages" ];
      }
      {
        hydraJobs = inputs.std.harvest inputs.self [ "automation" "ciJobs" ];
      };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
