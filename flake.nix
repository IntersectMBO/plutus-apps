# The flake.nix is the entrypoint of all nix code.
#
# This repository uses the standard tool https://github.com/divnix/std.
# Familiarity with std is required to be able to contribute effectively.
# While official documentation for std can be found in its GitHub, this flake
# has been thoroughly commented so as to quickstart new maintainers.
# This flake can also be used as a template for new std-based projects.
# Further documentation can be found in nix/README.md
#
# You may want to refer to the standard glossary as you go along:
# https://divnix.github.io/std/glossary.html
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
      url = "github:input-output-hk/haskell.nix/7075077d46e684d50e1b00759bb4590426c99c70";
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
  };

  # The flake outputs are managed by std.
  outputs = inputs:

    # The growOn function takes care of producing the flake outputs.
    inputs.std.growOn
      {
        # Boilerplate
        inherit inputs;

        # All nix files will reside inside this folder, no exception.
        # Each subfolder of cellsFrom is a "cell".
        # Cell names are arbitrary; a cell name is its folder name.
        # Cells are for highest-level organization and grouping of nix code.
        #
        # In this repository we have two cells:
        #   automation
        #     Hydra jobsets and GHA tasks
        #   plutus
        #     Devshell, tooling and packages for plutus and its documentation
        cellsFrom = ./__std__;

        # Each cell contains "cell blocks".
        # Block names are arbitrary.
        # Each block can be thought of as providing a "feature" to its cell.
        # Cell blocks have types.
        # Each cell block must be either:
        #   A nix file named after the cell block
        #   A directory named after the cell block and containing a default.nix
        # Not all cells have the same cell blocks.
        # All cell blocks belong in a cell.
        #
        # In this repository we have six cell blocks, listed below with their type:
        #   devshells :: devshells
        #     Development shells available via nix develop
        #   packages :: installables
        #     Derivations available via nix build
        #   devcontainer :: installables
        #     Doker image for creating a VS Core development environment
        #   library :: functions
        #     Everything that is not a derivation goes here
        #     Includes functions, attrsets and simple literal values shared across cells
        #     These are not exposed to the flake
        #   hydra-jobs :: installables
        #     Jobsets for our Hydra CI
        #
        # std provides a TUI to interact with the cell blocks.
        # Available interactions are determined by the cell block's type.
        # Because this repository does not yet use the TUI, the type is mostly irrelevant.
        cellBlocks = [
          (inputs.std.devshells "devshells")
          (inputs.std.installables "packages")
          (inputs.std.installables "devcontainer")
          (inputs.std.functions "library")
          (inputs.std.installables "hydra-jobs")
        ];
      }

      # The growOn function will then accept an arbitrary number of "soil" attrs.
      # This is where we translate cells and cell blocks into a standard nix flake
      # outputs attrs.
      #
      # This is where we also decide which cells and which cell blocks will
      # make it into the flake. To exclude stuff from the flake, we simply
      # do not "harvest" it.
      #
      # The attrs will be recursively merged in the order in which they appear.
      {
        # Here we say that we want the "devshells" cell block of the plutus-apps cell
        # (which contains a number of shell-able derivations) to be exposed
        # by the flake and accessible via nix develop.
        devShells = inputs.std.harvest inputs.self [ "plutus-apps" "devshells" ];
        packages = inputs.std.harvest inputs.self [ "plutus-apps" "packages" ];
      }
      {
        # Here we say that we want the "devcontainer" cell block of the plutus-apps cell
        # (which contains a number of buildable derivations) to be exposed
        # by the flake and accessible via nix build (or nix run).
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
