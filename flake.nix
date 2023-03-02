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
      url = "github:NixOS/nixpkgs";
    };
    std = {
      url = "github:divnix/std";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat";
      flake = false;
    };
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs = {
        hackage.follows = "hackage-nix";
        nixpkgs.follows = "nixpkgs";
      };
    };
    hackage-nix = {
      url = "github:input-output-hk/hackage.nix";
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
      # TODO Bump to 1.9.0.0 once plutus-apps hits GHC 9.2
      url = "github:haskell/haskell-language-server?ref=1.8.0.0";
      flake = false;
    };
    plutus-core = {
      url = "github:input-output-hk/plutus";
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
        #   plutus-apps
        #     devcontainer, devshells, library and packages for plutus-apps and its documentation
        cellsFrom = ./__std__/cells;

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
        #     Docker image for creating a VS Code development environment
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
