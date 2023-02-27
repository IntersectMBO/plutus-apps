########################################################################
# default.nix -- The top-level nix build file for Plutus apps.
#
# This file defines various attributes that are used for building and
# developing Plutus apps.
#
########################################################################
{ system ? builtins.currentSystem
, crossSystem ? null
, config ? { }
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { inherit system; } // sourcesOverride
, haskellNix ? import sources.haskell-nix {
    pkgs = import sources.nixpkgs { inherit system; };
    sourcesOverride = {
      hackage = sources.hackage-nix;
      stackage = sources.stackage-nix;
    };
  }
, packages ? import ./nix { inherit system sources crossSystem config sourcesOverride haskellNix enableHaskellProfiling; }
  # Whether to build our Haskell packages (and their dependencies) with profiling enabled.
, enableHaskellProfiling ? false
}:
let
  inherit (packages) pkgs plutus-apps;
  inherit (plutus-apps) haskell;
in
rec {
  inherit pkgs plutus-apps;

  inherit (haskell.packages.plutus-pab-executables.components.exes)
    plutus-pab-examples
    plutus-uniswap;

  plutus-use-cases = pkgs.callPackage ./plutus-use-cases {
    inherit haskell;
  };

  pab-cli = plutus-apps.haskell.packages.plutus-pab-executables.components.exes.pab-cli;

  plutus-chain-index = plutus-apps.haskell.packages.plutus-chain-index.components.exes.plutus-chain-index;

  marconi-chain-index = plutus-apps.haskell.packages.marconi-chain-index.components.exes.marconi-chain-index;

  marconi-sidechain = plutus-apps.haskell.packages.marconi-sidechain.components.exes.marconi-sidechain;

  create-script-context = plutus-apps.haskell.packages.plutus-example.components.exes.create-script-context;

  tests = import ./nix/tests/default.nix {
    inherit pkgs docs;
    inherit (plutus-apps.lib) gitignore-nix;
    inherit (plutus-apps) fixStylishHaskell fixPngOptimization fixCabalFmt;
    src = ./.;
  };

  docs = import ./nix/docs.nix { inherit pkgs plutus-apps; };

  # This builds a vscode devcontainer that can be used with the plutus-starter project (or probably the plutus project itself).
  devcontainer = import ./nix/devcontainer/plutus-devcontainer.nix { inherit pkgs plutus-apps; };

  build-and-push-devcontainer-script = import ./nix/devcontainer/deploy/default.nix { inherit pkgs plutus-apps; };

  # Packages needed for the bitte deployment
  bitte-packages = import ./bitte { inherit docs pkgs; };
}
