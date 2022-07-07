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

  inherit (plutus-apps) web-ghc;

  inherit (haskell.packages.plutus-pab-executables.components.exes)
    plutus-pab-examples
    plutus-uniswap;

  webCommon = pkgs.callPackage sources.web-common { inherit (plutus-apps.lib) gitignore-nix; };

  plutus-playground = pkgs.recurseIntoAttrs rec {
    haddock = plutus-apps.plutus-haddock-combined;

    inherit (pkgs.callPackage ./plutus-playground-client {
      inherit (plutus-apps) purs-tidy;
      inherit (plutus-apps.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit haskell webCommon;
    }) client server start-backend generate-purescript generated-purescript;
  };

  # TODO: Fails for now because of webpack can't include `nami-wallet` lib in it's bundle.
  # To reproduce the error, run `npm run build:webpack:prod` in `plutus-pab-executables/demo/pab-nami/client`
  pab-nami-demo = pkgs.recurseIntoAttrs rec {
    inherit (pkgs.callPackage ./plutus-pab-executables/demo/pab-nami/client {
      inherit (plutus-apps) purs-tidy;
      inherit pkgs haskell webCommon;
      inherit (plutus-apps.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
    }) client pab-setup-invoker pab-nami-demo-invoker pab-nami-demo-generator generate-purescript generated-purescript start-backend;
  };

  plutus-use-cases = pkgs.recurseIntoAttrs (pkgs.callPackage ./plutus-use-cases {
    inherit haskell;
  });

  pab-cli = plutus-apps.haskell.packages.plutus-pab-executables.components.exes.pab-cli;

  plutus-chain-index = plutus-apps.haskell.packages.plutus-chain-index.components.exes.plutus-chain-index;

  marconi = plutus-apps.haskell.packages.plutus-chain-index.components.exes.marconi;

  tests = import ./nix/tests/default.nix {
    inherit pkgs docs;
    inherit (plutus-apps.lib) gitignore-nix;
    inherit (plutus-apps) fixStylishHaskell fix-purs-tidy fixPngOptimization fixCabalFmt;
    inherit plutus-playground web-ghc;
    src = ./.;
    play-generated = plutus-playground.generated-purescript;
    nami-generated = pab-nami-demo.generated-purescript;
  };

  docs = import ./nix/docs.nix { inherit pkgs plutus-apps; };

  # This builds a vscode devcontainer that can be used with the plutus-starter project (or probably the plutus project itself).
  devcontainer = import ./nix/devcontainer/plutus-devcontainer.nix { inherit pkgs plutus-apps; };

  build-and-push-devcontainer-script = import ./nix/devcontainer/deploy/default.nix { inherit pkgs plutus-apps; };

  # Packages needed for the bitte deployment
  bitte-packages = import ./bitte { inherit plutus-playground docs pkgs web-ghc; };
}
