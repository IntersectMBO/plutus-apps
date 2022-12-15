# TODO(std) DUP(very similar to plutus shell but unlikely that we can reuse it)

{ inputs, cell }:

let
  inherit (cell.library) pkgs;

  plutus-apps-project = cell.library.plutus-apps-project;

  plutus-apps-devshell = pkgs.haskell-nix.haskellLib.devshellFor plutus-apps-project.shell;
in

inputs.std.lib.dev.mkShell {

  name = "plutus-apps";

  imports = [ plutus-apps-devshell ];

  commands = [
    {
      package = cell.packages.fix-png-optimization;
      category = "general commands";
      help = "Fix all PNG files in-place";
    }
    {
      package = cell.packages.fix-purs-tidy;
      category = "general commands";
      help = "Format all purs files in-place";
    }
    {
      package = pkgs.shellcheck;
      category = "general commands";
      help = "Shell file checker";
    }
    {
      package = pkgs.editorconfig-checker;
      category = "general commands";
      help = "Checker for editorconfig conformance";
    }
    {
      package = cell.packages.fix-cabal-fmt;
      category = "haskell";
      help = "Format all cabal files in-place";
    }
    {
      package = cell.packages.fix-stylish-haskell;
      category = "haskell";
      help = "Run stylish-haskell on all haskell files in-place";
    }
    {
      package = cell.packages.sphinx-build-readthedocs-site;
      category = "docs";
      help = "Build the docs locally in doc/read-the-docs-site/_build";
    }
    {
      package = cell.packages.sphinx-autobuild-readthedocs-site;
      category = "docs";
      help = "Start the autobuild server on localhost:8000";
    }
    {
      package = cell.packages.serve-readthedocs-site;
      category = "docs";
      help = "nix build and serve the doc site on localhost:8002";
    }
    {
      package = cell.packages.check-the-flake;
      category = "nix";
      help = "For nix maintainers: build everything in the flake";
    }
    {
      package = cell.packages.cabal-install;
      name = "cabal";
      category = "haskell";
      help = "Haskell build tool";
    }
    {
      package = cell.packages.haskell-language-server;
      name = "haskell-language-server";
      category = "haskell";
      help = "Haskell Language Server";
    }
    {
      package = cell.packages.hlint;
      name = "hlint";
      category = "haskell";
      help = "Haskell linting tool";
    }
    {
      package = cell.packages.stylish-haskell;
      name = "stylish-haskell";
      category = "haskell";
      help = "Haskell code formatter";
    }
    {
      package = cell.packages.cabal-fmt;
      name = "cabal-fmt";
      category = "haskell";
      help = "Cabal file formatter";
    }
    {
      package = cell.packages.nixpkgs-fmt;
      category = "nix";
      help = "Nix code formatter";
    }
    # TODO(std) bring this in eventually
    # {
    #   # tullia input isn't de-systemized for some reason
    #   package = inputs.tullia.packages.${pkgs.system}.tullia;
    #   category = "nix";
    #   help = "Tools for working with CI tasks";
    # }
  ];

  packages = [
    cell.packages.hie-bios
    cell.packages.sphinx-toolchain

    pkgs.editorconfig-core-c
    pkgs.jq
    pkgs.pre-commit
    pkgs.yq
    pkgs.gnused
    pkgs.awscli2
    pkgs.bzip2

    pkgs.zlib
    pkgs.cacert
  ];

  devshell.startup."pre-commit-check".text = cell.packages.pre-commit-check.shellHook;

  env = [
    # This is no longer set automatically as of more recent `haskell.nix` revisions,
    # but is useful for users with LANG settings.
    {
      name = "LOCALE_ARCHIVE";
      value = pkgs.lib.optionalString
        (pkgs.stdenv.hostPlatform.libc == "glibc") "${pkgs.glibcLocales}/lib/locale/locale-archive";
    }
    # TODO(std) bring this in eventually
    # {
    #   name = "WEB_COMMON_SRC";
    #   value = webCommon.cleanSrc;
    # }
    # TODO(std) most likely we don't need this
    # {
    #   name = "PYTHONPATH";
    #   value = "";
    # }
  ];
}

# TODO(std) bring these in eventually
# # Feed cardano-wallet, cardano-cli & cardano-node to our shell. This is stable as it doesn't mix
# # dependencies with this code-base; the fetched binaries are the "standard" builds that people
# # test. This should be fast as it mostly fetches Hydra caches without building much.
# cardano-wallet = (import sources.flake-compat {
#   inherit pkgs;
#   src = builtins.fetchTree
#     {
#       type = "github";
#       owner = "input-output-hk";
#       repo = "cardano-wallet";
#       rev = "18a931648550246695c790578d4a55ee2f10463e";
#       narHash = "sha256-3Rnj/g3KLzOW5YSieqsUa9IF1Td22Eskk5KuVsOFgEQ=";
#     };
# }).defaultNix;
# cardano-node = import
#   (pkgs.fetchgit {
#     url = "https://github.com/input-output-hk/cardano-node";
#     # A standard release compatible with the cardano-wallet commit above is always preferred.
#     rev = "1.35.4";
#     sha256 = "1j01m2cp2vdcl26zx9xmipr551v3b2rz9kfn9ik8byfwj1z7652r";
#   })
#   { };

# TODO(std) we want all these in the shell eventually
# awscli2
# bzip2
# cacert
# editorconfig-core-c
# dateutils
# ghcid
# jq
# nixFlakesAlias
# nixpkgs-fmt
# cabal-fmt
# nodejs
# plantuml
# # See https://github.com/cachix/pre-commit-hooks.nix/issues/148 for why we need this
# pre-commit
# shellcheck
# sqlite-interactive
# stack
# wget
# yq
# z3
# zlib
# cabal-install
# cardano-node.cardano-cli
# cardano-node.cardano-node
# cardano-wallet.packages.${pkgs.system}.cardano-wallet
# cardano-repo-tool
# docs.build-and-serve-docs
# fixPngOptimization
# fix-purs-tidy
# fixCabalFmt
# fixStylishHaskell
# haskell-language-server
# haskell-language-server-wrapper
# hie-bios
# hlint
# pab-nami-demo.start-backend
# psa
# purescript-language-server
# purs-0_14_3
# purs-tidy
# spago
# spago2nix
# stylish-haskell
# updateClientDeps
