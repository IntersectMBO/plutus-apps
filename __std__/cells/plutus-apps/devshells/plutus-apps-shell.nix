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
      package = cell.packages.scriv;
      category = "general commands";
      help = "Manage changelogs";
    }
    {
      package = cell.packages.fix-png-optimization;
      category = "general commands";
      help = "Fix all PNG files in-place";
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
      package = cell.packages.haskell-language-server-wrapper;
      name = "haskell-language-server-wrapper";
      category = "haskell";
      help = "Haskell Language Server Wrapper";
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
  ];

  packages = [
    cell.packages.hie-bios
    cell.packages.sphinx-toolchain
    cell.packages.hlint
    cell.packages.stylish-haskell
    cell.packages.haskell-language-server-wrapper
    cell.packages.cabal-install
    cell.packages.cabal-fmt
    cell.packages.plutus-use-cases
    cell.packages.cardano-wallet

    cell.library.cardano-node.cardano-node
    cell.library.cardano-node.cardano-cli

    pkgs.plantuml
    pkgs.shellcheck
    pkgs.sqlite-interactive
    pkgs.stack
    pkgs.wget
    pkgs.yq
    pkgs.jq
    pkgs.z3
    pkgs.ghcid
    pkgs.editorconfig-core-c
    pkgs.jq
    pkgs.pre-commit
    pkgs.yq
    pkgs.gnused
    pkgs.awscli2
    pkgs.bzip2
    pkgs.zlib
    pkgs.cacert
    pkgs.dateutils
    pkgs.act
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

    # This is used by doc/conf.py to link plutus-core read-the-docs pages
    {
      name = "PLUTUS_CORE_OBJECTS_INV";
      value = cell.library.plutus-core-objects-inv;
    }
  ];
}
