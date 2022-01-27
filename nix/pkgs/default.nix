{ pkgs
, checkMaterialization
, system ? builtins.currentSystem
, config ? { }
, sources
, enableHaskellProfiling
, topLevelPkgs
, ghcjsPluginPkgs ? null
, cabalProjectLocal ? null
}:
let
  inherit (pkgs) stdenv;

  gitignore-nix = pkgs.callPackage sources.gitignore-nix { };

  # { index-state, compiler-nix-name, project, projectPackages, packages, extraPackages }
  haskell = pkgs.callPackage ./haskell {
    inherit gitignore-nix sources;
    inherit checkMaterialization enableHaskellProfiling;

    # This ensures that the utility scripts produced in here will run on the current system, not
    # the build system, so we can run e.g. the darwin ones on linux
    inherit (pkgs.evalPackages) writeShellScript;

    inherit topLevelPkgs ghcjsPluginPkgs cabalProjectLocal;
  };

  #
  # additional haskell packages from ./nix/pkgs/haskell-extra
  #
  exeFromExtras = x: haskell.extraPackages."${x}".components.exes."${x}";
  cabal-install = haskell.extraPackages.cabal-install.components.exes.cabal;
  cardano-repo-tool = exeFromExtras "cardano-repo-tool";
  stylish-haskell = exeFromExtras "stylish-haskell";
  hlint = exeFromExtras "hlint";
  haskell-language-server = exeFromExtras "haskell-language-server";
  haskell-language-server-wrapper = pkgs.writeShellScriptBin "haskell-language-server-wrapper" ''${haskell-language-server}/bin/haskell-language-server "$@"'';
  hie-bios = exeFromExtras "hie-bios";

  #
  # dev convenience scripts
  #
  fix-purs-tidy = pkgs.callPackage ./fix-purs-tidy { inherit purs-tidy; };
  fixStylishHaskell = pkgs.callPackage ./fix-stylish-haskell { inherit stylish-haskell; };
  fixPngOptimization = pkgs.callPackage ./fix-png-optimization { };
  updateMaterialized = pkgs.writeShellScriptBin "updateMaterialized" ''
    # This runs the 'updateMaterialize' script in all platform combinations we care about.
    # See the comment in ./haskell/haskell.nix

    # Update the linux files (will do for all unixes atm).
    $(nix-build default.nix -A plutus-apps.haskell.project.plan-nix.passthru.updateMaterialized --argstr system x86_64-linux)
    $(nix-build default.nix -A plutus-apps.haskell.project.plan-nix.passthru.updateMaterialized --argstr system x86_64-darwin)
    $(nix-build default.nix -A plutus-apps.haskell.project.projectCross.mingwW64.plan-nix.passthru.updateMaterialized --argstr system x86_64-linux)
    $(nix-build default.nix -A plutus-apps.haskell.project.plan-nix.passthru.updateMaterialized --arg crossSystem '(import ./. {}).pkgs.lib.systems.examples.mingwW64' --argstr system x86_64-linux)
    $(nix-build default.nix -A plutus-apps.haskell.project.plan-nix.passthru.updateMaterialized --arg crossSystem '(import ./. {}).pkgs.lib.systems.examples.ghcjs' --argstr system x86_64-linux)
    $(nix-build default.nix -A plutus-apps.haskell.project.buildProject.plan-nix.passthru.updateMaterialized --arg crossSystem '(import ./. {}).pkgs.lib.systems.examples.ghcjs' --argstr system x86_64-linux)

    # This updates the sha files for the extra packages
    $(nix-build default.nix -A plutus-apps.haskell.extraPackages.updateAllShaFiles --argstr system x86_64-linux)
    $(nix-build default.nix -A plutus-apps.haskell.extraPackages.updateAllShaFiles --argstr system x86_64-darwin)
  '';
  updateClientDeps = pkgs.callPackage ./update-client-deps {
    inherit purs spago spago2nix;
  };

  #
  # sphinx python packages
  #
  sphinx-markdown-tables = pkgs.python3Packages.callPackage ./sphinx-markdown-tables { };
  sphinxemoji = pkgs.python3Packages.callPackage ./sphinxemoji { };

  # By default pre-commit-hooks.nix uses its own pinned version of nixpkgs. In order to
  # to get it to use our version we have to (somewhat awkwardly) use `nix/default.nix`
  # to which both `nixpkgs` and `system` can be passed.
  nix-pre-commit-hooks = (pkgs.callPackage (sources.pre-commit-hooks-nix + "/nix/default.nix") {
    inherit system;
    inherit (sources) nixpkgs;
  });

  # easy-purescript-nix has some kind of wacky internal IFD
  # usage that breaks the logic that makes source fetchers
  # use native dependencies. This isn't easy to fix, since
  # the only places that need to use native dependencies
  # are deep inside, and we don't want to build the whole
  # thing native. Fortunately, we only want to build the
  # client on Linux, so that's okay. However, it does
  # mean that e.g. we can't build the client dep updating
  # script on Darwin.
  easyPS = pkgs.callPackage (sources.easy-purescript-nix) { };

  # We pull out some packages from easyPS that are a pain to get otherwise.
  # This does mean we can't as easily control the version we get, though.
  inherit (easyPS) purs-tidy purs spago purescript-language-server psa spago2nix;

  # sphinx haddock support
  sphinxcontrib-haddock = pkgs.callPackage (sources.sphinxcontrib-haddock) { pythonPackages = pkgs.python3Packages; };

  # ghc web service
  web-ghc = pkgs.callPackage ./web-ghc { inherit haskell; };

  # combined haddock documentation for all public plutus libraries
  plutus-haddock-combined =
    let
      haddock-combine = pkgs.callPackage (sources.plutus-core + "/nix/lib/haddock-combine.nix") {
        ghc = haskell.projectAllHaddock.pkg-set.config.ghc.package;
        inherit (sphinxcontrib-haddock) sphinxcontrib-haddock;
      };
    in
    pkgs.callPackage (sources.plutus-core + "/nix/pkgs/plutus-haddock-combined") {
      inherit haskell haddock-combine;
      inherit (pkgs) haskell-nix;
    };

  # Collect everything to be exported under `plutus-apps.lib`: builders/functions/utils
  lib = rec {
    inherit gitignore-nix;
    haddock-combine = pkgs.callPackage (sources.plutus-core + "/nix/lib/haddock-combine.nix") { inherit sphinxcontrib-haddock; };
    filterNpm = pkgs.callPackage ../lib/filter-npm.nix { };
    npmlock2nix = pkgs.callPackage sources.npmlock2nix { };
    buildPursPackage = pkgs.callPackage ../lib/purescript.nix { inherit easyPS; inherit (pkgs) nodejs; };
    buildNodeModules = pkgs.callPackage ../lib/node_modules.nix ({
      inherit npmlock2nix;
    } // pkgs.lib.optionalAttrs (stdenv.isDarwin) {
      CoreServices = pkgs.darwin.apple_sdk.frameworks.CoreServices;
      xcodebuild = pkgs.xcodebuild;
    });
  };

in
{
  inherit sphinx-markdown-tables sphinxemoji sphinxcontrib-haddock;
  inherit nix-pre-commit-hooks;
  inherit haskell cabal-install cardano-repo-tool stylish-haskell hlint haskell-language-server haskell-language-server-wrapper hie-bios;
  inherit purs-tidy purs spago spago2nix purescript-language-server psa;
  inherit fix-purs-tidy fixStylishHaskell fixPngOptimization updateMaterialized updateClientDeps;
  inherit web-ghc;
  inherit easyPS plutus-haddock-combined;
  inherit lib;

  cabal-plan = pkgs.haskell-nix.tool "ghc8105" "cabal-plan" {
    index-state = pkgs.haskell-nix.internalHackageIndexState;
    version = "0.7.2.0";
  };
}
