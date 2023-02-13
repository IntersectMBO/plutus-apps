{ inputs, cell }:

let

  inherit (inputs.cells.plutus-apps.library) plutus-apps-project pkgs;
  inherit (pkgs.stdenv) system;

  make-haskell-jobs = project:
    let
      packages = pkgs.haskell-nix.haskellLib.selectProjectPackages project.hsPkgs;
    in
    {
      exes = pkgs.haskell-nix.haskellLib.collectComponents' "exes" packages;
      tests = pkgs.haskell-nix.haskellLib.collectComponents' "tests" packages;
      benchmarks = pkgs.haskell-nix.haskellLib.collectComponents' "benchmarks" packages;
      libraries = pkgs.haskell-nix.haskellLib.collectComponents' "library" packages;
      checks = pkgs.haskell-nix.haskellLib.collectChecks' packages;
      roots = project.roots;
      plan-nix = project.plan-nix;
    };

  native-plutus-apps-jobs = make-haskell-jobs plutus-apps-project;

  windows-plutus-apps-jobs = make-haskell-jobs plutus-apps-project.projectCross.mingwW64;

  other-jobs = inputs.cells.plutus-apps.devshells // inputs.cells.plutus-apps.packages;

  jobs =
    { native = native-plutus-apps-jobs; } //
    # Only cross-compile to windows from linux
    pkgs.lib.optionalAttrs (system == "x86_64-linux") { mingwW64 = windows-plutus-apps-jobs; } //
    # Devcontainer is only available on linux
    pkgs.lib.optionalAttrs (system == "x86_64-linux") inputs.cells.plutus-apps.devcontainer //
    other-jobs;

  # Hydra doesn't like these attributes hanging around in "jobsets": it thinks they're jobs!
  filtered-jobs = pkgs.lib.filterAttrsRecursive (n: _: n != "recurseForDerivations") jobs;

  required-job = pkgs.releaseTools.aggregate {
    name = "required-plutus";
    meta.description = "All jobs required to pass CI";
    constituents = pkgs.lib.collect pkgs.lib.isDerivation filtered-jobs;
  };

  final-jobset =
    if pkgs.system == "x86_64-linux" || pkgs.system == "x86_64-darwin" then
      filtered-jobs // { required = required-job; }
    else { };

in

final-jobset
