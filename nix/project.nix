{ repoRoot, inputs, pkgs, lib, system }:

let

  # Add symlinks to the DLLs used by executable code to the `bin` directory
  # of the components with we are going to run.
  # We should try to find a way to automate this will in haskellib.nix.
  symlinkDlls = ''
    ln -s ${pkgs.libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
    ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libgcc_s_seh-1.dll $out/bin/libgcc_s_seh-1.dll
    ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libstdc++-6.dll $out/bin/libstdc++-6.dll
    ln -s ${pkgs.windows.mcfgthreads}/bin/mcfgthread-12.dll $out/bin/mcfgthread-12.dll
  '';


  cabalProject = pkgs.haskell-nix.cabalProject' ({ pkgs, config, ... }:
    let
      isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
      isWindows = pkgs.stdenv.hostPlatform.isWindows;
      isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;
    in
    {
      name = "plutus-apps";

      compiler-nix-name = "ghc8107";

      src = ../.;

      sha256map = {
        "https://github.com/input-output-hk/cardano-addresses"."b7273a5d3c21f1a003595ebf1e1f79c28cd72513" = "129r5kyiw10n2021bkdvnr270aiiwyq58h472d151ph0r7wpslgp";
        "https://github.com/input-output-hk/cardano-ledger"."da3e9ae10cf9ef0b805a046c84745f06643583c2" = "sha256-3VUZKkLu1R43GUk9IwgsGQ55O0rnu8NrCkFX9gqA4ck=";
        "https://github.com/input-output-hk/cardano-wallet"."18a931648550246695c790578d4a55ee2f10463e" = "0i40hp1mdbljjcj4pn3n6zahblkb2jmpm8l4wnb36bya1pzf66fx";
        "https://github.com/input-output-hk/marconi"."7285a3bc1ae53bf672c7cc2359210c6c29fbce44" = "sha256-Z1ex1CqsIDzhzE8tbHtFvK+V+W3Fn1me2tHL/D+HhUE=";
      };

      shell.withHoogle = false;

      flake.variants.profiled.modules = [{
        enableProfiling = true;
        enableLibraryProfiling = true;
      }];

      inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP; };

      # Configuration settings needed for cabal configure to work when cross compiling
      # for windows. We can't use `modules` for these as `modules` are only applied
      # after cabal has been configured.
      cabalProjectLocal = lib.optionalString isWindows ''
        -- When cross compiling for windows we don't have a `ghc` package, so use
        -- the `plutus-ghc-stub` package instead.
        package plutus-tx-plugin
          flags: +use-ghc-stub

        -- Exlcude test that use `doctest`.  They will not work for windows
        -- cross compilation and `cabal` will not be able to make a plan.
        package prettyprinter-configurable
          tests: False
      '';

      modules = [{

        packages = {
          # ---------- Windows Packages ----------

          # Add dll symlinks to the compoents we want to run.
          plutus-core.components.tests.plutus-core-test.postInstall = lib.mkIf isWindows symlinkDlls;
          plutus-core.components.tests.plutus-ir-test.postInstall = lib.mkIf isWindows symlinkDlls;
          plutus-core.components.tests.untyped-plutus-core-test.postInstall = lib.mkIf isWindows symlinkDlls;
          plutus-ledger-api.components.tests.plutus-ledger-api-test.postInstall = lib.mkIf isWindows symlinkDlls;

          # These three tests try to use `diff` and the following could be used to make the
          # linux version of diff available.  Unfortunately the paths passed to it are windows style.
          # plutus-core.components.tests.plutus-core-test.build-tools = [ pkgs.buildPackages.diffutils ];
          # plutus-core.components.tests.plutus-ir-test.build-tools = [ pkgs.buildPackages.diffutils ];
          # plutus-core.components.tests.untyped-plutus-core-test.build-tools = [ pkgs.buildPackages.diffutils ];
          plutus-core.components.tests.plutus-core-test.buildable = lib.mkForce (!isWindows);
          plutus-core.components.tests.plutus-ir-test.buildable = lib.mkForce (!isWindows);
          plutus-core.components.tests.untyped-plutus-core-test.buildable = lib.mkForce (!isWindows);

          # ---------- Cross Compilation Packages ----------

          # Things that need plutus-tx-plugin
          freer-extras.package.buildable = !isCross;
          cardano-node-emulator.package.buildable = !isCross;
          cardano-node-socket-emulator.package.buildable = !isCross;
          cardano-streaming.package.buildable = !isCross;
          marconi-chain-index.package.buildable = !isCross;
          marconi-core.package.buildable = !isCross;
          pab-blockfrost.package.buildable = !isCross;
          plutus-benchmark.package.buildable = !isCross;
          plutus-chain-index.package.buildable = !isCross;
          plutus-chain-index-core.package.buildable = !isCross;
          plutus-contract.package.buildable = !isCross;
          plutus-contract-modelib.package.buildable = !isCross;
          plutus-contract-certification.package.buildable = !isCross;
          plutus-errors.package.buildable = !isCross;
          plutus-ledger.package.buildable = !isCross;
          plutus-pab.package.buildable = !isCross;
          plutus-pab-executables.package.buildable = !isCross;
          plutus-script-utils.package.buildable = !isCross;
          plutus-tx-constraints.package.buildable = !isCross;
          plutus-tx-plugin.package.buildable = !isCross;
          plutus-use-cases.package.buildable = !isCross;
          plutus-example.package.buildable = !isCross;

          # These need R
          plutus-core.components.benchmarks.cost-model-test.buildable = lib.mkForce (!isCross);
          plutus-core.components.benchmarks.update-cost-modelib.buildable = lib.mkForce (!isCross);

          # ---------- Darwin Packages ----------

          plutus-pab-executables.components.tests.plutus-pab-test-full-long-running.buildable = lib.mkForce (!isDarwin);

          # ---------- Common Packages ----------

          plutus-contract.flags.defer-plugin-errors = true;
          plutus-contract-modelib.flags.defer-plugin-errors = true;
          plutus-use-cases.flags.defer-plugin-errors = true;
          plutus-ledger.flags.defer-plugin-errors = true;
          plutus-script-utils.flags.defer-plugin-errors = true;
          plutus-example.flags.defer-plugin-errors = true;

          plutus-example.preCheck = "
            export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
            export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
            export CARDANO_SUBMIT_API=${config.hsPkgs.cardano-submit-api.components.exes.cardano-submit-api}/bin/cardano-submit-api${pkgs.stdenv.hostPlatform.extensions.executable}
            export CREATE_SCRIPT_CONTEXT=${config.hsPkgs.plutus-example.components.exes.create-script-context}/bin/create-script-context${pkgs.stdenv.hostPlatform.extensions.executable}
            export CARDANO_NODE_SRC=${../.}
          ";

          plutus-example.components.tests.plutus-example-test.build-tools =
            lib.mkForce (with pkgs.buildPackages; [ jq coreutils shellcheck lsof ]);

          # FIXME: Haddock mysteriously gives a spurious missing-home-modules warning
          plutus-tx-plugin.doHaddock = false;

          # Relies on cabal-doctest, just turn it off in the Nix build
          prettyprinter-configurable.components.tests.prettyprinter-configurable-doctest.buildable = lib.mkForce false;

          # Broken due to warnings, unclear why the setting that fixes this for the build doesn't work here.
          iohk-monitoring.doHaddock = false;
          cardano-wallet.doHaddock = false;

          # Werror everything. This is a pain, see https://github.com/input-output-hk/haskellib.nix/issues/519
          cardano-streaming.ghcOptions = [ "-Werror" ];
          marconi-chain-index.ghcOptions = [ "-Werror" ];
          marconi-core.ghcOptions = [ "-Werror" ];
          pab-blockfrost.ghcOptions = [ "-Werror" ];
          plutus-chain-index.ghcOptions = [ "-Werror" ];
          plutus-chain-index-core.ghcOptions = [ "-Werror" ];
          plutus-contract.ghcOptions = [ "-Werror" ];
          plutus-contract-modelib.ghcOptions = [ "-Werror" ];
          plutus-doc.ghcOptions = [ "-Werror" ];
          plutus-example.ghcOptions = [ "-Werror" ];
          plutus-ledger.ghcOptions = [ "-Werror" ];
          plutus-pab.ghcOptions = [ "-Werror" ];
          plutus-pab-executables.ghcOptions = [ "-Werror" ];
          plutus-script-utils.ghcOptions = [ "-Werror" ];
          plutus-tx-constraints.ghcOptions = [ "-Werror" ];
          plutus-use-cases.ghcOptions = [ "-Werror" ];
        };
      }];
    });


  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
    readTheDocs = {
      enable = true;
      siteFolder = "doc";
    };
    combinedHaddock = {
      enable = true;
      packages = [
        "plutus-core"
        "plutus-tx"
        "plutus-tx-plugin"
        "plutus-ledger-api"
        "quickcheck-contractmodel"
      ];
      prologue = ''
        = Combined documentation for all the public Plutus libraries

        == Handy module entrypoints

          * "Plutus.Contract": Writing Plutus apps (off-chain code).
          * "Ledger.Typed.Scripts": A type-safe interface for spending and
            producing script outputs. Built on "PlutusTx".
          * "Plutus.Trace.Emulator": Testing Plutus contracts in the emulator.
          * "Cardano.Node.Emulator.MTL": Test your transactions on an emulated node.
      '';
    };
  };

in

project
