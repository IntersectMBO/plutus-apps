{ inputs, cell }:

# Whether to set the `defer-plugin-errors` flag on those packages that need
# it. If set to true, we will also build the haddocks for those packages.
{ deferPluginErrors ? false

, enableHaskellProfiling ? false
}:

let
  project = cell.library.pkgs.haskell-nix.cabalProject' ({ pkgs, config, lib, ... }: {

    compiler-nix-name = cell.library.ghc-compiler-nix-name;

    src = cell.library.pkgs.haskell-nix.haskellLib.cleanSourceWith {
      src = inputs.self.outPath;
      name = "plutus-apps";
    };

    shell.withHoogle = false;

    # TODO(std) fix this when nix-shell goes away
    sha256map = import (inputs.self + /nix/pkgs/haskell/sha256map.nix);

    inputMap = {
      "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
    };

    # Configuration settings needed for cabal configure to work when cross compiling
    # for windows. We can't use `modules` for these as `modules` are only applied
    # after cabal has been configured.
    cabalProjectLocal = lib.optionalString pkgs.stdenv.hostPlatform.isWindows ''
      -- When cross compiling for windows we don't have a `ghc` package, so use
      -- the `plutus-ghc-stub` package instead.
      package plutus-tx-plugin
        flags: +use-ghc-stub

      -- Exlcude test that use `doctest`.  They will not work for windows
      -- cross compilation and `cabal` will not be able to make a plan.
      package prettyprinter-configurable
        tests: False
    '';
    modules =
      let
        inherit (config) src;
      in
      [
        ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
          packages = {
            # Things that need plutus-tx-plugin
            freer-extras.package.buildable = false;
            cardano-node-emulator.package.buildable = false;
            cardano-streaming.package.buildable = false;
            marconi-chain-index.package.buildable = false;
            marconi-core.package.buildable = false;
            marconi-sidechain.package.buildable = false;
            pab-blockfrost.package.buildable = false;
            plutus-benchmark.package.buildable = false;
            plutus-chain-index.package.buildable = false;
            plutus-chain-index-core.package.buildable = false;
            plutus-contract.package.buildable = false;
            plutus-contract-certification.package.buildable = false;
            plutus-e2e-tests.package.buildable = false;
            plutus-errors.package.buildable = false;
            plutus-ledger.package.buildable = false;
            plutus-pab.package.buildable = false;
            plutus-pab-executables.package.buildable = false;
            plutus-script-utils.package.buildable = false;
            plutus-tx-constraints.package.buildable = false;
            plutus-tx-plugin.package.buildable = false;
            plutus-use-cases.package.buildable = false;
            plutus-example.package.buildable = false;
            # These need R
            plutus-core.components.benchmarks.cost-model-test.buildable = lib.mkForce false;
            plutus-core.components.benchmarks.update-cost-model.buildable = lib.mkForce false;
          };
        })
        ({ pkgs, ... }:
          let
            # Add symlinks to the DLLs used by executable code to the `bin` directory
            # of the components with we are going to run.
            # We should try to find a way to automate this will in haskell.nix.
            symlinkDlls = ''
              ln -s ${pkgs.libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
              ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libgcc_s_seh-1.dll $out/bin/libgcc_s_seh-1.dll
              ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libstdc++-6.dll $out/bin/libstdc++-6.dll
              ln -s ${pkgs.windows.mcfgthreads}/bin/mcfgthread-12.dll $out/bin/mcfgthread-12.dll
            '';
          in
          lib.mkIf (pkgs.stdenv.hostPlatform.isWindows) {
            packages = {
              # Add dll symlinks to the compoents we want to run.
              plutus-core.components.tests.plutus-core-test.postInstall = symlinkDlls;
              plutus-core.components.tests.plutus-ir-test.postInstall = symlinkDlls;
              plutus-core.components.tests.untyped-plutus-core-test.postInstall = symlinkDlls;
              plutus-ledger-api.components.tests.plutus-ledger-api-test.postInstall = symlinkDlls;

              # These three tests try to use `diff` and the following could be used to make the
              # linux version of diff available.  Unfortunately the paths passed to it are windows style.
              # plutus-core.components.tests.plutus-core-test.build-tools = [ pkgs.buildPackages.diffutils ];
              # plutus-core.components.tests.plutus-ir-test.build-tools = [ pkgs.buildPackages.diffutils ];
              # plutus-core.components.tests.untyped-plutus-core-test.build-tools = [ pkgs.buildPackages.diffutils ];
              plutus-core.components.tests.plutus-core-test.buildable = lib.mkForce false;
              plutus-core.components.tests.plutus-ir-test.buildable = lib.mkForce false;
              plutus-core.components.tests.untyped-plutus-core-test.buildable = lib.mkForce false;
            };
          }
        )
        (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
          packages = {
            plutus-pab-executables.components.tests.plutus-pab-test-full-long-running.buildable = lib.mkForce false;
          };
        })
        ({ pkgs, config, ... }: {
          packages = {
            marconi-core.doHaddock = deferPluginErrors;
            marconi-core.flags.defer-plugin-errors = deferPluginErrors;

            marconi-chain-index.doHaddock = deferPluginErrors;
            marconi-chain-index.flags.defer-plugin-errors = deferPluginErrors;

            # The lines `export CARDANO_NODE=...` and `export CARDANO_CLI=...`
            # is necessary to prevent the error
            # `../dist-newstyle/cache/plan.json: openBinaryFile: does not exist (No such file or directory)`.
            # See https://github.com/input-output-hk/cardano-node/issues/4194.
            #
            # The line 'export CARDANO_NODE_SRC=...' is used to specify the
            # root folder used to fetch the `configuration.yaml` file (in
            # plutus-apps, it's currently in the
            # `configuration/defaults/byron-mainnet` directory.
            # Else, we'll get the error
            # `/nix/store/ls0ky8x6zi3fkxrv7n4vs4x9czcqh1pb-plutus-apps/marconi/test/configuration.yaml: openFile: does not exist (No such file or directory)`
            marconi-chain-index.preCheck = "
              export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
              export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
              export CARDANO_NODE_SRC=${src}
            ";

            marconi-sidechain.doHaddock = deferPluginErrors;
            marconi-sidechain.flags.defer-plugin-errors = deferPluginErrors;

            plutus-contract.doHaddock = deferPluginErrors;
            plutus-contract.flags.defer-plugin-errors = deferPluginErrors;

            plutus-e2e-tests.doHaddock = deferPluginErrors;
            plutus-e2e-tests.flags.defer-plugin-errors = deferPluginErrors;
            plutus-e2e-tests.preCheck = "
              export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
              export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
              export CARDANO_NODE_SRC=${src}
            ";
            plutus-e2e-tests.components.tests.plutus-e2e-tests-test.build-tools =
              lib.mkForce (with pkgs.buildPackages; [ jq coreutils shellcheck lsof ]);

            plutus-use-cases.doHaddock = deferPluginErrors;
            plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

            plutus-ledger.doHaddock = deferPluginErrors;
            plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;

            plutus-script-utils.doHaddock = deferPluginErrors;
            plutus-script-utils.flags.defer-plugin-errors = deferPluginErrors;

            plutus-example.doHaddock = deferPluginErrors;
            plutus-example.flags.defer-plugin-errors = deferPluginErrors;
            plutus-example.preCheck = "
              export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
              export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
              export CARDANO_SUBMIT_API=${config.hsPkgs.cardano-submit-api.components.exes.cardano-submit-api}/bin/cardano-submit-api${pkgs.stdenv.hostPlatform.extensions.executable}
              export CREATE_SCRIPT_CONTEXT=${config.hsPkgs.plutus-example.components.exes.create-script-context}/bin/create-script-context${pkgs.stdenv.hostPlatform.extensions.executable}
              export CARDANO_NODE_SRC=${src}
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

            # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
            cardano-streaming.ghcOptions = [ "-Werror" ];
            marconi-chain-index.ghcOptions = [ "-Werror" ];
            marconi-core.ghcOptions = [ "-Werror" ];
            marconi-sidechain.ghcOptions = [ "-Werror" ];
            pab-blockfrost.ghcOptions = [ "-Werror" ];
            plutus-chain-index.ghcOptions = [ "-Werror" ];
            plutus-chain-index-core.ghcOptions = [ "-Werror" ];
            plutus-contract.ghcOptions = [ "-Werror" ];
            plutus-doc.ghcOptions = [ "-Werror" ];
            plutus-example.ghcOptions = [ "-Werror" ];
            plutus-ledger.ghcOptions = [ "-Werror" ];
            plutus-pab.ghcOptions = [ "-Werror" ];
            plutus-pab-executables.ghcOptions = [ "-Werror" ];
            plutus-script-utils.ghcOptions = [ "-Werror" ];
            plutus-tx-constraints.ghcOptions = [ "-Werror" ];
            plutus-use-cases.ghcOptions = [ "-Werror" ];

            # Honestly not sure why we need this, it has a mysterious unused dependency on "m"
            # This will go away when we upgrade nixpkgs and things use ieee754 anyway.
            ieee.components.library.libs = lib.mkForce [ ];

            # See https://github.com/input-output-hk/iohk-nix/pull/488
            cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
            cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
          };
        })
      ] ++ lib.optional enableHaskellProfiling {
        enableLibraryProfiling = true;
        enableProfiling = true;
      };
  });

in
project
