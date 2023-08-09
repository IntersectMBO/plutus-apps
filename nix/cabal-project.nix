{ inputs', pkgs, config, l, meta, ... }:

let

  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  isWindows = pkgs.stdenv.hostPlatform.isWindows;
  isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;


  sha256map = {
    "https://github.com/input-output-hk/cardano-addresses"."b7273a5d3c21f1a003595ebf1e1f79c28cd72513" = "129r5kyiw10n2021bkdvnr270aiiwyq58h472d151ph0r7wpslgp";
    "https://github.com/input-output-hk/cardano-ledger"."da3e9ae10cf9ef0b805a046c84745f06643583c2" = "sha256-3VUZKkLu1R43GUk9IwgsGQ55O0rnu8NrCkFX9gqA4ck=";
    "https://github.com/input-output-hk/cardano-wallet"."18a931648550246695c790578d4a55ee2f10463e" = "0i40hp1mdbljjcj4pn3n6zahblkb2jmpm8l4wnb36bya1pzf66fx";
  };


  # Configuration settings needed for cabal configure to work when cross compiling
  # for windows. We can't use `modules` for these as `modules` are only applied
  # after cabal has been configured.
  cabalProjectLocal = l.optionalString isWindows ''
    -- When cross compiling for windows we don't have a `ghc` package, so use
    -- the `plutus-ghc-stub` package instead.
    package plutus-tx-plugin
      flags: +use-ghc-stub

    -- Exlcude test that use `doctest`.  They will not work for windows
    -- cross compilation and `cabal` will not be able to make a plan.
    package prettyprinter-configurable
      tests: False
  '';


  # Add symlinks to the DLLs used by executable code to the `bin` directory
  # of the components with we are going to run.
  # We should try to find a way to automate this will in haskell.nix.
  symlinkDlls = ''
    ln -s ${pkgs.libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
    ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libgcc_s_seh-1.dll $out/bin/libgcc_s_seh-1.dll
    ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libstdc++-6.dll $out/bin/libstdc++-6.dll
    ln -s ${pkgs.windows.mcfgthreads}/bin/mcfgthread-12.dll $out/bin/mcfgthread-12.dll
  '';


  packages = {

    # ---------- Windows Packages ----------

    # Add dll symlinks to the compoents we want to run.
    plutus-core.components.tests.plutus-core-test.postInstall = l.mkIf isWindows symlinkDlls;
    plutus-core.components.tests.plutus-ir-test.postInstall = l.mkIf isWindows symlinkDlls;
    plutus-core.components.tests.untyped-plutus-core-test.postInstall = l.mkIf isWindows symlinkDlls;
    plutus-ledger-api.components.tests.plutus-ledger-api-test.postInstall = l.mkIf isWindows symlinkDlls;

    # These three tests try to use `diff` and the following could be used to make the
    # linux version of diff available.  Unfortunately the paths passed to it are windows style.
    # plutus-core.components.tests.plutus-core-test.build-tools = [ pkgs.buildPackages.diffutils ];
    # plutus-core.components.tests.plutus-ir-test.build-tools = [ pkgs.buildPackages.diffutils ];
    # plutus-core.components.tests.untyped-plutus-core-test.build-tools = [ pkgs.buildPackages.diffutils ];
    plutus-core.components.tests.plutus-core-test.buildable = l.mkForce (!isWindows);
    plutus-core.components.tests.plutus-ir-test.buildable = l.mkForce (!isWindows);
    plutus-core.components.tests.untyped-plutus-core-test.buildable = l.mkForce (!isWindows);

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
    plutus-contract-model.package.buildable = !isCross;
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
    plutus-core.components.benchmarks.cost-model-test.buildable = l.mkForce (!isCross);
    plutus-core.components.benchmarks.update-cost-model.buildable = l.mkForce (!isCross);

    # ---------- Darwin Packages ----------

    plutus-pab-executables.components.tests.plutus-pab-test-full-long-running.buildable = l.mkForce (!isDarwin);

    # ---------- Common Packages ----------

    marconi-core.doHaddock = meta.enableHaddock;
    marconi-core.flags.defer-plugin-errors = meta.enableHaddock;

    marconi-chain-index.doHaddock = meta.enableHaddock;
    marconi-chain-index.flags.defer-plugin-errors = meta.enableHaddock;

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
    marconi-chain-index.preCheck = ''
      export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
      export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
      export CARDANO_NODE_SRC=${../.}
    '';

    plutus-contract.doHaddock = meta.enableHaddock;
    plutus-contract.flags.defer-plugin-errors = meta.enableHaddock;

    plutus-contract-model.doHaddock = meta.enableHaddock;
    plutus-contract-model.flags.defer-plugin-errors = meta.enableHaddock;

    plutus-use-cases.doHaddock = meta.enableHaddock;
    plutus-use-cases.flags.defer-plugin-errors = meta.enableHaddock;

    plutus-ledger.doHaddock = meta.enableHaddock;
    plutus-ledger.flags.defer-plugin-errors = meta.enableHaddock;

    plutus-script-utils.doHaddock = meta.enableHaddock;
    plutus-script-utils.flags.defer-plugin-errors = meta.enableHaddock;

    plutus-example.doHaddock = meta.enableHaddock;
    plutus-example.flags.defer-plugin-errors = meta.enableHaddock;

    plutus-example.preCheck = "
      export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
      export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
      export CARDANO_SUBMIT_API=${config.hsPkgs.cardano-submit-api.components.exes.cardano-submit-api}/bin/cardano-submit-api${pkgs.stdenv.hostPlatform.extensions.executable}
      export CREATE_SCRIPT_CONTEXT=${config.hsPkgs.plutus-example.components.exes.create-script-context}/bin/create-script-context${pkgs.stdenv.hostPlatform.extensions.executable}
      export CARDANO_NODE_SRC=${../.}
    ";

    plutus-example.components.tests.plutus-example-test.build-tools =
      l.mkForce (with pkgs.buildPackages; [ jq coreutils shellcheck lsof ]);

    # FIXME: Haddock mysteriously gives a spurious missing-home-modules warning
    plutus-tx-plugin.doHaddock = false;

    # Relies on cabal-doctest, just turn it off in the Nix build
    prettyprinter-configurable.components.tests.prettyprinter-configurable-doctest.buildable = l.mkForce false;

    # Broken due to warnings, unclear why the setting that fixes this for the build doesn't work here.
    iohk-monitoring.doHaddock = false;
    cardano-wallet.doHaddock = false;

    # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
    cardano-streaming.ghcOptions = [ "-Werror" ];
    marconi-chain-index.ghcOptions = [ "-Werror" ];
    marconi-core.ghcOptions = [ "-Werror" ];
    pab-blockfrost.ghcOptions = [ "-Werror" ];
    plutus-chain-index.ghcOptions = [ "-Werror" ];
    plutus-chain-index-core.ghcOptions = [ "-Werror" ];
    plutus-contract.ghcOptions = [ "-Werror" ];
    plutus-contract-model.ghcOptions = [ "-Werror" ];
    plutus-doc.ghcOptions = [ "-Werror" ];
    plutus-example.ghcOptions = [ "-Werror" ];
    plutus-ledger.ghcOptions = [ "-Werror" ];
    plutus-pab.ghcOptions = [ "-Werror" ];
    plutus-pab-executables.ghcOptions = [ "-Werror" ];
    plutus-script-utils.ghcOptions = [ "-Werror" ];
    plutus-tx-constraints.ghcOptions = [ "-Werror" ];
    plutus-use-cases.ghcOptions = [ "-Werror" ];
  };


  modules = [{ inherit packages; }];

in

{ inherit modules sha256map cabalProjectLocal; }

