############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, haskell-nix
, gitignore-nix
, z3
, libsodium-vrf
, libsecp256k1
, compiler-nix-name
, enableHaskellProfiling
  # Whether to set the `defer-plugin-errors` flag on those packages that need
  # it. If set to true, we will also build the haddocks for those packages.
, deferPluginErrors
}:
let
  project = haskell-nix.cabalProject' ({ pkgs, config, ... }: {
    inherit compiler-nix-name;
    # This is incredibly difficult to get right, almost everything goes wrong, see https://github.com/input-output-hk/haskell.nix/issues/496
    src = let root = ../../../.; in
      haskell-nix.haskellLib.cleanSourceWith {
        filter = gitignore-nix.gitignoreFilter root;
        src = root;
        # Otherwise this depends on the name in the parent directory, which reduces caching, and is
        # particularly bad on Hercules, see https://github.com/hercules-ci/support/issues/40
        name = "plutus-apps";
      };
    sha256map = import ./sha256map.nix;
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
            playground-common.package.buildable = false;
            plutus-benchmark.package.buildable = false;
            plutus-chain-index.package.buildable = false;
            plutus-chain-index-core.package.buildable = false;
            plutus-contract.package.buildable = false;
            plutus-contract-certification.package.buildable = false;
            plutus-errors.package.buildable = false;
            plutus-ledger.package.buildable = false;
            plutus-ledger-constraints.package.buildable = false;
            plutus-pab.package.buildable = false;
            plutus-pab-executables.package.buildable = false;
            plutus-playground-server.package.buildable = false; # Would also require libpq
            plutus-script-utils.package.buildable = false;
            plutus-streaming.package.buildable = false;
            plutus-tx-plugin.package.buildable = false;
            plutus-use-cases.package.buildable = false;
            plutus-example.package.buildable = false;
            web-ghc.package.buildable = false;
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
              ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
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
        ({ pkgs, config, ... }: {
          packages = {
            plutus-contract.doHaddock = deferPluginErrors;
            plutus-contract.flags.defer-plugin-errors = deferPluginErrors;

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

            plutus-pab-executables.components.tests.plutus-pab-test-full-long-running = {
              platforms = lib.platforms.linux;
            };

            # Broken due to warnings, unclear why the setting that fixes this for the build doesn't work here.
            iohk-monitoring.doHaddock = false;
            cardano-wallet.doHaddock = false;

            # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
            playground-common.ghcOptions = [ "-Werror" ];
            plutus-chain-index.ghcOptions = [ "-Werror" ];
            plutus-chain-index-core.ghcOptions = [ "-Werror" ];
            plutus-contract.ghcOptions = [ "-Werror" ];
            plutus-doc.ghcOptions = [ "-Werror" ];
            plutus-example.ghcOptions = [ "-Werror" ];
            plutus-ledger.ghcOptions = [ "-Werror" ];
            plutus-ledger-constraints.ghcOptions = [ "-Werror" ];
            plutus-playground-server.ghcOptions = [ "-Werror" ];
            plutus-pab.ghcOptions = [ "-Werror" ];
            plutus-pab-executables.ghcOptions = [ "-Werror" ];
            plutus-script-utils.ghcOptions = [ "-Werror" ];
            plutus-use-cases.ghcOptions = [ "-Werror" ];

            # Honestly not sure why we need this, it has a mysterious unused dependency on "m"
            # This will go away when we upgrade nixpkgs and things use ieee754 anyway.
            ieee.components.library.libs = lib.mkForce [ ];

            # See https://github.com/input-output-hk/iohk-nix/pull/488
            cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
            cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf libsecp256k1 ] ];
          };
        })
      ] ++ lib.optional enableHaskellProfiling {
        enableLibraryProfiling = true;
        enableProfiling = true;
      };
  });

in
project
