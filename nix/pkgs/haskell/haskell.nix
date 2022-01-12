############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, haskell-nix
, gitignore-nix
, z3
, libsodium-vrf
, checkMaterialization
, compiler-nix-name
, enableHaskellProfiling
  # Whether to set the `defer-plugin-errors` flag on those packages that need
  # it. If set to true, we will also build the haddocks for those packages.
, deferPluginErrors
}:
let
  project = haskell-nix.cabalProject' ({ pkgs, ... }: {
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
    # These files need to be regenerated when you change the cabal files.
    # See ../CONTRIBUTING.doc for more information.
    # Unfortuntely, they are *not* constant across all possible systems, so in some circumstances we need different sets of files
    # At the moment, we only need one but conceivably we might need one for darwin in future.
    # See https://github.com/input-output-hk/nix-tools/issues/97
    materialized =
      if pkgs.stdenv.hostPlatform.isLinux then ./materialized-linux
      else if pkgs.stdenv.hostPlatform.isDarwin then ./materialized-darwin
      else if pkgs.stdenv.hostPlatform.isWindows then ./materialized-windows
      else builtins.error "Don't have materialized files for this platform";
    # If true, we check that the generated files are correct. Set in the CI so we don't make mistakes.
    inherit checkMaterialization;
    sha256map = {
      "https://github.com/input-output-hk/iohk-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c" = "1il8fx3misp3650ryj368b3x95ksz01zz3x0z9k00807j93d0ka0";
      "https://github.com/input-output-hk/plutus"."cc72a56eafb02333c96f662581b57504f8f8992f" = "1w89ikv3jsg5x9xf7qpcjnix3nf016z0xpf48q5238h4ngvcqp9y";
      "https://github.com/Quid2/flat"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "1lrzknw765pz2j97nvv9ip3l1mcpf2zr4n56hwlz0rk7wq7ls4cm";
      "https://github.com/input-output-hk/servant-purescript"."44e7cacf109f84984cd99cd3faf185d161826963" = "10pb0yfp80jhb9ryn65a4rha2lxzsn2vlhcc6xphrrkf4x5lhzqc";
      "https://github.com/input-output-hk/purescript-bridge"."47a1f11825a0f9445e0f98792f79172efef66c00" = "0da1vn2l6iyfxcjk58qal1l4755v92zi6yppmjmqvxf1gacyf9px";
      "https://github.com/input-output-hk/cardano-wallet"."a5085acbd2670c24251cf8d76a4e83c77a2679ba" = "1apzfy7qdgf6l0lb3icqz3rvaq2w3a53xq6wvhqnbfi8i7cacy03";
      "https://github.com/input-output-hk/cardano-node"."814df2c146f5d56f8c35a681fe75e85b905aed5d" = "1hr00wqzmcyc3x0kp2hyw78rfmimf6z4zd4vv85b9zv3nqbjgrik";
      "https://github.com/input-output-hk/cardano-ledger"."1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5" = "0avzyiqq0m8njd41ck9kpn992yq676b1az9xs77977h7cf85y4wm";
      "https://github.com/input-output-hk/ouroboros-network"."d2d219a86cda42787325bb8c20539a75c2667132" = "18xk7r0h2pxrbx76d6flsxifh0a9rz1cj1rjqs1pbs5kdmy8b7kx";
      "https://github.com/input-output-hk/cardano-base"."41545ba3ac6b3095966316a99883d678b5ab8da8" = "0icq9y3nnl42fz536da84414av36g37894qnyw4rk3qkalksqwir";
      "https://github.com/input-output-hk/cardano-prelude"."bb4ed71ba8e587f672d06edf9d2e376f4b055555" = "00h10l5mmiza9819p9v5q5749nb9pzgi20vpzpy1d34zmh6gf1cj";
      "https://github.com/input-output-hk/cardano-crypto"."f73079303f663e028288f9f4a9e08bcca39a923e" = "1n87i15x54s0cjkh3nsxs4r1x016cdw1fypwmr68936n3xxsjn6q";
      "https://github.com/input-output-hk/cardano-addresses"."71006f9eb956b0004022e80aadd4ad50d837b621" = "11dl3fmq7ry5wdmz8kw07ji8yvrxnrsf7pgilw5q9mi4aqyvnaqk";
      "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
      "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
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
    modules = [
      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
        packages = {
          # Things that need plutus-tx-plugin
          playground-common.package.buildable = false;
          plutus-benchmark.package.buildable = false;
          plutus-chain-index.package.buildable = false;
          plutus-chain-index-core.package.buildable = false;
          plutus-contract.package.buildable = false;
          plutus-errors.package.buildable = false;
          plutus-ledger.package.buildable = false;
          plutus-ledger-constraints.package.buildable = false;
          plutus-pab.package.buildable = false;
          plutus-pab-executables.package.buildable = false;
          plutus-playground-server.package.buildable = false; # Would also require libpq
          plutus-tx-plugin.package.buildable = false;
          plutus-use-cases.package.buildable = false;
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

          # FIXME: Haddock mysteriously gives a spurious missing-home-modules warning
          plutus-tx-plugin.doHaddock = false;

          # Relies on cabal-doctest, just turn it off in the Nix build
          prettyprinter-configurable.components.tests.prettyprinter-configurable-doctest.buildable = lib.mkForce false;

          plutus-pab-executables.components.tests.plutus-pab-test-full-long-running = {
            platforms = lib.platforms.linux;
          };

          # Broken due to warnings, unclear why the setting that fixes this for the build doesn't work here.
          iohk-monitoring.doHaddock = false;

          # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
          playground-common.ghcOptions = [ "-Werror" ];
          plutus-chain-index.ghcOptions = [ "-Werror" ];
          plutus-chain-index-core.ghcOptions = [ "-Werror" ];
          plutus-contract.ghcOptions = [ "-Werror" ];
          plutus-ledger.ghcOptions = [ "-Werror" ];
          plutus-ledger-constraints.ghcOptions = [ "-Werror" ];
          plutus-playground-server.ghcOptions = [ "-Werror" ];
          plutus-pab.ghcOptions = [ "-Werror" ];
          plutus-pab-executables.ghcOptions = [ "-Werror" ];
          plutus-doc.ghcOptions = [ "-Werror" ];
          plutus-use-cases.ghcOptions = [ "-Werror" ];

          # Honestly not sure why we need this, it has a mysterious unused dependency on "m"
          # This will go away when we upgrade nixpkgs and things use ieee754 anyway.
          ieee.components.library.libs = lib.mkForce [ ];

          # See https://github.com/input-output-hk/iohk-nix/pull/488
          cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
          cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
        };
      })
    ] ++ lib.optional enableHaskellProfiling {
      enableLibraryProfiling = true;
      enableExecutableProfiling = true;
    };
  });

in
project
