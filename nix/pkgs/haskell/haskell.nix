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
, topLevelPkgs
, ghcjsPluginPkgs ? null
, cabalProjectLocal ? null
}@args:
let
  compiler-nix-name = if topLevelPkgs.stdenv.hostPlatform.isGhcjs then "ghc8107" else args.compiler-nix-name;
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
      if pkgs.stdenv.hostPlatform.isLinux then (if topLevelPkgs.stdenv.hostPlatform.isGhcjs then ./materialized-ghcjs-build else ./materialized-linux)
      else if pkgs.stdenv.hostPlatform.isGhcjs then ./materialized-ghcjs
      else if pkgs.stdenv.hostPlatform.isDarwin then ./materialized-darwin
      else if pkgs.stdenv.hostPlatform.isWindows then ./materialized-windows
      else builtins.error "Don't have materialized files for this platform";
    # If true, we check that the generated files are correct. Set in the CI so we don't make mistakes.
    # inherit checkMaterialization;
    checkMaterialization = true;
    sha256map = {
      "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "1lrzknw765pz2j97nvv9ip3l1mcpf2zr4n56hwlz0rk7wq7ls4cm";
      "https://github.com/input-output-hk/purescript-bridge.git"."6a92d7853ea514be8b70bab5e72077bf5a510596" = "13j64vv116in3c204qsl1v0ajphac9fqvsjp7x3zzfr7n7g61drb";
      "https://github.com/input-output-hk/servant-purescript.git"."a0c7c7e37c95564061247461aef4be505a853538" = "177na04jf6wf18kandzsah40lw3xswmmccpr3hkb8wb4hypcffnf";
      "https://github.com/input-output-hk/cardano-base"."4ea7e2d927c9a7f78ddc69738409a5827ab66b98" = "0n0hxbr0l95cdc25jmmgs7apmmw17i91chhj5rzzv1k7f3iymf6d";
      "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "06sdx5ndn2g722jhpicmg96vsrys89fl81k8290b3lr6b1b0w4m3";
      "https://github.com/input-output-hk/cardano-ledger-specs"."bf008ce028751cae9fb0b53c3bef20f07c06e333" = "0my3801w1vinc0kf5yh9lxl6saqxgwm6ccg0vvzi104pafcwwcqx";
      "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" = "02jddik1yw0222wd6q0vv10f7y8rdgrlqaiy83ph002f9kjx7mh6";
      "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
      "https://github.com/input-output-hk/iohk-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c" = "1il8fx3misp3650ryj368b3x95ksz01zz3x0z9k00807j93d0ka0";
      "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" = "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";
      "https://github.com/input-output-hk/ouroboros-network"."1f4973f36f689d6da75b5d351fb124d66ef1057d" = "186056rvzdzy4jhvamjjbcmjyr94hs5hcyr8x6a0ch21hv5f014p";
      "https://github.com/input-output-hk/cardano-node.git"."b6ca519f97a0e795611a63174687e6bb70c9f752" = "0z5lpmqc98fwg3xzpzxkfslbxdjwfyyw8bn8yq0574sf4942vqdn";
      "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
      "https://github.com/input-output-hk/hedgehog-extras"."edf6945007177a638fbeb8802397f3a6f4e47c14" = "0wc7qzkc7j4ns2rz562h6qrx2f8xyq7yjcb7zidnj7f6j0pcd0i9";
      "https://github.com/input-output-hk/cardano-wallet"."ae7569293e94241ef6829139ec02bd91abd069df" = "1mv1dhpkdj9ridm1fvq6jc85qs6zvbp172228rq72gyawjwrgvi6";
      "https://github.com/input-output-hk/cardano-addresses"."35f9c49dcd953b45da6dfbcb84b39c3f448ced58" = "sha256-dpRlLB0d6IhBwi3i6gGdNsD1Wt99ldiLTSwa1ld2YrM=";
      "https://github.com/input-output-hk/plutus"."dda41bc89f93953b63ed0e37ae04c13319e6c87b" = "04gr909cqv86pf2ljyxh813jmn34p431jmjk24zq1jc0ppj30d7h";
    };
    # Configuration settings needed for cabal configure to work when cross compiling
    # for windows. We can't use `modules` for these as `modules` are only applied
    # after cabal has been configured.
    cabalProjectLocal = lib.optionalString pkgs.stdenv.hostPlatform.isWindows ''
      package plutus-tx-plugin
        flags: +use-ghc-stub

      -- Exlcude test that use `doctest`.  They will not work for windows
      -- cross compilation and `cabal` will not be able to make a plan.
      package prettyprinter-configurable
        tests: False
    '' + lib.optionalString pkgs.stdenv.hostPlatform.isGhcjs ''
      packages:
        contrib/*
      package plutus-tx-plugin
        flags: +use-ghc-stub
      package prettyprinter-configurable
        tests: False
      package network
        tests: False

      allow-newer:
             stm:base

           -- ghc-boot 8.10.4 is not in hackage, so haskell.nix needs consider 8.8.3
           -- when cross compiling for windows or it will fail to find a solution including
           -- a new Win32 version (ghc-boot depends on Win32 via directory)
           , ghc-boot:base
           , ghc-boot:ghc-boot-th
           , snap-server:attoparsec
           , io-streams-haproxy:attoparsec
           , snap-core:attoparsec
           , websockets:attoparsec
           , jsaddle:base64-bytestring

      source-repository-package
        type: git
        location: https://github.com/hamishmack/foundation
        tag: 421e8056fabf30ef2f5b01bb61c6880d0dfaa1c8
        --sha256: 0cbsj3dyycykh0lcnsglrzzh898n2iydyw8f2nwyfvfnyx6ac2im
        subdir: foundation
    '' + lib.optionalString (topLevelPkgs.stdenv.hostPlatform.isGhcjs && !pkgs.stdenv.hostPlatform.isGhcjs) ''
      packages:
        ${topLevelPkgs.buildPackages.haskell-nix.compiler.${compiler-nix-name}.project.configured-src}
        contrib/double-conversion-2.0.2.0

      allow-newer: ghcjs:base16-bytestring
                 , ghcjs:aeson
                 , stm:base
                 , cardano-binary:recursion-schemes
                 , jsaddle:ghcjs-base
                 , ghcjs-base:primitive
                 , ghcjs-base:time
                 , ghcjs-base:hashable
                 , ghcjs-base:aeson
                 , servant-foreign:lens
                 , servant-client:http-client
      constraints: plutus-tx-plugin +ghcjs-plugin,
                   ghci +ghci

      package ghci
        flags: +ghci

      package plutus-tx-plugin
        flags: +ghcjs-plugin

      -- The following is needed because Nix is doing something crazy.
      package byron-spec-ledger
        tests: False

      package plutus-doc
        tests: False

      package prettyprinter-configurable
        tests: False

      package small-steps
        tests: False

      package small-steps-test
        tests: False

      package byron-spec-chain
        tests: False
    '';
    modules = [
      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
        packages = {
          # Things that need plutus-tx-plugin
          web-ghc.package.buildable = false;
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
          ghcjs.components.library.build-tools = let alex = pkgs.haskell-nix.tool compiler-nix-name "alex" {
            index-state = pkgs.haskell-nix.internalHackageIndexState;
            version = "3.2.5";
          }; in [ alex ];
          ghcjs.flags.use-host-template-haskell = true;

          # This is important. We may be reinstalling lib:ghci, and if we do
          # it *must* have the ghci flag enabled (default is disabled).
          ghci.flags.ghci = true;

          plutus-use-cases.ghcOptions =
            if (ghcjsPluginPkgs != null && pkgs.stdenv.hostPlatform.isGhcjs)
            then
              (
                let attr = ghcjsPluginPkgs.haskell.projectPackages.plutus-tx-plugin.components.library;
                in
                [
                  "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                  "-host-package-db ${attr}/package.conf.d"
                  "-Werror"
                ]
              )
            else __trace "nativePlutus is null" [ ];

          plutus-tx-plugin.ghcOptions =
            if (ghcjsPluginPkgs != null && pkgs.stdenv.hostPlatform.isGhcjs)
            then
              (
                let attr = ghcjsPluginPkgs.haskell.projectPackages.plutus-tx-plugin.components.library;
                in
                [
                  "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                  "-host-package-db ${attr}/package.conf.d"
                  #                                              "-Werror"
                ]
              )
            else __trace "nativePlutus is null" [ ];

          plutus-tx-tests.ghcOptions =
            if (ghcjsPluginPkgs != null && pkgs.stdenv.hostPlatform.isGhcjs)
            then
              (
                let attr = ghcjsPluginPkgs.haskell.projectPackages.plutus-tx-plugin.components.library;
                in
                [
                  "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                  "-host-package-db ${attr}/package.conf.d"
                  #                                              "-Werror"
                ]
              )
            else __trace "nativePlutus is null" [ ];

          plutus-errors.ghcOptions =
            if (ghcjsPluginPkgs != null && pkgs.stdenv.hostPlatform.isGhcjs)
            then
              (
                let attr = ghcjsPluginPkgs.haskell.projectPackages.plutus-tx-plugin.components.library;
                in
                [
                  "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                  "-host-package-db ${attr}/package.conf.d"
                  "-Werror"
                ]
              )
            else __trace "nativePlutus is null" [ ];

          plutus-benchmark.ghcOptions =
            if (ghcjsPluginPkgs != null && pkgs.stdenv.hostPlatform.isGhcjs)
            then
              (
                let attr = ghcjsPluginPkgs.haskell.projectPackages.plutus-tx-plugin.components.library;
                in
                [
                  "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                  "-host-package-db ${attr}/package.conf.d"
                  "-Werror"
                ]
              )
            else __trace "nativePlutus is null" [ ];


          plutus-ledger.components.library.build-tools = if (ghcjsPluginPkgs != null && pkgs.stdenv.hostPlatform.isGhcjs) then [ pkgs.pkgsCross.ghcjs.buildPackages.haskell-nix.compiler.${compiler-nix-name}.buildGHC ] else [ ];
          plutus-ledger.ghcOptions =
            if (ghcjsPluginPkgs != null && pkgs.stdenv.hostPlatform.isGhcjs)
            then
              (
                let attr = ghcjsPluginPkgs.haskell.projectPackages.plutus-tx-plugin.components.library;
                in
                [
                  "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                  "-host-package-db ${attr}/package.conf.d"
                  "-Werror"
                ]
              )
            else __trace "nativePlutus is null" [ ];

          plutus-ledger-test.ghcOptions =
            if (ghcjsPluginPkgs != null && pkgs.stdenv.hostPlatform.isGhcjs)
            then
              (
                let attr = ghcjsPluginPkgs.haskell.projectPackages.plutus-tx-plugin.components.library;
                in
                [
                  "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                  "-host-package-db ${attr}/package.conf.d"
                  "-Werror"
                ]
              )
            else __trace "nativePlutus is null" [ ];

          plutus-pab.ghcOptions =
            if (ghcjsPluginPkgs != null && pkgs.stdenv.hostPlatform.isGhcjs)
            then
              (
                let attr = ghcjsPluginPkgs.haskell.projectPackages.plutus-tx-plugin.components.library;
                in
                [
                  "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                  "-host-package-db ${attr}/package.conf.d"
                  "-Werror"
                ]
              )
            else __trace "nativePlutus is null" [ ];

          Cabal.patches = [ ../../patches/cabal.patch ];

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

          plutus-pab.components.tests.plutus-pab-test-full-long-running = {
            platforms = lib.platforms.linux;
          };

          # Broken due to warnings, unclear why the setting that fixes this for the build doesn't work here.
          iohk-monitoring.doHaddock = false;

          # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
          playground-common.ghcOptions = [ "-Werror" ];
          plutus-contract.ghcOptions = [ "-Werror" ];
          # plutus-ledger.ghcOptions = [ "-Werror" ];
          plutus-ledger-api.ghcOptions = [ "-Werror" ];
          plutus-playground-server.ghcOptions = [ "-Werror" ];
          # plutus-pab.ghcOptions = [ "-Werror" ];
          plutus-tx.ghcOptions = [ "-Werror" ];
          # plutus-tx-plugin.ghcOptions = [ "-Werror" ];
          plutus-doc.ghcOptions = [ "-Werror" ];
          # plutus-use-cases.ghcOptions = [ "-Werror" ];

          # Honestly not sure why we need this, it has a mysterious unused dependency on "m"
          # This will go away when we upgrade nixpkgs and things use ieee754 anyway.
          ieee.components.library.libs = lib.mkForce [ ];
        };
      })
      ({ pkgs, ... }: lib.mkIf (!pkgs.stdenv.hostPlatform.isGhcjs) {
        packages = {
          # See https://github.com/input-output-hk/iohk-nix/pull/488
          cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
          cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
        };
      })
      # For GHCJS
      ({ packages.ghcjs.src = topLevelPkgs.buildPackages.haskell-nix.compiler.${compiler-nix-name}.project.configured-src; })
      (lib.mkIf (topLevelPkgs.stdenv.hostPlatform.isGhcjs) {
        packages.double-conversion.components.library.libs = lib.mkForce [ ];
      })
      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform.isGhcjs) {
        packages = {
          lzma.components.library.libs = lib.mkForce [ pkgs.buildPackages.lzma ];
          cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.buildPackages.libsodium-vrf ] ];
          cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.buildPackages.libsodium-vrf ] ];
          plutus-core.ghcOptions = [ "-Wno-unused-packages" ];
          iohk-monitoring.ghcOptions = [ "-Wno-deprecations" ]; # TODO find alternative fo libyaml
        };
      })
      ({ pkgs, config, ... }@args: {
        packages.cardano-addresses-jsbits.components.library.preConfigure =
          (import (args.config.packages.cardano-addresses.src + "/jsbits/emscripten") args).packages.cardano-addresses-jsbits.components.library.preConfigure;
      })
    ] ++ lib.optional enableHaskellProfiling {
      enableLibraryProfiling = true;
      enableExecutableProfiling = true;
    };
  });

in
project
