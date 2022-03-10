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
    inherit checkMaterialization;
    sha256map = {
      "https://github.com/input-output-hk/iohk-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c" = "1il8fx3misp3650ryj368b3x95ksz01zz3x0z9k00807j93d0ka0";
      "https://github.com/input-output-hk/plutus"."73f2d9d749d19de058996442b76e4d0068fc87ef" = "1mh84qldc7bg84884aqfwhhwx3f93jp5bdb240gs8ba6rbsa9s8p";
      "https://github.com/Quid2/flat"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "1lrzknw765pz2j97nvv9ip3l1mcpf2zr4n56hwlz0rk7wq7ls4cm";
      "https://github.com/input-output-hk/servant-purescript"."44e7cacf109f84984cd99cd3faf185d161826963" = "10pb0yfp80jhb9ryn65a4rha2lxzsn2vlhcc6xphrrkf4x5lhzqc";
      "https://github.com/input-output-hk/purescript-bridge"."47a1f11825a0f9445e0f98792f79172efef66c00" = "0da1vn2l6iyfxcjk58qal1l4755v92zi6yppmjmqvxf1gacyf9px";
      "https://github.com/input-output-hk/cardano-wallet"."4e0e71fdf22594d0c91cd1c0f31fd59bd0f58889" = "1wz4lyy4g1xy4mc6x9i3fq89fmxwj02ry08kv3g0dw3j84j297mi";
      "https://github.com/input-output-hk/cardano-node"."814df2c146f5d56f8c35a681fe75e85b905aed5d" = "1hr00wqzmcyc3x0kp2hyw78rfmimf6z4zd4vv85b9zv3nqbjgrik";
      "https://github.com/input-output-hk/cardano-ledger"."1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5" = "0avzyiqq0m8njd41ck9kpn992yq676b1az9xs77977h7cf85y4wm";
      "https://github.com/input-output-hk/ouroboros-network"."d2d219a86cda42787325bb8c20539a75c2667132" = "18xk7r0h2pxrbx76d6flsxifh0a9rz1cj1rjqs1pbs5kdmy8b7kx";
      "https://github.com/input-output-hk/cardano-base"."44e7572b0cde519b5741982d6a4bb088b2c1d4e0" = "01w1wfwr2wyzaalzb9v1zbvln771xnl0ynixzyw6zzxmhs887fmm";
      "https://github.com/input-output-hk/cardano-prelude"."bb4ed71ba8e587f672d06edf9d2e376f4b055555" = "00h10l5mmiza9819p9v5q5749nb9pzgi20vpzpy1d34zmh6gf1cj";
      "https://github.com/input-output-hk/cardano-crypto"."1fff72e39e690676d4156a56858c6b72e1f0bff9" = "06kahs46z842xndq3sgcrqyvmgvs05rnflbq76599pfnb2vspy2q";
      "https://github.com/input-output-hk/cardano-addresses"."35f9c49dcd953b45da6dfbcb84b39c3f448ced58" = "1cv2frbxc6ic9n5xi5bxvxdgbh1nkl0ymqidq90qis0x3ln6b53n";
      "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
      "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
      "https://github.com/input-output-hk/cardano-config"."e9de7a2cf70796f6ff26eac9f9540184ded0e4e6" = "1wm1c99r5zvz22pdl8nhkp13falvqmj8dgkm8fxskwa9ydqz01ld";
      "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" = "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";
      "https://github.com/input-output-hk/hedgehog-extras"."edf6945007177a638fbeb8802397f3a6f4e47c14" = "0wc7qzkc7j4ns2rz562h6qrx2f8xyq7yjcb7zidnj7f6j0pcd0i9";
      "https://raw.githubusercontent.com/input-output-hk/hackage-overlay-ghcjs/fdb618b59d8fd5ef101acc97dee6a4f2f59f898b" = "1cpni2xw24kllpx76y4fwqxspv7kzrsy76ksk104zha5wp7wcr01";
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

      allow-newer:
             ouroboros-consensus:Win32

    '' + lib.optionalString pkgs.stdenv.hostPlatform.isGhcjs ''
      package plutus-tx-plugin
        flags: +use-ghc-stub
      package prettyprinter-configurable
        tests: False
      package network
        tests: False
      package double-conversion
        tests: False
      package freer-extras
        tests: False
      package beam-sqlite
        tests: True
      package clock
        tests: False
        benchmarks: False

      package cryptohash-sha256
        flags: -use-cbits

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
           -- no idea why ouroboros-consensus-byron restricts to <.28
           , ouroboros-consensus-byron:cryptonite
           -- Needed for the version of beam-sqllite in ghcjs-overlay
           , beam-sqlite:aeson

      constraints:
          cborg < 0.2.6.0
        -- Make sure we use versions in ghcjs-overlay
        , network == 3.1.2.5
        , unix-compat == 0.5.3
        , basement == 0.0.12
        , foundation == 0.0.26.1
        , beam-sqlite == 0.5.0.0
        , digest == 0.0.1.2
        , unix-bytestring == 0.3.7.3

      index-state: ghcjs-overlay HEAD
      repository ghcjs-overlay
        url: https://raw.githubusercontent.com/input-output-hk/hackage-overlay-ghcjs/fdb618b59d8fd5ef101acc97dee6a4f2f59f898b
        secure: True
        root-keys:
        key-threshold: 0

    '' + lib.optionalString (topLevelPkgs.stdenv.hostPlatform.isGhcjs && !pkgs.stdenv.hostPlatform.isGhcjs) ''
      packages:
        ${topLevelPkgs.buildPackages.haskell-nix.compiler.${compiler-nix-name}.project.configured-src}


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
                 -- no idea why ouroboros-consensus-byron restricts to <.28
                 , ouroboros-consensus-byron:cryptonite

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
      ({ pkgs, ... }: lib.mkIf (!pkgs.stdenv.hostPlatform.isGhcjs && pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
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
          plutus-example.package.buildable = false;
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
      ({ pkgs, config, ... }:
        let plutus-tx-plugin-ghc-options = 
              let attr = ghcjsPluginPkgs.haskell.project.hsPkgs.plutus-tx-plugin.components.library;
              in lib.optionals (ghcjsPluginPkgs != null && pkgs.stdenv.hostPlatform.isGhcjs)
                [
                  "-host-package-db ${attr.passthru.configFiles}/${attr.passthru.configFiles.packageCfgDir}"
                  "-host-package-db ${attr}/package.conf.d"
                ];
        in {
        packages = {
          ghcjs.components.library.build-tools = let alex = pkgs.haskell-nix.tool compiler-nix-name "alex" {
            index-state = pkgs.haskell-nix.internalHackageIndexState;
            version = "3.2.5";
          }; in [ alex ];
          ghcjs.flags.use-host-template-haskell = true;

          # This is important. We may be reinstalling lib:ghci, and if we do
          # it *must* have the ghci flag enabled (default is disabled).
          ghci.flags.ghci = true;

          cardano-wallet-core.patches = [ ../../patches/cardano-wallet-pr-3074.patch ];

          plutus-contract.doHaddock = deferPluginErrors;
          plutus-contract.flags.defer-plugin-errors = deferPluginErrors;

          plutus-use-cases.doHaddock = deferPluginErrors;
          plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

          plutus-ledger.doHaddock = deferPluginErrors;
          plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;

          plutus-example.doHaddock = deferPluginErrors;
          plutus-example.flags.defer-plugin-errors = deferPluginErrors;

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
          plutus-chain-index.ghcOptions =
            # "-Wno-deprecations" works around
            #    Module ‘Data.Yaml’:
            #      GHCJS is not supported yet (will break at runtime once called).
            lib.optional (ghcjsPluginPkgs != null && pkgs.stdenv.hostPlatform.isGhcjs) "-Wno-deprecations"
              ++ [ "-Werror" ];
          plutus-chain-index-core.ghcOptions = [ "-Werror" ];
          plutus-contract.ghcOptions = plutus-tx-plugin-ghc-options ++ [ "-Werror" ];
          plutus-ledger.components.library.build-tools = if (ghcjsPluginPkgs != null && pkgs.stdenv.hostPlatform.isGhcjs) then [ pkgs.pkgsCross.ghcjs.buildPackages.haskell-nix.compiler.${compiler-nix-name}.buildGHC ] else [ ];
          plutus-ledger.ghcOptions = plutus-tx-plugin-ghc-options ++ [ "-Werror" ];
          plutus-ledger-constraints.ghcOptions = [ "-Werror" ];
          plutus-ledger-test.ghcOptions = plutus-tx-plugin-ghc-options;
          plutus-playground-server.ghcOptions = [ "-Werror" ];
          plutus-pab.ghcOptions = plutus-tx-plugin-ghc-options ++ [ "-Werror" ] ++
            # Let's not fail on this nonsense.
            #src/Plutus/PAB/Run.hs:32:1: error: [-Wdeprecations, -Werror=deprecations]
            #    Module ‘Data.Yaml’:
            #      GHCJS is not supported yet (will break at runtime once called).
            lib.optional (ghcjsPluginPkgs != null && pkgs.stdenv.hostPlatform.isGhcjs) "-Wno-deprecations";
          plutus-pab-executables.ghcOptions = [ "-Werror" ];
          plutus-doc.ghcOptions = [ "-Werror" ];
          plutus-use-cases.ghcOptions = plutus-tx-plugin-ghc-options ++ [ "-Werror" ];
          plutus-example.ghcOptions = plutus-tx-plugin-ghc-options ++ [ "-Werror" ];
          plutus-tx-plugin.ghcOptions = plutus-tx-plugin-ghc-options;
          plutus-tx-tests.ghcOptions = plutus-tx-plugin-ghc-options;
          plutus-errors.ghcOptions = plutus-tx-plugin-ghc-options;
          plutus-benchmark.ghcOptions = plutus-tx-plugin-ghc-options;

          # Honestly not sure why we need this, it has a mysterious unused dependency on "m"
          # This will go away when we upgrade nixpkgs and things use ieee754 anyway.
          ieee.components.library.libs = lib.mkForce [ ];
        };
      })
      ({ pkgs, ... }:
        if (topLevelPkgs.stdenv.hostPlatform.isGhcjs) then {
          packages = {
            # See https://github.com/input-output-hk/iohk-nix/pull/488
            # TODO figure out why this needs to be `buildPackages` here for ghcjs to work.
            cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.buildPackages.libsodium-vrf ] ];
            cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.buildPackages.libsodium-vrf ] ];
          };
        }
        else {
          packages = {
            # See https://github.com/input-output-hk/iohk-nix/pull/488
            # TODO figure out why this needs to be `buildPackages` here for ghcjs to work.
            cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
            cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
          };
        })
      # For GHCJS
      ({ packages.ghcjs.src = topLevelPkgs.buildPackages.haskell-nix.compiler.${compiler-nix-name}.project.configured-src; })
      (lib.mkIf (topLevelPkgs.stdenv.hostPlatform.isGhcjs) {
        packages.double-conversion.components.library.libs = lib.mkForce [ ];
      })
      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform.isGhcjs) {
        packages =
          let
            runEmscripten = ''
              patchShebangs jsbits/emscripten/build.sh
              (cd jsbits/emscripten && PATH=${
                  # The extra buildPackages here is for closurecompiler.
                  # Without it we get `unknown emulation for platform: js-unknown-ghcjs` errors.
                  lib.makeBinPath (with pkgs.buildPackages.buildPackages;
                    [ emscripten closurecompiler coreutils python2 ])
                }:$PATH ./build.sh)
            '';
            libsodium-vrf = pkgs.libsodium-vrf.overrideAttrs (attrs: {
              nativeBuildInputs = attrs.nativeBuildInputs or [ ] ++ (with pkgs.buildPackages.buildPackages; [ emscripten python2 ]);
              prePatch = ''
                export HOME=$(mktemp -d)
                export PYTHON=${pkgs.buildPackages.buildPackages.python2}/bin/python
              '' + attrs.prePatch or "";
              configurePhase = ''
                emconfigure ./configure --prefix=$out --enable-minimal --disable-shared --without-pthreads --disable-ssp --disable-asm --disable-pie CFLAGS=-Os
              '';
              CC = "emcc";
            });
            emzlib = pkgs.zlib.overrideAttrs (attrs: {
              # makeFlags in nixpks zlib derivation depends on stdenv.cc.targetPrefix, which we don't have :(

              prePatch = ''
                export HOME=$(mktemp -d)
                export PYTHON=${pkgs.buildPackages.buildPackages.python2}/bin/python
              '' + attrs.prePatch or "";
              makeFlags = "PREFIX=js-unknown-ghcjs-";
              # We need the same patching as macOS
              postPatch = ''
                substituteInPlace configure \
                  --replace '/usr/bin/libtool' 'emar' \
                  --replace 'AR="libtool"' 'AR="emar"' \
                  --replace 'ARFLAGS="-o"' 'ARFLAGS="-r"'
              '';
              configurePhase = ''
                emconfigure ./configure --prefix=$out --static
              '';

              nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ (with pkgs.buildPackages.buildPackages; [ emscripten python2 ]);

              CC = "emcc";
              AR = "emar";

              # prevent it from passing `-lc`, which emcc doesn't like.
              LDSHAREDLIBC = "";
            });
          in
          {
            cardano-wallet-core.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitReallyMinimal ];
            # this should be static! And build with emscripten, see libsodium-vrf above.
            lzma.components.library.libs = lib.mkForce [ pkgs.buildPackages.lzma ];
            cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
            cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
            digest.components.library.libs = lib.mkForce [ emzlib.static emzlib ];
            plutus-core.ghcOptions = [ "-Wno-unused-packages" ];
            iohk-monitoring.ghcOptions = [ "-Wno-deprecations" ]; # TODO find alternative fo libyaml
            plutus-pab.components.tests.psgenerator.buildable = false;
            plutus-playground-server.package.buildable = false;

            # basement.patches = [ ../../../contrib/basement-0.0.12.patch ];
            # beam-sqlite.patches = [ ../../../contrib/beam-sqlite-0.5.0.0.patch ];
            # clock.patches = [ ../../../contrib/clock-0.8.2.patch ];
            # cryptonite.patches = [ ../../../contrib/cryptonite-0.29.patch ];
            # digest.patches = [ ../../../contrib/digest-0.0.1.2.patch ];
            # direct-sqlite.patches = [ ../../../contrib/direct-sqlite-2.3.26.patch ];
            # double-conversion.patches = [ ../../../contrib/double-conversion-2.0.2.0.patch ];
            # foundation.patches = [ ../../../contrib/foundation-0.0.26.1.patch ];
            # gauge.patches = [ ../../../contrib/gauge-0.2.5.patch ];
            # lzma.patches = [ ../../../contrib/lzma-0.0.0.3.patch ];
            # mersenne-random-pure64.patches = [ ../../../contrib/mersenne-random-pure64-0.2.2.0.patch ];
            # network.patches = [ ../../../contrib/network-3.1.2.1.patch ];
            # network.postUnpack = ''
            #   export patchFlags="--binary -p1"
            # '';

            # network-info.patches = [ ../../../contrib/network-info-0.2.0.10.patch ];
            # network-info.postUnpack = ''
            #   export patchFlags="--binary -p1"
            # '';
            # scrypt.patches = [ ../../../contrib/scrypt-0.5.0.patch ];
            # terminal-size.patches = [ ../../../contrib/terminal-size-0.3.2.1.patch ];
            # unix-bytestring.patches = [ ../../../contrib/unix-bytestring-0.3.7.3.patch ];
            # unix-compat.patches = [ ../../../contrib/unix-compat-0.5.3.patch ];
          };
      })
      ({ pkgs, ... }: {
        packages.plutus-pab.components.exes.pab-mktx-lib.postInstall = ''
          ${pkgs.buildPackages.tree}/bin/tree $out
          mkdir -p $out/_pkg
          # copy over all executables
          cp -r $out/bin/* $out/_pkg/

          ${pkgs.buildPackages.tree}/bin/tree $out/_pkg
          (cd $out/_pkg; ${pkgs.buildPackages.zip}/bin/zip -r -9 $out/pkg.zip *)
          rm -fR $out/_pkg

          mkdir -p $out/nix-support
          echo "file binary-dist \"$(echo $out/*.zip)\"" \
              > $out/nix-support/hydra-build-products
        '';
      })
    ] ++ lib.optional
      enableHaskellProfiling
      {
        enableLibraryProfiling = true;
        enableExecutableProfiling = true;
      };
  });

in
project
