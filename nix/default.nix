{ system ? builtins.currentSystem
, crossSystem ? null
, config ? { }
, overlays ? [ ]
, sourcesOverride ? { }
, sources
, haskellNix
, checkMaterialization ? false
, enableHaskellProfiling ? false
}:
let
  ownOverlays =
    [
      # Modifications to derivations from nixpkgs
      (import ./overlays/nixpkgs-overrides.nix)
      # fix r-modules
      (import ./overlays/r.nix)
      # stdenv.lib is still needed by the pinned version of easy purescipt
      (final: prev: { stdenv = prev.stdenv // { inherit (final) lib; }; })
    ];

  iohkNixMain = import sources.iohk-nix { };

  extraOverlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.nixpkgsArgs.overlays
    # our own overlays:
    # needed for cardano-api wich uses a patched libsodium
    ++ iohkNixMain.overlays.crypto
    ++ ownOverlays;

  pkgs = import sources.nixpkgs {
    inherit crossSystem;
    # In nixpkgs versions older than 21.05, if we don't explicitly pass
    # in localSystem we will hit a code path that uses builtins.currentSystem,
    # which breaks flake's pure evaluation.
    localSystem = { inherit system; };
    overlays = extraOverlays ++ overlays;
    config = haskellNix.nixpkgsArgs.config // config;
  };

  nativePkgs = import sources.nixpkgs {
    overlays = extraOverlays ++ overlays;
    config = haskellNix.config // config;
  };

  ghcjsPluginPkgs =
    if !pkgs.stdenv.targetPlatform.isGhcjs
    then null
    else
      import ./pkgs {
        pkgs = nativePkgs;
        inherit checkMaterialization enableHaskellProfiling sources;
        #      -- packages: ${pkgs.pkgsCross.ghcjs.buildPackages.haskell-nix.compiler.${compiler-nix-name}.configured-src}
        cabalProjectLocal = ''

      source-repository-package
        type: git
        location: https://github.com/ghcjs/ghcjs.git
        tag: e4cd4232a31f6371c761acd93853702f4c7ca74c
        --sha256: 1q52p91cvkm6ckpcgfrkd3crk68zcaxik5i3gxdbfppf0ljlrlbi

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
      constraints: plutus-tx +ghcjs-plugin,
                   ghci +ghci

      package ghci
        flags: +ghci

      package plutus-tx
        flags: +ghcjs-plugin

      -- The following is needed because Nix is doing something crazy.
      package byron-spec-ledger
        tests: False

      package marlowe
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
      };
  plutus = import ./pkgs { inherit pkgs checkMaterialization enableHaskellProfiling sources ghcjsPluginPkgs; };

in
{
  inherit pkgs ghcjsPluginPkgs plutus sources;
}
