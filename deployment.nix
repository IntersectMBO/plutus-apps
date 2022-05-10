{ system ? builtins.currentSystem
, enableHaskellProfiling ? false
, packages ? import ./. { inherit system enableHaskellProfiling; }
}:
let
  inherit (packages) pkgs plutus-apps plutus-playground pab-nami-demo plutus-chain-index pab-cli docs webCommon;
  inherit (pkgs) lib utillinux python3 nixpkgs-fmt;
  inherit (plutus-apps) haskell;

  # Feed cardano-wallet, cardano-cli & cardano-node to our shell.
  # This is stable as it doesn't mix dependencies with this code-base;
  # the fetched binaries are the "standard" builds that people test.
  # This should be fast as it mostly fetches Hydra caches without building much.
  cardano-wallet = import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-wallet";
      rev = "f7bc922150473a260578966fec12a4dedf32eb85";
      sha256 = "sha256-O4m6+6kWZl+ANunVbvkHIte6WXaC8m4ji8kqk3kei3s=";
    })
    { };
  cardano-node = import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      # A standard release compatible with the cardano-wallet commit above is always preferred.
      rev = "65422ff373f2f88a563afa746a9a16d211ffdc7c";
      sha256 = "0rvwf2nmfn8yad0004qfp1racygriqrd7fd0z2wgbjgh18dfnin2";
    })
    { };

  # local build inputs ( -> ./nix/pkgs/default.nix )
  localInputs = (with plutus-apps; [
    cardano-node.cardano-node
    cardano-wallet.cardano-wallet
    plutus-chain-index
    pab-cli
  ]);

in
haskell.project.shellFor {
  nativeBuildInputs = localInputs;

  # We don't currently use this, and it's a pain to materialize, and otherwise
  # costs a fair bit of eval time.
  withHoogle = false;
}
