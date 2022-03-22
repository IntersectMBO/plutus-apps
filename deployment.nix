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
      rev = "f6d4db733c4e47ee11683c343b440552f59beff7";
      sha256 = "0gb3zyv3q5v5sd8r29s02yc0brwq5a01is9c0n528391n2r8g1yy";
    })
    { };
  cardano-node = import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      # A standard release compatible with the cardano-wallet commit above is always preferred.
      rev = "1.34.1";
      sha256 = "1hh53whcj5y9kw4qpkiza7rmkniz18r493vv4dzl1a8r5fy3b2bv";
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
