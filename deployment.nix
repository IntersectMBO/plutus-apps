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
      rev = "a5085acbd2670c24251cf8d76a4e83c77a2679ba";
      sha256 = "1apzfy7qdgf6l0lb3icqz3rvaq2w3a53xq6wvhqnbfi8i7cacy03";
    })
    { };
  cardano-node = import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      # A standard release compatible with the cardano-wallet commit above is always preferred.
      rev = "1.33.0";
      sha256 = "1hr00wqzmcyc3x0kp2hyw78rfmimf6z4zd4vv85b9zv3nqbjgrik";
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
