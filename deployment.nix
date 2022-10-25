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
  cardano-wallet = (import sources.flake-compat {
    inherit pkgs;
    src = builtins.fetchTree
      {
        type = "github";
        owner = "input-output-hk";
        repo = "cardano-wallet";
        rev = "18a931648550246695c790578d4a55ee2f10463e";
        narHash = "sha256-3Rnj/g3KLzOW5YSieqsUa9IF1Td22Eskk5KuVsOFgEQ=";
      };
  }).defaultNix;
  cardano-node = import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      # A standard release compatible with the cardano-wallet commit above is always preferred.
      rev = "1.35.3";
      sha256 = "020fwimsm24yblr1fmnwx240wj8r3x715p89cpjgnnd8axwf32p0";
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
