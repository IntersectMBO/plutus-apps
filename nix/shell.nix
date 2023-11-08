{ repoRoot, inputs, pkgs, lib, system }:

cabalProject:

let

  cardano-cli = cabalProject.hsPkgs.cardano-cli.components.exes.cardano-cli;
  cardano-node = cabalProject.hsPkgs.cardano-node.components.exes.cardano-node;
  cardano-wallet = cabalProject.hsPkgs.cardano-wallet.components.exes.cardano-wallet;

in

{
  name = "plutus-apps";

  packages = [
    cardano-cli
    cardano-node
    cardano-wallet

    pkgs.sqlite-interactive
    pkgs.wget
    pkgs.yq
    pkgs.jq
    pkgs.z3
    pkgs.gnused
    pkgs.awscli2
    pkgs.bzip2
    pkgs.zlib
    pkgs.cacert
    pkgs.dateutils
    pkgs.act
    pkgs.hub
  ];

  env = {
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
  };

  preCommit = {
    shellcheck.enable = false;
    stylish-haskell.enable = true;
    nixpkgs-fmt.enable = true;
    cabal-fmt.enable = true;
    optipng.enable = true;
  };
}
