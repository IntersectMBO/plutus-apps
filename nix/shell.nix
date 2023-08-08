# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#34-nixshellnix

{ inputs', pkgs, ... }:

let

  cardano-cli = inputs'.self.cardano-node.legacyPackages.cardano-cli;
  cardano-node = inputs'.self.cardano-node.legacyPackages.cardano-node;
  cardano-wallet = inputs'.self.cardano-wallet.legacyPackages.cardano-wallet;

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
    pkgs.ghcid
    pkgs.gnused
    pkgs.awscli2
    pkgs.bzip2
    pkgs.zlib
    pkgs.cacert
    pkgs.dateutils
    pkgs.act
  ];

  env = {
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
  };
}
