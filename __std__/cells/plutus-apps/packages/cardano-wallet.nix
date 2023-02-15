{ inputs, cell }:

let

  cardano-wallet-compat = import inputs.flake-compat {
    inherit (cell.library) pkgs;
    src = builtins.fetchTree {
      type = "github";
      owner = "input-output-hk";
      repo = "cardano-wallet";
      rev = "18a931648550246695c790578d4a55ee2f10463e";
      narHash = "sha256-3Rnj/g3KLzOW5YSieqsUa9IF1Td22Eskk5KuVsOFgEQ=";
    };
  };

  cardano-wallet = cardano-wallet-compat.defaultNix.packages.${cell.library.pkgs.system}.cardano-wallet;

  # FIXME Both cardano-node and cardano-wallet put a bunch of .dynlib files inside their respective 
  # derivation's /bin folder! So when the devshell merges all the bin folders, it detects a collision
  # for the shared subset of common .dynlib files. It's unclear why the default shell doesn't have 
  # the same behaviour.
  fixed-cardano-wallet = cell.library.pkgs.stdenv.mkDerivation {
    name = "fixed-cardano-wallet";
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      ln -s ${cardano-wallet}/bin/cardano-wallet $out/bin/cardano-wallet
    '';
  };

in
fixed-cardano-wallet
