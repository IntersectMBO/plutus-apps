{ inputs, cell }:

let
  cardano-node-compat = import inputs.flake-compat {
    inherit (cell.library) pkgs;
    src = builtins.fetchTree {
      type = "github";
      owner = "input-output-hk";
      repo = "cardano-node";
      rev = "ebc7be471b30e5931b35f9bbc236d21c375b91bb";
      narHash = "sha256-WRRzfpDc+YVmTNbN9LNYY4dS8o21p/6NoKxtcZmoAcg=";
    };
  };
in
cardano-node-compat.defaultNix.packages.${cell.library.pkgs.system}
