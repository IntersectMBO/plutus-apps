{ inputs, cell }:

inputs.cardano-node.packages.${cell.library.pkgs.system}
