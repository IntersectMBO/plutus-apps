< < <<<<<< Updated upstream:plutus-use-cases/default.nix
{ pkgs, haskell }:
let
puc-scripts-invoker = haskell.packages.plutus-use-cases.components.exes.plutus-use-cases-scripts;
========
{ inputs, pkgs, ... }:

let
puc-scripts-invoker = inputs.self.cabalProject.hsPkgs.plutus-use-cases.components.exes.plutus-use-cases-scripts;
>>>>>>>> Stashed changes:nix/src/generated-puc-scripts-output.nix

generated-puc-scripts-output = pkgs.runCommand "plutus-use-cases-scripts-output" { } ''
    mkdir -p $out/scripts
    mkdir -p $out/transactions
    ${puc-scripts-invoker}/bin/plutus-use-cases-scripts $out/scripts scripts
    ${puc-scripts-invoker}/bin/plutus-use-cases-scripts $out/scripts scripts -u
    # Mainnet address is used because no networkid is specified (with the '-n' flag)
<<<<<<<< Updated upstream:plutus-use-cases/default.nix
    ${puc-scripts-invoker}/bin/plutus-use-cases-scripts $out/transactions transactions -p ${haskell.packages.plutus-use-cases.src}/scripts/protocol-parameters.json
========
    ${puc-scripts-invoker}/bin/plutus-use-cases-scripts $out/transactions transactions -p ${inputs.self.cabalProject.hsPkgs.plutus-use-cases.src}/scripts/protocol-parameters.json
>>>>>>>> Stashed changes:nix/src/generated-puc-scripts-output.nix
    tar zcf $out/all-outputs.tar.gz -C $out scripts transactions
  '';

in

generated-puc-scripts-output
