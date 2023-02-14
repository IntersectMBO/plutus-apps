{ inputs, cell }:

let
  plutus-apps = cell.library.plutus-apps-project;

  puc-scripts-invoker = plutus-apps.hsPkgs.plutus-use-cases.components.exes.plutus-use-cases-scripts;

  generated-puc-scripts-output = cell.library.pkgs.runCommand "plutus-use-cases-scripts-output" { } ''
    mkdir -p $out/scripts
    mkdir -p $out/transactions
    ${puc-scripts-invoker}/bin/plutus-use-cases-scripts $out/scripts scripts
    ${puc-scripts-invoker}/bin/plutus-use-cases-scripts $out/scripts scripts -u
    # Mainnet address is used because no networkid is specified (with the '-n' flag)
    ${puc-scripts-invoker}/bin/plutus-use-cases-scripts $out/transactions transactions -p ${plutus-apps.hsPkgs.plutus-use-cases.src}/scripts/protocol-parameters.json
    tar zcf $out/all-outputs.tar.gz -C $out scripts transactions
  '';

in

generated-puc-scripts-output
