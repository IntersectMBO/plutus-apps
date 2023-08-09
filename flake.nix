# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#31-flakenix

{
  description = "Plutus Apps";


  inputs = {
    iogx.url = "github:input-output-hk/iogx";
  };


  outputs = inputs: inputs.iogx.lib.mkFlake {
    inherit inputs;
    repoRoot = ./.;
  };


  nixConfig = {

    extra-substituters = [
      "https://cache.iog.io"
    ];

    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];

    allow-import-from-derivation = true;

    accept-flake-config = true;
  };
}
