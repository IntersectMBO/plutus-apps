# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#31-flakenix

{
  description = "Plutus Apps";


  inputs = {
    iogx.url = "github:input-output-hk/iogx";
    iogx.inputs.hackage-nix.follows = "hackage-nix";
    iogx.inputs.CHaP.follows = "CHaP";

    cardano-node.url = "github:input-output-hk/cardano-node/ebc7be471b30e5931b35f9bbc236d21c375b91bb";

    cardano-wallet.url = "github:input-output-hk/cardano-node/18a931648550246695c790578d4a55ee2f10463e";

    hackage-nix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };


  outputs = inputs: inputs.iogx.lib.mkFlake {
    inherit inputs;
    repoRoot = ./.;
    systems = ["x86_64-darwin" "x86_64-linux" "aarch64-darwin"];
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
