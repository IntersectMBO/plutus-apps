plutus-example.components.exes.create-script-context
marconi-chain-index.components.exes.marconi-chain-index

plutus-pab-executables.components.exes.pab-cli
plutus-chain-index.components.exes.plutus-chain-index
plutus-pab-examples.components.exes.plutus-pab-examples
.plutus-pab-executables.components.exes.plutus-uniswap

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs = {
        hackage.follows = "hackage-nix";
        nixpkgs.follows = "nixpkgs";
      };
    };



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



pkgs.stdenv.mkDerivation {
  name = "haskell-language-server-wrapper";
  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/bin
    echo '#!${pkgs.bash}/bin/bash' > $out/bin/haskell-language-server-wrapper
    echo '${haskell-language-server}/bin/haskell-language-server "$@"' >> $out/bin/haskell-language-server-wrapper
    chmod u+x $out/bin/haskell-language-server-wrapper
  '';
}


