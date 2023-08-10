# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#32-nixhaskellnix

{
  supportedCompilers = [ "ghc8107" ];


  enableCrossCompilation = true;


  defaultChangelogPackages = [ ];


  enableCombinedHaddock = true;


  projectPackagesWithHaddock = [
    "plutus-core"
    "plutus-tx"
    "plutus-tx-plugin"
    "plutus-ledger-api"
    "quickcheck-contractmodel"
  ];


  combinedHaddockPrologue = ''
    = Combined documentation for all the public Plutus libraries

    == Handy module entrypoints

      * "Plutus.Contract": Writing Plutus apps (off-chain code).
      * "Ledger.Typed.Scripts": A type-safe interface for spending and
        producing script outputs. Built on "PlutusTx".
      * "Plutus.Trace.Emulator": Testing Plutus contracts in the emulator.
      * "Cardano.Node.Emulator.MTL": Test your transactions on an emulated node.
  '';
}
