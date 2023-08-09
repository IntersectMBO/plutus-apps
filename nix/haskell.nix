# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#32-nixhaskellnix

{ system, ... }:

{
  supportedCompilers = [ "ghc8107" ];


  enableCrossCompilation = true;


  defaultChangelogPackages = [ ];

  # Because of lack of resources in our darwin CI cluster, the "combined-plutus-apps-haddock" job
  # often gets timed out, which of course makes the required darwin CI checks fail. In the end, we
  # just remove it from the required darwin jobs, but still keep it in the linux jobs.
  enableCombinedHaddock = system == "x86_64-linux";


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
