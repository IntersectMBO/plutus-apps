# TODO(std) DUP

{ inputs, cell }:

let

  inherit (cell.library.pkgs.haskell-nix) haskellLib;

  hspkgs = cell.library.plutus-apps-project-with-haddock.hsPkgs;

  toHaddock =
    haskellLib.collectComponents' "library" (
      haskellLib.selectProjectPackages hspkgs // {
        inherit (hspkgs) plutus-core plutus-tx plutus-tx-plugin plutus-ledger-api quickcheck-contractmodel;
      }
    );

in

cell.library.combine-haddock {

  ghc = cell.packages.ghc;

  hspkgs = builtins.attrValues toHaddock;

  prologue = cell.library.pkgs.writeTextFile {
    name = "prologue";
    text = ''
      = Combined documentation for all the public Plutus libraries

      == Handy module entrypoints

        * "Plutus.Contract": Writing Plutus apps (off-chain code).
        * "Ledger.Typed.Scripts": A type-safe interface for spending and
          producing script outputs. Built on "PlutusTx".
        * "Plutus.Trace.Emulator": Testing Plutus contracts in the emulator.
    '';
  };
}
