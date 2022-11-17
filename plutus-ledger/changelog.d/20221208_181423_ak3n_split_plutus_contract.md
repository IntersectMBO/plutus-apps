### Removed

- Moved to `cardano-node-emulator` package:
  - `Ledger.TimeSlot` to `Cardano.Node.Emulator.TimeSlot`
  - `Ledger.Params` to `Cardano.Node.Emulator.Params`
  - `Ledger.Generators` to `Cardano.Node.Emulator.Generators`
  - `Ledger.Fee` to `Cardano.Node.Emulator.Fee`
  - `Ledger.Validation` to `Cardano.Node.Emulator.Validation`
  - `Wallet.Emulator.Chain` to `Cardano.Node.Emulator.Chain`

### Changed

- Moved to `Ledger.Tx.CardanoAPI`:
  - `Ledger.Validation.getRequiredSigners`
  - `Ledger.Validation.fromPlutusIndex`
  - `Ledger.Validation.fromPlutusTxOut`
  - `Ledger.Validation.fromPlutusTxOutRef`