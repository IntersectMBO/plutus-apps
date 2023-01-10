
<a id='changelog-1.1.0'></a>
# 1.1.0 — 2023-01-10

## Removed

- Moved to `cardano-node-emulator` package:
  - `Ledger.TimeSlot` to `Cardano.Node.Emulator.TimeSlot`
  - `Ledger.Params` to `Cardano.Node.Emulator.Params`
  - `Ledger.Generators` to `Cardano.Node.Emulator.Generators`
  - `Ledger.Fee` to `Cardano.Node.Emulator.Fee`
  - `Ledger.Validation` to `Cardano.Node.Emulator.Validation`
  - `Wallet.Emulator.Chain` to `Cardano.Node.Emulator.Chain`

## Added

- `minAdaTxOut`, computes the minimum amount of Ada required for a `TxOut` more
  precisely, by taking the params and the `TxOut`.

- Added `makeAutoBalancedTransactionWithUtxoProvider` and related functions to `Ledger.Fee`.

- `Ledger.Address.CardanoAddress` an alias to address in the latest era
- `Ledger.Address.cardanoAddressCredential` to retrieve `plutus` credentials
  from a Cardano address
- `Ledger.Address.cardanoStakingCredential` to retrieve `plutus` staking credentials
  from a Cardano address
- `Ledger.Address.cardanoStakingCredential` to retrieve `plutus` `PubKeyHash`
  from a Cardano address
- `Ledger.Address.toPlutusAddress` to get a `plutus` address from a Cardano one
  (it replaces `Ledger.Tx.CardanoAPI.fromCardanoAddressInEra`)

## Changed

- `minAdaTxOut` is now renamed `minAdaTxOutEstimated`.

- Moved `adjustTxOut` into `Ledger.Index`
- Balancing no longer logs if and which inputs and outputs were added.

- Moved to `Ledger.Tx.CardanoAPI`:
  - `Ledger.Validation.getRequiredSigners`
  - `Ledger.Validation.fromPlutusIndex`
  - `Ledger.Validation.fromPlutusTxOut`
  - `Ledger.Validation.fromPlutusTxOutRef`

- `Ledger.Address` now priviledges `cardano-api` addresses instead of `plutus-api` addresses.

## Deprecated

- `fromCardanoAddressInEra`, `fromCardanoAddress`, `fromCardanoPaymentCredential`,
  `fromCardanoPaymentKeyHash`, `fromCardanoScriptHash`,
  `fromCardanoStakeAddressReference`and `fromCardanoStakeCredential` from `Ledger.Tx.CardanoAPI`
  that shouldn't be used now that we use `cardano-api` adress in the emulator.
