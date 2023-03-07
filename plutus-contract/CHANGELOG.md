
<a id='changelog-1.2.0'></a>
# 1.2.0 — 2023-03-03

## Removed

- The `Plutus.Contract.Wallet` module has benn moved to the `plutus-pab` package
  under the `Cardano.Wallet.LocalClient.ExportTx`.

- `EmulatorAssertionContractError` and `TxConstraintResolutionContractError`
  were removed from `ContractError`.

- Removed `OpenApi.ToSchema` instance for `PABReq`, `ChainIndexQuery`, `ActiveEndpoint`, `RequestID`, `IterationID`, `Request a`, `ThreadToken`, `Wallet`, `WalletId`, `TxKey`, `SequenceId`, `DereferencedInput`, `BeneficialOwner`, `AnnotatedTx`, `ContractInstanceId`, `ContractActivityStatus`, `EndpointDescription`.

## Added

- A set of functions were added to submit a cardano `Tx` from a a set of
  constraints: `subCardanoTx`, `subCardanoTxConstraints`,
  `subCardanoTxConstraintsPending`, `subCardanoTxConstraintsWith`,
  and `mkCardanoTxConstraints`.

- `walletFundsChangePlutus`: A version of `walletFundsChange` that works with `plutus-core` `Value`s.
- `plutusValueAtAddress`: A version of `valueAtAddress` that works with `plutus-core` `Value`s.

## Changed

- The `getUnspentOutput` function that was in the `Plutus.Contract.Wallet`
  module is now in `Plutus.Contract.Request`.

- Moved to using the `Value` type from `cardano-api` instead of the one from `plutus-core`.

- `plutus-ledger-constraints` was replaced with `plutus-tx-constraints`.

<a id='changelog-1.1.0'></a>
# 1.1.0 — 2023-01-12

## Added

- Added 'Ledger.Value.currencyValueOf' function.

- Added the `Plutus.Trace.Emulator.runEmulatorTraceIOWithConfig` function

- Creation of an 'assertEvaluationError' `Contract.Test` to ease verification of
  invalid transactions.

## Changed

- Changed `Plutus.Trace.Emulator.TraceConfig` data contructor to work with `LogMessage EmulatorEvent`
  instead of `EmulatorEvent'`, added a parameter to configure the `LogLevel` of log messages,
  renamed the field `showEvent` to `traceConfigShowEvent` and renamed the field `outputHandle` to
  `traceConfigOutputHandle`.

- Changed the signature of `Plutus.Trace.Emulator.runEmulatorTrace` by additing a `TraceConfig`
  parameter which is used to filter the log messages by their `LogLevel`, and returns `LogMessage
  EmulatorEvent` instead of `EmulatorEvent'`.

- Changed the signature of `Plutus.Trace.Emulator.evalEmulatorTrace` by additing a `TraceConfig`
  parameter which is used to filter the log messages by their `LogLevel`.

- Change the output format of log messages printed by `Plutus.Trace.Emulator.runEmulatorTrace` by
  changing the `Default` instance of `TraceConfig`.

- Renamed `Plutus.Trace.Emulator.currentSlot` to `Plutus.Trace.Emulator.chainCurrentSlot`.

- We now use `cardano-api` `AddressInEra` (through the alias
  `Ledger.Address.CardanoAddress`) in the emulator instead of `plutus`
  addresses.

- `DoubleSatisfaction` is now using `cardano-api` `Tx` instead of the emulator
  one.

## Deprecated

- Deprecated `Plutus.Trace.Emulator.runEmulatorTraceIO'` in favor of `Plutus.Trace.Emulator.runEmulatorTraceIOWithConfig`

## Security

- Fixed state machine thread token on-chain check in light of https://www.tweag.io/blog/2022-03-25-minswap-lp-vulnerability
