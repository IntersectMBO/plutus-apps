
<a id='changelog-1.2.0'></a>
# 1.2.0 — 2023-03-03

## Removed

- Remove `UnbalancedEmulatorTx` and `unBalancedTxTx` as the `Tx` was removed from `plutus-ledger`.

## Added

- Added the lacking types and functions from `plutus-ledger-constraints` including following modules:
  - `Ledger.Tx.Constraints.OnChain.V1`
  - `Ledger.Tx.Constraints.OnChain.V2`
  - `Ledger.Tx.Constraints.TxConstraints`

<a id='changelog-1.1.0'></a>
# 1.1.0 — 2023-01-12

## Added

- Add support for the `MustMint` constraint.

- Added `mustProduceAtLeast` and `mustSpendAtLeast` constraints.
- Added `checkValueSpent` in `processLookupsAndConstraints` to validate spend inputs and outputs.
