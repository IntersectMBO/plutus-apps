
<a id='changelog-1.2.0'></a>
# 1.2.0 — 2023-03-03

## Removed

- Removed dependency on playground-common and everything that depended on it.
- Plutus.PAB.Effects.Contract: removed exportSchema and endpointsToSchemas

## Added

- The `Cardano.Wallet.LocalClient.ExportTx` module (previously
  `Plutus.Contract.Wallet` in `plutus-contract`) was added.

- Added a lot of missing `OpenApi.ToSchema` instances.

## Changed

- `plutus-ledger-constraints` was replaced with `plutus-tx-constraints`.
