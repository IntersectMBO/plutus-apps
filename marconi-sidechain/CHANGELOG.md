
<a id='changelog-1.2.0'></a>
# 1.2.0 — 2023-03-03

## Removed

- Removed the ToJSON/FromJSON instances of the UTXO indexer types (moved to `marconi-chain-index`)

- Remove the `--utxo-db` CLI parameter in favor of `--utxo-db-fname`

## Added

- Added a CLI param (`--db-dir`) for specifiying the directory in which to store the SQLite database files.
- Added a CLI param (`--utxo-db-fname`) for specifying the database file name of the UTXO indexer.

- Added golden tests for the CLI parameter, PLT-1513

## Changed

- Renamed the module names of `marconi-mamba` to reflect the package name. For example, instead of
  `Marconi.CLI`, we now have `Marconi.Mamba.CLI`.

- Change the method names of the JSON-RPC endpoint:
  - utxo-report -> getUtxoFromAddress
  - target-addresses -> getTargetAddresses

- Providing target addresses in the CLI through `--addresses-to-index` is now optional

## Fixed

- Fixed the resuming capability of the UTXO indexer

- Fixed lint errors and warnings

- update the client/server examples for the recent changes in the UTXO payload and query interface

- Changed CLI params to resemble those of marconi-sidechain, PLT-1561

## Add

- add json-rpc test to test the payload serialization through RPC routes
