
<a id='changelog-1.2.0'></a>
# 1.2.0 — 2023-03-03

## Added

- Epoch stakepool size indexer; the data indexed is roughly equivalent to the `epoch_stake` table in cardano-db-sync.

- Mint/burn indexer: indexes mint/burn events of custom tokens in transactions.

- Address-Datum indexer: indexes all datums for any given address in Cardano.

- CLI flag to allow disabling the Address-Datum indexer.

- Added ToJSON/FromJSON instances for the UTXO indexer types like `UtxoRow` and `Utxo`

- Bootstrapped a benchmark suite for evaluating performance of `marconi-chain-index`

- Added a simple benchmark for evaluating query performance for the address with the most UTXOs.

- Adding 'PRAGMA journal_mode=WAL' for all indexers that are opening a SQLite database

- Added golden tests for the CLI parameter, PLT-1513

## Changed

- Renamed the `marconi` package name to `marconi-chain-index`
- Renamed the module names of the `jsonrpc` lib from Marconi.JsonRpc` to `Network.JsonRpc`

- `UtxoRow` doesn't store the `BlockNo` anymore

## Fixed

- Fixed the resuming of the UTXO indexer
- Fixed the UtxoEvent generator to generate unique utxos and only spend utxos which have been
  previously created.

- Fixed lint errors and warnings

- minor changes, mainly adding `show` instances for debugging purposes

* Fixed a bug in the datum extraction in the Utxo indexer
