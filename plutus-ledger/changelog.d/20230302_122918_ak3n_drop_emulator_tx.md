### Removed

- Remove `unspentOutputsTx` and `spentOutputs`.
- Remove `cardanoApiTx`, `emulatorTx`, `onCardanoTx`, `cardanoTxMap`, `addSignature`, `addSignature'`, `txOutRefs`, `unspentOutputsTx`, `txId`.
- Remove `CardanoTx(EmulatorTx, CardanoApiTx)`.
- Remove `toCardanoTxBody`, `toCardanoTxBodyContent`, `toCardanoTxInWitness`, `toCardanoMintValue`.
- Remove `Tx` and `TxStripped` types and all related functions.

### Changed

- Renamed `SomeCardanoApiTx(SomeTx)` to `CardanoTx(CardanoTx`.
- Renamed `CardanoApiEmulatorEraTx` to `CardanoEmulatorEraTx`.