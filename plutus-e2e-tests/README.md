# plutus-e2e-tests

End-to-end tests using [cardano-testnet](https://github.com/input-output-hk/cardano-node/tree/master/cardano-testnet) to configure and start a local Cardano testnet and [cardano-api](https://github.com/input-output-hk/cardano-node/tree/master/cardano-api) to build transactions and query ledger state. These tests focus on functionality involving plutus scripts.

## Status

This framework is still in development with a handful of tests covering:
- Using plutus builtin functions `verifySchnorrSecp256k1Signature` and `verifyEcdsaSecp256k1Signature` in different protocol versions with different expected outcomes (success or particular errors).
- Spending funds locked at script using reference script, reference inputs and providing datum as witness in txbody.
- Minting tokens using reference script and providing script witness in txbody.
- Check each field in Plutus V1 and V2 TxInfo.

Tests can be run on either a private or public testnet. They are grouped by target protocol version (E.g. Babbage PV8) and share an instance of private testnet during execution. `cardano-testnet` is used to manage private testnets.

To run these tests on a public testnet, see preconditions:
- Have a directory containing at least these two directories:
  - "utxo-keys" containing "test.skey" and "test.vkey" files. These must both be text envelope PaymentKey format, currently no support for PaymentExtendedKey.
  - "ipc" containing the active node.socket
- Cardano node is fully synced on a public network (e.g. preview testnet)
- There is at least one ada-only UTxO at the test account. Each test will spend a few ada so make sure there's enough funds.
- Modify the test options passed to each test to be run from `TestnetOptions` to `LocalNodeOptions`. E.g. swap `testnetOptionsBabbage8` for `localNodeOptionsPreview`.
- Ensure the `LocalNodeOption`'s `localEnvDir` points to the directory containing your keys ("utxo-keys") and node socket ("ipc").

There are plans to add the following features:
- Test reporting, e.g. [tasty-html](https://hackage.haskell.org/package/tasty-html) or [Allure](https://qameta.io/allure-report/).
- Nightly CI test execution.