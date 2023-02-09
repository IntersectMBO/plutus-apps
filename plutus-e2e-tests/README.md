# plutus-e2e-tests

End-to-end tests using [cardano-testnet](https://github.com/input-output-hk/cardano-node/tree/master/cardano-testnet) to configure and start a local Cardano testnet and [cardano-api](https://github.com/input-output-hk/cardano-node/tree/master/cardano-api) to build transactions and query ledger state. These tests focus on functionality involving plutus scripts.

## Status

This framework is still in early stages of development with only a handful of tests covering:
- Using plutus builtin functions `verifySchnorrSecp256k1Signature` and `verifyEcdsaSecp256k1Signature` in different protocol versions with different expected outcomes (success or particular errors).
- Spending funds locked at script using reference script, reference inputs and providing datum as witness in txbody.
- Minting tokens using reference script and providing script witness in txbody.

There are plans to add the following features:
- Shared instance of `cardano-testnet` for all tests targeting a common protocol version, e.g. Babbage PV8. This should make overall execution time shorter.
- Some tests to also be run on a public testnet, e.g. `preview` or `pre-prod`.
- Test reporting, e.g. [tasty-html](https://hackage.haskell.org/package/tasty-html) or [Allure](https://qameta.io/allure-report/).
- Nightly CI test execution.