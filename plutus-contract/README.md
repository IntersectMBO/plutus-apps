# plutus-contract

A library for writing Plutus contracts.

The high-level workflow is this:

* Write a contract using the `Plutus.Contract` module. The type of contract is `Contract w s e a` with schema 's', producing a value of type 'a' or an error 'e'.
* (optional) Write traces for the contract using the `Plutus.Trace` module. Traces are sequences of actions by simulated wallets that use the contract. See `Spec.Emulator` for an example.
* (optional) Write unit tests for the contract, using the `Plutus.Contract.Test` module to make assertions about traces.
* Turn the contract into an executable using the `plutus-pab` package.

The `plutus-use-cases` package contains some hand-written examples for the use cases we currently have.
