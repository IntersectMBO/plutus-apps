.. _end_to_end_testing_strategy:

ADR 15: End-to-end testing strategy for plutus, plutus-apps, cardano-api and cardano-testnet
=======================================

Date: 2022-12-02

Author(s)
---------

james-iohk <james.browning@iohk.io>

Status
------

Draft

Context
-------

End-to-end testing of Plutus functionality is curently performed by a combination of
automation and exploratory approaches. Both are performed on a Cardano testnet with a real node and
network. Automation test scenarios for Plutus are being run as part of the wider
[cardano-node-tests](https://github.com/input-output-hk/cardano-node-tests) test suite, which is 
driven by a `cardano-cli`` Python wrapper. These tests focus on general node/cli functionality
and only cover a few key scenarios for core [plutus](https://github.com/input-output-hk/plutus)
functionality, such as TxInfo and SECP256k1 builtins. None of the offchain tooling in
[plutus-apps](https://github.com/input-output-hk/plutus-apps) is being covered.

This document outlines an **additional** approach to end-to-end test automation using a framework
written in Haskell. Arguments for and against the creation and maintainance of a new test framework
alongside [cardano-node-tests](https://github.com/input-output-hk/cardano-node-tests) have been
made below along with suggestions of which features and types of tests are best suited for each
framework.

The exploratory testing approach is not in the scope of this document.

Decision
--------

An end-to-end testing framework written in Haskell to be created in a new repository. It will use
[cardano-testnet](https://github.com/input-output-hk/cardano-node/tree/master/cardano-testnet)
to initialise a local network environment, a combination of plutus-apps and cardano-api for building
transactions to submit, and assertions can be made using the result of node and chain-follower (Marconi)
queries. The aim is to cover all of `plutus` and `plutus-apps` end-to-end testing requirements,
although, this is also an oportunity to get additional coverage of other packages such as
`cardano-testnet`` and `cardano-api`.

Types of tests for the Haskell framework
----------------------------------------
All of Plutus end-to-end testing requirements will be covered in the Haskell framework. Arguments for
and against this are covered in the below section. With less friction and access to Haskell and Plutus
interfaces we are likely to cover more scenarios at this level. E.g. builtin functions, edge cases
and error scenarios. Although, it is more efficient to the wider coverage at unit and integration
levels with fewer and broader scnearios covered at the end-to-end level.

**Examples of feature areas to be included:**
- Any Plutus Core builtin function. These may be tested extensively in the unit/property/intgration
    suites but there's value in having some coverage at the end-to-end level too.
- Use cases. These use a combination of tooling to be closer to realistic plutus applications than 
    typical end-to-end test cases.
- Latest Plutus version, which would likely introduce new changes to the cardano ledger. Perhaps
    `TxInfo` is extended. It'd likely be easier and quicker to cover here than waiting for 
    `cardano-cli` to implement in `cardano-node-tests` or depend on repetative manual approach for
    regression testing.
- A subset of integration tests for the constraint library in `plutus-apps`. More of the edge-cases
    will be covered at lower levels but it is also good to check each one functioning on a real network.
- Using each of the Marconi indexers for asserting for data relating to transactions submitted using
    other componenets such as `cardano-api`.

Types of tests for cardano-node-tests Python framework
------------------------------------------------------
It would still bring value to have some coverage in `cardano-node-tests` for plutus transactions.
This will mainly be to cover specific cli flags, e.g. reference inputs and custom-typed script data
in cli's json format. It is also important to test the cli's error handling with script evaluation.

**Examples of feature areas to be included:**
- Cli flags that require use of plutus scripts E.g. tx-out-reference-script-file or calculate-plutus-script-cost
- Checking cli behaviour when script evaluation passes. This could be displaying expected fee correctly.
- Checking cli behaviour when script evaluation fails. This can be how different types errors are formatted. 

Argument
--------

**Pros** of building and maintaining our own test framework:
- Plutus team will have full ownership of the end-to-end test environment and its priorities.
- Plutus scripts can be defined alongside test definitions. Cardano-node-tests requires pre-compiled scripts
- We want an end-to-end test environment for Marconi and other plutus tooling anyway and
    `cardano-node-tests` cannot be used for that.
- Tests will demonstrate how these Haskell packages can be used together to guide plutus application
    development using our offchain tooling. Particularly useful for less experienced haskell developers.  
- Possible to define tests once and run at different levels. E.g. `plutus-apps` emulated integration
    tests suite at end-to-end level.
- Benefits from use of all plutus/plutus-apps apis. E.g. using PlutusTx to produce scripts,
    typed interface, Contract monad, constraint library. This is not possible with `cardano-node-tests`. 
- `cardano-cli` would not be a dependency for plutus test coverage so no risk of being blocked by that.
- Plutus-tools team is now a major contributor for `cardano-api` so this approach will motivate us to
    promptly support necessery `plutus-ledger` changes for plutus end-to-end test coverage.
- Plutus team can implement and review majority of tests in Haskell rather than Python (likely the
    team's preference). Also won't need to review `cardano-node-tests` much beyond a few sanity tests.
- Less friction caused by cross-team: planning, dependencies and expectations. Plutus team won't need to
    wait for node test team to implement the tests. Likely that other node/cli features will often
    be prioritised.
- This approach will will improve our high-level perspective of each componenent and help guide
    UX improvements.
- Node team are not pressured to focus on plutus scenarios, they retain control of their priorities.

**Cons** of building and maintaining our own test framework:
- `cardano-node-tests` is well established and already has useful features such as: running tests in
    different eras, transitioning between eras, and measuring deviations in script cost.
- We are proposing to add another means of spinning up a local testnet when there are already
    multiple others (see notes section below)
- No major work needed to continue using `cardano-node-tests`. Node test team will continue to
    upgrade/maintain it regardless.
- Some coverage of plutus tests already exist and will continue to be added in
    `cardano-node-tests`, especially for testing cli functionality.
- Going forwards, many plutus tests will be mostly a copy/paste job. E.g. The existing tests for
    SECP256k1 will be very similar to future tests using builtins, just with different script
    and redeemer.
- Plutus team will still be required to support the node test team with defining and reviewing a
    subset of plutus tests in `cardano-node-tests`.
- Node test team may grow, less delays in getting plutus tests implementd by a Python developer
- `cardano-cli` is a wrapper for `cardano-api`, so unlike plutus, that package already getting
    high-level test coverage at the.

Alternatives
------------

Other than choosing to remain using `cardano-node-test`, we could look at the approach teams such as
Djed or Hydra are using to see if we can share infrastructure. I don't think either are using
`cardano-testnet`.

Instead of creating a new repository it is possible the end-to-end tests could live in `plutus-apps`.
Although, because the componenets under test span other repositories it would be restrictive and
additional work at the time when dependencies are updated in `plutus-apps`.

Notes
-----

Benchmarking hasn't been covered above because we already have a team dedicated to testing cardano-node
performance that incldues some plutus scripts. It is an automated approach using `cardano-cli`.

Other places spinning up a local testnet:
https://github.com/woofpool/cardano-private-testnet-setup
https://github.com/input-output-hk/mithril/mithril-test-lab
https://github.com/input-output-hk/hydra/hydra-cluster
https://github.com/input-output-hk/cardano-node/tree/master/scripts/byron-to-alonzo
https://github.com/input-output-hk/cardano-js-sdk/tree/master/packages/e2e/local-network
https://github.com/input-output-hk/cardano-wallet/blob/master/lib/wallet/exe/local-cluster.hs