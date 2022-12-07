.. _end_to_end_testing_strategy:

ADR 16: End-to-end testing strategy for plutus, cardano-ledger-api and cardano-node-client
==========================================================================================

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
automation and exploratory approaches. Both are performed on a Cardano testnet with a real node.
Automation test scenarios for Plutus are currently being run as part of the wider `cardano-node-tests
<https://github.com/input-output-hk/cardano-node-tests/>`_ test suite, which is 
driven by a ``cardano-cli`` Python wrapper. Those tests focus on general ledger/node/cli functionality
and only cover a few key scenarios for core `plutus <https://github.com/input-output-hk/plutus/>`_
functionality, such as TxInfo and SECP256k1 builtins.

This document outlines an **additional** approach to end-to-end test automation using a framework
written in Haskell. Arguments for and against the creation and maintainance of a new test framework
alongside ``cardano-node-tests`` have been made below along with suggestions of which features and
types of tests are best suited for each framework.

The exploratory testing approach is not in the scope of this document.

Decision
--------

The primary aim is to satisfy all of ``plutus`` (core) end-to-end testing requirements, although, this
is an oportunity to also get coverage of other packages being developed such as `cardano-testnet`,
``cardano-ledger-api`` and ``cardano-node-client``. An end-to-end testing framework written in Haskell
to be created in a new repository. It will use `cardano-testnet
<https://github.com/input-output-hk/cardano-node/tree/master/cardano-testnet/>`_
to initialise a local network environment, ``cardano-ledger-api`` for building and balancing
transactions, and ``cardando-node-client`` to submit balanced transactions and for querying the ledger
state to make test assertions.

Although ``cardano-ledger-api`` is under test it isn't feasible to expect thorough coverage of all
node functionality, such as staking and update proposals, because the primary focus is to satisify
end-to-end testing requirements for ``plutus``. Fortunately, much of the node and ledger functionality
is already covered by ``cardano-node-tests``.

Initially, a few tests that do not depend on `plutus-apps
<https://github.com/input-output-hk/plutus-apps/>`_  will be created. This entails building
transactions with ``cardano-ledger-api`` and waiting for on-chain events using ``cardano-node-client``
without use of the constraints library. This approach allows us to build transactions specifically,
which is especially useful for testing edge-cases and error scenarios. Although, it will likely
require more boilerplate code and negatively impact readabity.

We may decide early on to depend on ``plutus-apps`` to use the Contract monad, which would give a
uniform interface for off-chain code such as different node backends (private and public testnets,
and emulator) and chain-indexer queryies (``cardano-node-client`` or Marconi in future). It should
also reduce the amount of boilerplate code and provide additional features such as trace logging.

If we wish to use the ``cardano-node-emulator``, which is a faster node without consensus, for
property based testing that requires faster test execution then we'll need to depend on ``plutus-apps``.
Alternatively, those types of tests would exist only in ``plutus-apps``. 

There is the option of including additional packages to test from ``plutus-apps`` at a later stage.

Types of tests for the Haskell framework
----------------------------------------
All ``plutus`` end-to-end testing requirements will be covered by the Haskell framework. Arguments for
and against this approach are covered in the "Argument" section below. In summary, with access to
the Haskell and Plutus interfaces and reduced friction from using a single programming language we
are likely improve test coverage at this level. E.g. builtin functions and error scenarios.

Although we will be building out the end-to-end test covereage, it is more efficient to have fewer
and broader test scenarios at this level and a greater number of tests at the lower unit and
integration levels for stressing particular features and covering negative scenarios and edge cases.

Examples of types of tests to be included
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Any Plutus Core builtin function. These may already be tested extensively in the lower 
  unit/property/intgration levels but there's value in having some coverage at the end-to-end
  level too.
* Use cases that go beyond testing features in isolation. Bringing together various functionality
  helps demonstrate the capability of more realistic plutus applications.
* Latest Plutus version, which would likely introduce new changes to the cardano ledger. Perhaps
  ``TxInfo`` is extended to accomodate the updated transaction body. It'd be possible to implement 
  tests earlier in the development cycle with this framework than waiting for ``cardano-cli`` to
  implement the functionality to cover in ``cardano-node-tests`` or depend on repetative manual
  approach for regression testing.

Types of tests for cardano-node-tests Python framework
------------------------------------------------------
It would still bring value to have some coverage in ``cardano-node-tests`` for plutus transactions.
This will mainly cover specific cli flags, e.g. reference inputs and custom-typed script data
in cli's json format. It is also important to test the cli's error handling with script evaluation.

Examples of types of tests to be included
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Cli flags that require use of plutus scripts E.g. tx-out-reference-script-file orcalculate-plutus-script-cost
* Checking cli behaviour when script evaluation passes. This could be displaying expected fee correctly.
* Checking cli behaviour when script evaluation fails. This can be how different types errors are formatted. 

Argument
--------

Pros of building and maintaining our own test framework
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Plutus tools team will have full ownership of the end-to-end test environment and its priorities
  for ``plutus``.
* Plutus scripts can be defined alongside the tests. ``cardano-node-tests`` requires pre-compiled scripts.
* Tests will demonstrate how these Haskell packages can be used together to guide plutus application
  development using the node apis. Particularly useful for less experienced haskell developers.  
* Possible to define tests once and run at different levels. For example, on private or public
  testnets and with ``cardano-node-emulator`` emulated node.
* Benefits from use of all ``plutus`` apis. For example, using PlutusTx to produce scripts using a
  typed interface, and optionally the Contract monad from ``plutus-apps``.
* Have the oportunity to add more componenets under test at a later stage, such as Marconi or a PAB.
* ``cardano-cli`` would not be a dependency for plutus test coverage so no risk of being blocked by its
  stage of development.
* Plutus team can implement and review majority of tests in Haskell rather than Python, which is
  likely to be the team's preference. Also won't need to review as many tests in ``cardano-node-tests``.
* Less friction caused by cross-team: planning, dependencies and expectations. Plutus team won't
  need to wait for node test team to implement the tests. It's likely that other node/cli features
  will often be prioritised.
* This approach will will improve our high-level perspective of each componenent and help guide
  UX improvements.
* Node team are not pressured to focus on plutus scenarios, they retain control of their priorities.

Cons of building and maintaining our own test framework
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* ``cardano-node-tests`` is well established and already has useful features such as: running tests in
  different eras, transitioning between eras, reporting, and measuring deviations in script cost.
* We are proposing to add another means of spinning up a local testnet when there are already
  multiple others (see notes section below).
* No major work needed to continue using ``cardano-node-tests``. Node test team will continue to
  upgrade/maintain it regardless.
* Some coverage of plutus tests already exist and can continue to be added in
  ``cardano-node-tests`` by the node qa and plutus teams.
* Going forwards, many plutus tests will be mostly a copy/paste job. For example, the existing
  SECP256k1 tests build and submit a single transaction using a basic minting policy and a redeemer
  consisting of inputs to the verify function. This approach will be similar to future tests of
  builtin functions.
* Plutus team will still be required to support the node test team with defining and reviewing a
  subset of plutus tests in ``cardano-node-tests``.
* Node test team may grow, less delays in getting plutus tests implementd by a Python developer.
* ``cardano-cli`` is a wrapper for ``cardano-api`` (soon to be ``cardano-ledger-api`` and
  ``cardano-node-client``), so that package already getting test coverage at the end-to-end level.

Additional Considerations
-------------------------
* Business stakeholders will want to see test results so think about producing and storing a report.
  It would be nice open source this along with the tests.
* At first tests will be run on private testnet but we must consider how these tests can also be run
  on a public testnet. For example, initial wallet balances and utxos will need to be handled
  dynamically because we'd only have control over these in the private testnet.
* Seeing as ``cardano-ledger-api`` and ``cardano-node-client`` are still in early stages of production
  it would make sense not to block creation of this test framework. We can begin using ``cardano-api``
  and switch over when ready.
* End-to-end tests can be slow to execute and as the suite grows we may want to run a subset at more
  frequent intervals. For example, we run tests for the latest Plutus version nightly but older
  tests/versions are run weekly, or for tags/release only.

Alternatives
------------

Other than choosing to remain using ``cardano-node-test``, we could look at the approach teams such as
Djed or Hydra are using to see if we can share infrastructure. I don't think either are using
``cardano-testnet``.

Instead of creating a new repository it is possible the end-to-end tests could live in ``plutus-apps``.
Although, because the componenets under test span other repositories it would be restrictive and
additional work at the time when dependencies are updated in ``plutus-apps``.

Notes
-----

This ADR document should be moved out of ``plutus-apps``` and into the new end-to-end test repository
once created.

Benchmarking hasn't been covered above because we already have a team dedicated to testing cardano-node
performance that incldues some plutus scripts. It is an automated approach using ``cardano-cli``.

Other places spinning up a local testnet
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* https://github.com/woofpool/cardano-private-testnet-setup
* https://github.com/input-output-hk/mithril/mithril-test-lab
* https://github.com/input-output-hk/hydra/hydra-cluster
* https://github.com/input-output-hk/cardano-node/tree/master/scripts/byron-to-alonzo
* https://github.com/input-output-hk/cardano-js-sdk/tree/master/packages/e2e/local-network
* https://github.com/input-output-hk/cardano-wallet/blob/master/lib/wallet/exe/local-cluster.hs
* https://github.com/mlabs-haskell/plutip