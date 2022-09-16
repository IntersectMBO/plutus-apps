.. _plutus_tools_component_descriptions:

Plutus tools component descriptions
=================================================

The Plutus team has provided a set of “Tools”---components, libraries and 
executables---for external developers to use. They are located in the 
`plutus-apps <https://github.com/input-output-hk/plutus-apps>`_ repository. 

For each tool listed below, we have indicated its specific location within 
plutus-apps, provided a short description, indicated its dependencies, and given 
a brief statement about its state of development to give you some context. 

1. Plutus script utils
--------------------------

`Plutus script utils <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-script-utils>`_ 
is a utility library for helping users write Plutus scripts that are to be used on-chain. 
Currently, this is a “kitchen sink” library containing code that was moved out of 
plutus-ledger-api. 

It provides: 

   * hashing functions for Datums, Redeemers and Plutus scripts for any Plutus language version. 
   * functionality for wrapping the untyped Plutus script with a typed interface. 
   * utility functions for working with the ScriptContext of a Plutus Script. 

+--------------+---------------------------------------------------------------+
| Plutus Script Utils                                                          |
+==============+===============================================================+
| Location     | ``plutus-apps/plutus-script-utils``                           |
+--------------+---------------------------------------------------------------+
| Dependencies | cardano-api, plutus-ledger-api                                |
+--------------+---------------------------------------------------------------+
| State        | Work in progress with an unstable interface.                  |
+--------------+---------------------------------------------------------------+

2. Marconi
--------------------------

`Marconi <https://github.com/input-output-hk/plutus-apps/tree/main/marconi>`_ is an 
application for indexing useful data from the Cardano blockchain in a scalable way. 

+--------------+---------------------------------------------------------------+
| Marconi                                                                      |
+==============+===============================================================+
| Location     | ``plutus-apps/marconi``                                       |
+--------------+---------------------------------------------------------------+
| Dependencies | cardano-api, plutus-ledger-api                                |
+--------------+---------------------------------------------------------------+
| State        | In a very early alpha version. Has only a few indexers and    |
|              | the final architecture is currently being designed.           |
+--------------+---------------------------------------------------------------+

3. Plutus ledger
--------------------------

`Plutus ledger <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-ledger>`_ 
is a "kitchen sink" set of components containing data types and functions that 
complement `cardano-ledger <https://github.com/input-output-hk/cardano-ledger>`_ 
related to Plutus. 

+--------------+---------------------------------------------------------------+
| Plutus ledger                                                                |
+==============+===============================================================+
| Location     | ``plutus-apps/plutus-ledger``                                 |
+--------------+---------------------------------------------------------------+
| Dependencies | cardano-api, cardano-ledger, plutus-ledger-api,               |
|              | plutus-script-utils                                           |
+--------------+---------------------------------------------------------------+
| State        | Unstable interface with an uncertain future.                  |
+--------------+---------------------------------------------------------------+

4. Plutus ledger constraints
---------------------------------

`Plutus ledger constraints <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-ledger-constraints>`_ 
contains a constraints-based API that can be used to generate on-chain validation 
functions and to build transactions by providing a list of constraints. The main 
design is to be able to use the same constraints on-chain and off-chain in a plutus 
application. The off-chain part generates transactions based on types in plutus-ledger. 

For example:

   * ``checkScriptContext (MustSpendAtLeast 10Ada, MustProduceOutput myOutput, …)``
   * ``mkTx (MustSpendAtLeast 10Ada, MustProduceOutput myOutput, …)``

+--------------+---------------------------------------------------------------+
| Plutus ledger constraints                                                    |
+==============+===============================================================+
| Location     | ``plutus-apps/plutus-ledger-constraints``                     |
+--------------+---------------------------------------------------------------+
| Dependencies | plutus-ledger                                                 |
+--------------+---------------------------------------------------------------+
| State        | In the process of being deprecated in favor of                |
|              | plutus-tx-constraints in order to build transactions using    |
|              | cardano-api instead of plutus-ledger.                         |
+--------------+---------------------------------------------------------------+

5. Plutus Tx constraints
---------------------------------

`Plutus-tx-constraints <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-tx-constraints>`_ 
contains a constraints-based API that can be used to generate on-chain validation 
functions and to build transactions by providing a list of constraints. The main 
design is to be able to use the same constraints on-chain and off-chain in a Plutus 
application. The off-chain part generates transactions based on types in cardano-api. 

+--------------+---------------------------------------------------------------+
| Plutus Tx constraints                                                        |
+==============+===============================================================+
| Location     | ``plutus-apps/plutus-tx-constraints``                         |
+--------------+---------------------------------------------------------------+
| Dependencies | plutus-ledger-api, plutus-script-utils, cardano-api           |
+--------------+---------------------------------------------------------------+
| State        | Work in progress to support all the functionality from        | 
|              | plutus-ledger-constraints.                                    |
+--------------+---------------------------------------------------------------+

6. Plutus chain index
---------------------------------

`Plutus chain index <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-chain-index-core>`_ 
is an application for indexing useful data from the Cardano blockchain that is to be 
used in the Contract Monad. The main design is for indexing information related to the 
UTxO set in order to keep the amount of indexed information proportional to the 
UTxO set. The component is not meant to index historical information, contrary to 
the purpose of cardano-db-sync. 

+--------------+---------------------------------------------------------------+
| Plutus chain index                                                           |
+==============+===============================================================+
| Location     | ``plutus-apps/plutus-chain-index-core``                       |
|              | ``plutus-apps/plutus-chain-index``                            |
+--------------+---------------------------------------------------------------+
| Dependencies | cardano-api, plutus-ledger-api, plutus-ledger,                |
|              | plutus-script-utils                                           |
+--------------+---------------------------------------------------------------+
| State        | In the process of being deprecated in favor of Marconi.       | 
+--------------+---------------------------------------------------------------+

7. Contract monad (aka contract API, plutus contract)
-----------------------------------------------------------

`Contract monad <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-contract>`_ 
is a library for writing Plutus contracts. It provides the Contract Monad interface 
for writing the off-chain part of a Plutus application that is to be interpreted 
by an emulator or a plutus application backend (PAB). 

+--------------+---------------------------------------------------------------+
| Contract monad                                                               |
+==============+===============================================================+
| Location     | ``plutus-apps/plutus-contract``                               |
+--------------+---------------------------------------------------------------+
| Dependencies | cardano-api, plutus-ledger-api, plutus-ledger,                |
|              | plutus-ledger-constraints, plutus-chain-index-core            |
+--------------+---------------------------------------------------------------+
| State        | Maintained but not extended. Unstable interface.              | 
+--------------+---------------------------------------------------------------+

8. Contract monad emulator
-----------------------------------------------------------

`Contract monad emulator <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-contract>`_ 
is a library that provides an environment for emulating the blockchain. The environment 
provides a way for writing traces for the contract which are sequences of actions 
by simulated wallets that use the contract. The component is highly dependent on the 
Contract Monad. However, there are plans to decouple it from the Contract Monad 
so that it can be used with other libraries.

+--------------+---------------------------------------------------------------+
| Contract monad emulator                                                      |
+==============+===============================================================+
| Location     | ``plutus-apps/plutus-contract``                               |
+--------------+---------------------------------------------------------------+
| Dependencies | cardano-api, plutus-ledger-api, plutus-ledger,                |
|              | plutus-ledger-constraints, plutus-chain-index-core,           |
|              | plutus-contract                                               |
+--------------+---------------------------------------------------------------+
| State        | Maintained and will be extended. Unstable interface.          | 
+--------------+---------------------------------------------------------------+

9. Plutus contract state machine
-----------------------------------------------------------

`Plutus contract state machine <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-contract>`_ 
is a library that provides a high-level model for writing a Plutus application based 
on the State Machine formalism. Even though it is inefficient, it can be used to 
model a Plutus application and to test the actual implementation.

+--------------+---------------------------------------------------------------+
| Plutus contract state machine                                                |
+==============+===============================================================+
| Location     | ``plutus-apps/plutus-contract``                               |
+--------------+---------------------------------------------------------------+
| Dependencies | plutus-contract                                               |
+--------------+---------------------------------------------------------------+
| State        | We maintain it, but it is not production ready. It is         |
|              | inefficient---it creates Plutus applications that have high   |
|              | Plutus script sizes; it is not scalable.                      | 
+--------------+---------------------------------------------------------------+

10. Plutus contract testing model
------------------------------------------

`Plutus contract testing model <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-contract>`_ 
is used for testing Plutus contracts with *contract models*, using the framework 
provided by :hsmod:`Plutus.Contract.Test.ContractModel`. This framework generates
and runs tests on the Plutus emulator, where each test may involve a number of 
emulated wallets, each running a collection of Plutus contracts, all submitting 
transactions to an emulated blockchain. Once you have defined a suitable model, 
then QuickCheck can generate and run many thousands of scenarios, taking the 
application through a wide variety of states, and checking that it behaves correctly 
in each one. 

   See also: :doc:`Testing Plutus Contracts with Contract Models <../tutorials/contract-models>`. 

+--------------+---------------------------------------------------------------+
| Plutus contract testing model                                                |
+==============+===============================================================+
| Location     | ``plutus-apps/plutus-contract``                               |
+--------------+---------------------------------------------------------------+
| Dependencies | plutus-ledger, plutus-ledger-api, plutus-ledger-constraints,  |
|              | plutus-contract                                               |
+--------------+---------------------------------------------------------------+
| State        | Maintained and worked on. Developed by QuviQ.                 | 
+--------------+---------------------------------------------------------------+

11. Plutus use case examples
------------------------------------------

`Plutus use case examples <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-use-cases>`_ 
contains hand-written examples for the use cases we currently have. The primary 
examples are: 

   * Auction, 
   * Crowdfunding, 
   * Game, 
   * GameStateMachine, and 
   * Vesting. 

For each plutus application use case, we provide test scenarios (or test cases) 
with and without the :hsmod:`Plutus.Contract.Test.ContractModel`.

+--------------+---------------------------------------------------------------+
| Plutus use case examples                                                     |
+==============+===============================================================+
| Location     | ``plutus-apps/plutus-use-cases``                              |
+--------------+---------------------------------------------------------------+
| Dependencies | plutus-ledger, plutus-ledger-api, plutus-ledger-constraints,  |
|              | plutus-contract                                               |
+--------------+---------------------------------------------------------------+
| State        | Maintained. The examples are used as test cases.              |
|              | While they work in the plutus-contract emulator, they         |
|              | are not guaranteed to work on the actual Cardano network.     | 
+--------------+---------------------------------------------------------------+

12. PAB (Plutus application backend)
------------------------------------------

`PAB <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-pab>`_ 
is a web server library for managing the state of Plutus contract instances. 
The PAB executes the off-chain component of Plutus applications. It manages 
application requests to the wallet backend, the Cardano node and the chain-index. 
PAB stores the application state and offers an HTTP REST API for managing application 
instances. 

PAB wraps the contracts built with plutus-contract. It is the central point of 
contact for everything, integrating all the Cardano components. 

+--------------+---------------------------------------------------------------+
| PAB                                                                          |
+==============+===============================================================+
| Location     | ``plutus-apps/plutus-pab``                                    |
+--------------+---------------------------------------------------------------+
| Dependencies | cardano-api, plutus-ledger-api, plutus-ledger,                |
|              | plutus-contract, plutus-contract emulator,                    |
|              | plutus-chain-index-core                                       |
+--------------+---------------------------------------------------------------+
| State        | Maintenance mode for bugs, but not a high priority component. |
|              | We are not adding any new features unless they are            |
|              | contributed by external stakeholders.                         | 
+--------------+---------------------------------------------------------------+

13. PAB simulator 
---------------------------

`PAB simulator <https://github.com/input-output-hk/plutus-apps/tree/main/plutus-pab>`_ 
is a simulator for the PAB that is used for testing a plutus application in a 
simulated environment. 

+--------------+---------------------------------------------------------------+
| PAB simulator                                                                |
+==============+===============================================================+
| Location     | ``plutus-apps/plutus-pab``                                    |
+--------------+---------------------------------------------------------------+
| Dependencies | cardano-api, plutus-ledger-api, plutus-ledger,                |
|              | plutus-contract, plutus-contract emulator,                    |
|              | plutus-chain-index-core, plutus-pab                           |
+--------------+---------------------------------------------------------------+
| State        | Maintenance mode for bugs. Not a high priority component.     | 
+--------------+---------------------------------------------------------------+

14. Plutus application template (aka Plutus platform starter project)
---------------------------------------------------------------------------

`Plutus application template <https://github.com/input-output-hk/plutus-starter>`_ 
is a Haskell template offering a simple starter project to assist dApp 
developers in starting to write Plutus applications. 

+--------------+---------------------------------------------------------------+
| Plutus application template                                                  |
+==============+===============================================================+
| Location     | ``https://github.com/input-output-hk/plutus-starter``         |
+--------------+---------------------------------------------------------------+
| Dependencies | Everything in plutus-apps.                                    |
+--------------+---------------------------------------------------------------+
| State        | Not maintained for the time being. It will be reworked        |
|              | as we continue to refine best practices for writing Plutus    |
|              | applications.                                                 | 
+--------------+---------------------------------------------------------------+
