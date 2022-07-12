.. _common_pab_api:

ADR 4: Common PAB API
=====================

Date: 2022-07-12

Authors
---------

Gergely Szabo <gergely@mlabs.city>

Status
------

Draft

Context
-------

There are multiple implementations of a Plutus backend, and also other tools related to Plutus smart contracts. Some of them are using the same contract interface as the official implementation, but some of them use a different interface. However, as the ecosystem evolves, it would be beneficial to create a well defined standard, that other off-chain tools can use as a reference, or as an interface to implement.

Currently, as we are getting close to the Vasil hardfork, testing tools and Plutus backend tools are at a hurry to update their dependencies and get to a Vasil compliant/compatible state. However, tools that are depending on `plutus-apps` are blocked by the PAB development. This initiative was born out of this context, but could solve other problems as well.

Benefits
--------

The idea proposed in this document could speed up the development of off-chain tools, by loosening up some of tightly coupled dependencies, so these projects can move more freely. This would also mean that the cost of the interface update would be reduced, so we could see more features added to the standard, and the PAB API following the capabilities of Cardano more closely. As an added benefit, community involvement with the API could also greatly improve.

A standard API for all Plutus contacts would help keeping the ecosystem on the same track with their implementation. As more and more off-chain tools implement the same contract interface in the future, it will be relatively easy to switch between different PAB implementations, or to use multiple of these tools at the same time without a need for serious code rewrites.

Proposal
--------

The PAB Contract (defined in `plutus-apps`) is using the `freer-simple` effect system to define all the contract effects. This already allows us to separate the interface from the implementation, or to have multiple implementations for one interface.

Our proposal, is to further separate these two components: create a separate repository with these contract effects and types, but without any implementation or any unnecessary dependencies. By moving the contracts out of the plutus-apps monorepository, any tool could update to newer version to their discretion. Without many dependencies, many tools could utilize the contract API without having to depend on the whole plutus-apps monorepo.

The current implementation of Contract effects are tightly coupled to the PAB and chain-index implementations, so we also need to make changes in order to get a more general API, but this could be done in a further iteration.

Architectural decisions
-----------------------
TBD

