.. _hearing_hysterical_screams:

ADR 18: Indexers query synchronisation primitive
================================================

Date: 2023-03-08

Authors
-------

Ometita Radu - Adrian <radu.ometita@iohk.io>

Status
------

Draft

Context
-------

Running two indexers in parallel creates the implicit assumption that both indexers are answering queries over the same original (blockchain) data. That is not generally true due to the different speeds at which indexers process blocks.

This has been observed first when the PAB needed to index some blockchain information and a smart contract would query both the PAB and the Chain Index. It would often be the case that their views were not synchronised, requiring the developer to synchronise the two views, which is not a trivial thing to do, given the current APIs.

Decision
--------

Provide a very simple primitive that would allow users to easily synchronise queries across several indexers. We choose this primitive to be a simple notification system based on the `contra-tracer` package.

A generic notification data type wraps user-defined notifications. The generic data type contains information about blockchain control operations, providing information about the latest block that has been ingested or (alternatively) if any rollbacks occurred.

A user may listen to these notifications and be informed about the slot numbers that have been processed by all the running indexers, thus giving him the information required to choose an optimal slot number where to run the queries.

Argument
--------

This change will enable the further development of several interesting functionalities. Some examples follow:

A. Indexer tracker component -> keeps track of synchronisation status for all indexers, can be queried and can select the optimal slot number for running queries. This component will allow indexers to run in parallel with no need to synchronise on a block number making indexing more effective. Gives the user the option to build extensive logic with regards to synchronisation levels amongst indexers (for example: start processing requests if indexers are synchronised within 10 blocks, or they are at least 10 blocks to the head, etc.)

B. Asynchronous queries -> the notification system (the user-defined part) can be used to answer queries when some external conditions are satisfied (for example: notify me when slot number X has been processed, or when something changes at address Y)

C. Integration with the `iohk-monitoring` framework or some similar solution:

  * Enables plugging into any logging infrastructure for indexers.
  * We can leverage all the functionality present in the `iohk-monitoring` package, including support for performance testing

D. Easily adaptable to various tracer backends:
  
  * We can provide basic tracers for a number of backends, and the reliance on `contra-tracer` will make adapting any kind of message to the pre-defined backends possible, maybe even easy.

E. Implement similar functionality to what the PAB has (asynchronous queries will help with that), but with the transparent rolling back of contract states.

We also simplify a lot of the current API by dropping everything related to query intervals. (A) or some restricted form thereof will make those obsolete. I would suggest forcing a `Point` argument into every query that signifies the slot at which we want to run the query. One alternative could be to use a `Maybe Point`, Nothing signifying to query at the top of the chain. I think that would be a mistake since it introduces non-deterministic queries. Making them deterministic would require the results to contain information regarding the point at which they ran, which would complicate the API and increase the risk of users simply using the simpler API disregarding the risks that they expose themselves to.

Another good effect that this change will have, connected to the disappearing of query intervals, is that we only query information at a single point by default, which makes simple queries faster (both in querying the data, but also serialisation and transfer) and also lowers resource consumption. The user can choose to return multiple results if the query requires them, but is no longer forced to do that for queries that require only one.
