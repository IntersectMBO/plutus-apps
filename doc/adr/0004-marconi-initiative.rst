=========================
Making a case for Marconi
=========================

Plutus off-chain code oftentimes needs access to indexed portions of the blockchain. The Chain Index project is the initial solution meant to deliver access to this kind of data. However, after release, a couple of shortcomings were identified which prompted the development of an indexing solution that is based on a different set of architectural and functional constraints.

A lot of the shortcomings are connected to the exploratory type of development that we used to deliver the Chain Index which was prompted by the lack of a clear specification and a lack of concern for non-functional and quality assurance requirements. The top-down design resulted in a monolithic and fairly complex architecture which made the code difficult to reuse, compose and understand.

Some of the problems we identified due to the above-mentioned approaches are:

  A. The use of free algebras (the `freer-simple` package) makes the code fairly complex and difficult to understand (quite a few type-level computations are happening). The separation between syntax and semantics imposed by the library also complicates matters for no clear reason (for example, if we write two semantics, one for pure code used for testing and one for production code, then there would be a lot of production code that would not be tested). 
  
  B. The monolithic architecture lacks the features necessary to customise the indexed set of data, the Chain Index provides only all-or-nothing indexing. While this can be addressed the monolithic architecture makes it an uphill battle. 
  
  C. Also due to the monolithic architecture that assumes that there is only one index running we ran into trouble when we made the Plutus Application Backend collect and index information requested by smart contracts. Now we have two components that index information from the blockchain, but they are not synchronised. Querying the Chain Index about transactions received from the Plutus Application Backend often results in no data returned, since the Chain Index indexes data slower than the PAB.
  
  D. The lack of non-functional requirements resulted in software that uses an unreasonable amount of resources and results in slow synchronisation speeds. And since everything is monolithic it is difficult to turn off indexing of data which is not required by our customers there is no way to limit the required resources.
  
  E. The same lack of a specification and non-functional requirements makes the testing feel ad-hoc and like an afterthought.

The Chain Index was meant to be a software application that supports the execution of smart contracts. And, in that, it succeeded. However, we found out that our customers would rather have a library of functionality that they can customise to build their indexers, only for data they care about for their application, using whatever storage engine that they prefer and supporting only the queries that they need to support.

So when we took all the feedback into account we decided that a redesign of the indexing solution using a much simplified and modular design is a worthwhile enterprise.

We continue by introducing some of the design principles that guided us in the specification of Marconi.

============================
Design principles of Marconi
============================

I follow the Algebra Driven Design approach for Marconi components, so from the get-go, we will have a checked specification for the software that we develop.

The specification is based on a simplified model which should help with documenting how everything works without getting into the more complex details.

Having a set of property-based tests to validate that the implementation conforms to the specification also means that the correctness of the implementation does not rely on type-level checks or complicated term-level machinery (we could even verify the correctness of a Rust implementation by leveraging the Rust to Haskell FFI).

Because we have no reliance on type-level checks or complicated architectural patterns to validate the software (we use the specification and property tests for that), the code is much easier to understand, document and extend.

=================
Indexing solution
=================

The indexing solution has the following basic requirements: it needs to deal with rollbacks as elegantly as possible and provide a way to compromise between memory, disk and CPU usage.

On the Cardano blockchain, there are frequent rollbacks, but they can only span a maximum of 2160 blocks (and most of them are < 10 blocks). We call the 2160 number the security parameter K (and we denote it by 'K' henceforth).

Indexers are a store which is updated by events created from each block. The problem introduced by rollbacks is that we need to undo all state changes when a rollback occurs.

We opted for a design where we keep K blocks in memory as the list of events that are fed into the function that merges them into the persisted state and persist them once they go beyond the K limit.

This architectural decision has some desirable effects:

  1. Managing rollbacks is very simple and fast. We drop the events that were rolled back. (no need to undo the application of blocks on the state stored on disk, which would be necessary if we were to store everything on disk as fast as possible).
  
  2. Making 'K' configurable makes the design already quite scalable. Developers do not usually need to guard themselves against rollbacks by K blocks so they can choose to only store 10 events in memory thus sacrificing chain integrity for RAM.
  
  3. In case of a restart recovery is very simple. We only store fully confirmed transactions so there is nothing to do other than resume operation.

And some less desirable effects:

  1. We must keep K events in memory, which (depending on how large events are) can waste some memory. Our educated guess is that this is a reasonable compromise, but depending on how large events can get that may not be the case for your use case.
  
  2. Queries are more complicated as we need to query both events in memory and the state persisted on disk.

=================
Query and storage
=================

The indexed data is accessible through queries. There are no constraints on the format of queries or results. Both are identified by a type variable that the indexer exposes and the code for both of them has to be provided by the user.

This is currently the only area where the users can customise the indexer by providing functions that handle queries and storage. Note that the queries have to run on both the in-memory data and the data that is stored.

This allows us to associate any kind of storage type to the indexers, though, right now we are only using SQLite.

==================
Versioned contents
==================

We need some sort of versioning or sequencing of events, notifications and queries.

  1. Synchronisation of multiple indexers (queries have a validity interval)
  2. Resume functionality (we need to know from which slot to resume)
  3. Handling of rollbacks (now there is explicit handling of rollbacks)

More information will become available in the next few sprints.

=============
Event streams
=============

To support PAB functionality which subscribes to a source for a set of event types we need a way to produce events from indexers.

They are also very useful for contracts that want to track rollbacks. Rollbacks are invisible from the point of view of the indexed data, but it may be the case that the internal state of a contract needs to know that the state has been reverted.

============
Similar work
============

One of the advantages that Marconi has over Oura and Scrolls is the fact that they are both a streaming solution and an indexer. So you can listen to a stream of events and know that those events are reflected in the index.

