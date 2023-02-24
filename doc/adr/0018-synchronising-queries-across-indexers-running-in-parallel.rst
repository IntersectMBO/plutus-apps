
.. synchronising_queries_across_indexers_running_in_parallel:

ADR 18: Synchronising queries across indexers running in parallel
=================================================================

Date: 2023-02-24

Authors
-------

Ometita Radu - Adrian <radu.ometita@iohk.io>

Status
------

Draft

Context
-------

Running two indexers in parallel creates the implicit assumption that both indexers are answering queries over the same original (blockchain) data. That is not generally true due to the different speeds at which indexers process blocks.

This has been observed first when the PAB needed to index some blockchain information and a smart contract would query both the PAB and the Chain Index. It would often be the case that their views were not synchronised, requiring the developer to synchronise the two views, which is not a trivial thing to do.


Decision
--------

1. Change the `Query` type to include a preferred slot number and how many results we want to receive. If there is no preferred slot number, the top available slot number is assumed.
2. Change the `Response` type to include all the requested query results encoded as the full query result, followed by a set of deltas (events).
3. [Optional] Add a new function to the `Queryable` type class that witnesses the folding of Events into the query result.

Example:

Give a request for slot number 10, with a maximum of 5 results, some of the indexers might output:

Indexer 01: State at slot 6, Delta for slots 7, 8, 9 and 10 (if the indexer is synchronized up to or past slot 10 and has at least 5 more deltas stored as events)
Indexer 02: State at slot 6, Delta for slots 7 and 8 (if the indexer is only synchronised up to slot 8)
Indexer 03: State at slot 8, Delta for slots 9 and 10 (if the indexer is synchronised up to or past slot 10, but only has events for slots 9 and 10 stored in the database)
Indexer 04: No result (the indexer is not synchronised to slot 6)
Best result (defined as the oldest available slot or all indexers), if calculated for indexers 1, 2 and 3 would be the data at slot 8. If we include the 4th indexer, then there is no result available as the 4th indexer is still synchronising.

Argument
--------

A fairly simple solution to the synchronisation problem would be to qualify responses by the current slot number. This is a reasonable first thought, but it does have a high probability of returning results that are qualified by different slot numbers (if the indexers are fully synchronised, it would be a small offset, but still dependent on the various indexers processing speeds). The advantage of this solution is that at least, we now know that the indexers were not fully synchronised. If the application does not require them to be, then this is a reasonable solution.

If, however, the application requires all queries to run at the same slot number, this solution will simply not function. What we could do is request multiple query responses from indexers in the hope that we can find a common slot number where all indexers have an answer. Is this hope realistic? I would argue that it is realistic for indexers that have been fully synchronised with the blockchain since the processed slot offset between them should be fairly small.

Now that will solve our functional problem, but it may introduce some non-functional ones. First, the query results may be fairly big, and returning more of them will put pressure on the network and the parsing of data by the client applications. This can be partially mitigated by selecting a smaller interval, but maybe we can help out here by improving the design a bit. One observation is that we don't actually need to send the full query results. We need to send a query result for the oldest slot number available, but for the newer ones, we only need to send the difference recorded by the next slot. This implementation maps very well to the way we store information in the indexer (an accumulator and a set of events).

I think this solution is better than what we currently have because it gives a principled way of composing queries over a set of indexers that run at different speeds. This allows us to run them in parallel on the same processor (as we do now) or distributed. It allows for efficient use of resources since we don't need to use any locking. The implementation is also simple since we don't share any data across threads and we don't need to deal with thead synchronisation issues.

Implications
------------

However, this solution also complicates the life of our users. Now, they need to also have some function that allows them to fold these differences into the older query responses. I would suggest a couple of strategies that we could employ to simplify their lives, that are not the object of this ADR:

1. Write a set of property-based tests that validate that the folding function is correct and instruct our users to verify their implementation using our acceptance tests.
2. Develop a new software service that can synchronise queries across several indexers. This service would use our implementation of the folding function (which is verified by our tests) and after receiving all the query responses will select the best ones and provide them to the users already folded.

The only thing remaining is to note that if we want to protect against rollbacks we will also need to store the block hash together with the slot number and at this point our design is complete.

Alternatives
------------

Currently, the way we almost avoid that situation is to run a set of indexers in parallel by waiting on all of them to finish processing the current block before advancing to the next one. This is a fairly big improvement, but essentially it still has the same problem, except that it will not be triggered as often. One scenario when this could happen is if the query comes while the indexers are about to process a block. Some of these indexers may answer the query before block processing, and some after the block has been processed. This can be mitigated by a query-locking mechanism that spans all the indexers, but this adds complexity while not managing to solve the root problem.

This solution also forgoes the possibility of splitting up the indexers into different processes or machines. Our customers complained about the excessive resource consumption, performance and the inability to easily add their own indexers to the chain index. Giving users the possibility of distributing their indexers will partially address the first concern by splitting resource consumption on several machines (which is also addressed at the indexer design level by an inherently scalable design) and indirectly the second (since indexers are distributable, they have a simple, standard API that allows any customer to roll their own version, implicitly solving the hard problems like rollbacks and synchronisation).

Another alternative, one that assumes that we have indexer notifications implemented would be to have the client listen to block processed notifications from indexers and automatically compute the optimal slot at which a query can be run. In this case we would mitigate the need to have several results returned and no other real downside.
