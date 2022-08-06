.. _marconi_indexer:

ADR X: Marconi indexers
=======================

Date: 2022-08-05

Authors
-------

Ometita Radu <radu.ometita@iohk.io>

Status
------

Draft

Context
-------

+ We need to be able to index blockchain information.

+ Current solution may cause data corruption in case of long rollbacks.

+ Previous solution used too much RAM.

Decision
--------

+ Split storage between memory and disk.

Events
^^^^^^

+ Event structure
  + Slot numbers
  + Block ids

Queries
^^^^^^^

+ Query validity intervals.

+ Queries are synchronous.

API Design
^^^^^^^^^^

+ Definitions for:
  + Data: Event
  + Data: Query / Response
  + Function: Query
  + Function: Storage
  + Function: Resume

+ Configuration
  + Minimum events retained (K)
  + Maximum in-memory events (N, from N*K - implicit)

Implementation
^^^^^^^^^^^^^^

+ Diagram.

+ Vector data structure as a ring buffer

+ Using stored events for resuming.

+ Merging events into the accumulator.
  + The case for no accumulator => forever queries.
  + When do we merge events (in the indexer or outside of it)

+ Moving events into storage.

+ Type families should make the code easier to read and use
  + Query      -> (Events, Notifications, Response)
  + Connection -> m

Argument
--------

+ There is no real reason why we would not want to support this.

Alternatives
------------

+ Included in the design.

+ Less queries should be faster.

Implications
------------


