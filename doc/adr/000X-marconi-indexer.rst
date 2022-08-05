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

+ Current solution may cause data corruption in case of long rollbacks

Decision
--------

Events
^^^^^^

Accumulators
^^^^^^^^^^^^

Queries
^^^^^^^

API Design
^^^^^^^^^^

Rollbacks
^^^^^^^^^

Argument
--------

+ There is no real reason why we would not want to support this

Alternatives
------------

+ Included in the design.

+ Less queries should be faster.

Implications
------------


