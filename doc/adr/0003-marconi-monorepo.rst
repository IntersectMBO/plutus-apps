.. _marconi_monorepo:

ADR 3: Move Marconi into a separate repository
==============================================

Date: 2022-06-08

Authors
---------

Lorenzo Calegari <lorenzo.calegari@iohk.io>

Status
------

Draft

Context
-------

Marconi is a Haskell executable that lives in `plutus-apps/plutus-chain-index`.

It is desirable to move it into a separate repository for the following reasons:

* Better visibility and easier to discover.
* It wants to depend on a version of `cardano-api` different to the one in 
  `plutus-apps`.
* It seems to "make sense" to warrant its own repository, as it is a fairly 
  independent component.

However, creating a separate repository would be rather costly.
It would involve a great deal of duplication, due to the way our current 
nix code is structured, not to mention the added complexity and overhead 
inherent in maintaining a separate codebase.

Decision
--------

* Marconi will be kept in `plutus-apps` for the time being.

Implications
------------

* A nix flake will be added in `plutus-apps` so that users will be able 
  to obtain the executable trivially.
* The possibility to specify a separate version of `cardano-api` just for 
  marconi **whist staying in plutus-apps** will be explored.
  Note that the dependency on a specific version `cardano-api` won't be a 
  problem until the next hard fork.
* As a very low priority task, a new repository *will* be created for marconi,
  which will use `std` from the start 
  (see :ref:`Repository Standardization <repository_standardization>`)



