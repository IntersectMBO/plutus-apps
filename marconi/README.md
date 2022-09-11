
This is an initial draft for the Marconi README file. Going forward, we will need to discuss and refine this document and/or the implementation so that they match.

# Marconi

## The Cardano blockchain indexer for dApp developers

A lightweight customizable solution for indexing and querying the Cardano blockchain.

### Built by IOG in Haskell

* dApp developers can index whatever is important to them.
* Marconi is an indexing solution for developers who need to store on-chain data in a local database.

## Introduction

Marconi is a blockchain query solution that you can configure for the types of information that you need to index and according to how you need to structure your database schema.

Marconi consists of these three components:

| Component | Description |
| --------- | ----------- |
| Streamer  | Provides event stream of configured data types |
| Index     | Indexes and stores the data in a database |
| Query     | Fetches information from the database |

The streamer attaches to a node, providing an event stream of the configured data types. Your applications can use streams directly. Or streams may be passed to the index component which indexes and stores the data in a database. You can use the query component to fetch information from the database.

## Design Principles

### Scalability through Customized Solutions for Each Use Case

The philosophy behind Marconi is to maintain scalability by customizing the solution for each use case. You can filter the streams of information to transmit only the data you need for your dApp, minimizing network traffic and database storage. You can also customize database schemas and queries so that the dApps have the most efficient and scalable application-specific API to interact with the data.

### Handling Blockchain Rollback Events Properly

Marconi is designed to enable dApps to handle blockchain rollback events properly. Since the head of the chain is constantly advancing and rolling back as the consensus algorithm advances the chain, this causes challenging synchronization issues for dApps that need rapid updates of the chain state.

Marconi's index component handles rollbacks by distinguishing volatile and immutable blocks. A block on the top of the chain is volatile since it is subject to some probability of being rolled back. Eventually, when the block is deeper than the security parameter, the block becomes immutable. The Marconi indexer tracks this status and keeps the volatile blocks in memory until they become immutable. Once immutable, Marconi persists the blocks to the database.

### Specifying Slots to Query

Through the query interface, you can specify a range of slots to query or indicate that the query is for blocks deeper than a specified number of slots. As a result, you can use simple state management for applications that work only with immutable blocks.

### Querying Volatile Data

If your application requires faster response times, you can query volatile data on the head of the chain; however, you must take care to invalidate cached data when rollbacks occur.

## API Layers

The core Marconi APIs are defined in Haskell in order to support simple use cases where all the components run together in a single process.

If your use cases require multiple synchronized instances for load balancing or for supporting different indexers, Marconi is designed with alternative transport layers on top of the core API that support network streaming and RPC calls.

## What Differentiates Marconi

* Developed by IOG
* Built in Haskell

### DB Sync

Cardano DB-sync is IOG's current indexing solution, but it uses lots of memory and days/week to sync. DB-sync takes an index everything approach to put all information into a database that can support any query at the cost of requiring a large DB server to run it.

In contrast, Marconi requires the user to customize it for the specific application so that only relevant information is streamed or stored. Marconi will be a scalable solution that allows the dApp developer to index whatever is important for their unique use case.

### Ogmios

Ogmios exposes the basic node interface as a web service. This is a great option for queries that can be made directly to the node for applications that want a web service interface.

### Oura

Oura, implemented in Rust, functions as a notification system that indexers provide. It has good connectivity with cloud infrastructure like Kafka. Marconi provides similar functionality implemented in Haskell to make it easy for Haskell programmers to customize it.

### Scrolls

Scrolls is the storage and query solution that our indexers provide. Like Scrolls, we can be selective for what is to be indexed. Scrolls can store into multiple databases. Marconi is currently focused on local DBs like SQL lite.

### Carp

## Customizing Marconi

You can customize indexing in the following ways:

1. *This statement is a placeholder:* A function that is given events and current state outputs notifications.

2. Customize how a function performs queries by determining the following aspects of the function:

   * The query type. Because Marconi uses an Abstract Data Type (ADT), you need to define the types of queries that the indexer responds to.
   * Query for certain slot numbers by determining the point in the in-memory history where you want to run the query.
   * Weigh considerations for in-memory and on-disk data. The query function produces a result by merging the in-memory and on-disk data. If you are concerned only with on-disk data, then you can query the database directly.

3. Customize a function that stores buffered data. While there is no connection to any storage mechanism, there is a nice API is available, and you are encouraged to change it to suit your needs. Changing it is very simple.

Marconi uses the `streaming` library so you can make use of its many combinators.

## Documentation

When it exists, we can link to our readthedocs user documentation.

* User Guide
* Reference Guide
* Tutorial
* Example Code

## Intended Use Cases

Description of its primary intended use cases.

* Sync with the Cardano blockchain (private/public testnet or mainnet) by reading all blocks from the genesis block to the current tip
* Index the syncing information based on the user’s predefined indexing
* Query the indexed information based on the user’s predefined queries

### Use Case 1

### Use Case 2

### Use Case 3

## Example Queries

Can we provide example queries?

## Architecture

* Provide a compelling diagram

## Architecture Decision Records

* Records of decisions that were made by the team.
* Discuss alternatives.
* Why did we choose one design over another?
   * Konstantinos has some for Plutus Apps.
   * Radu has some for Marconi.

## System Requirements

## How to Install and Configure

### Installation Procedures

The `marconi` executable is available as a nix flake.

You may either clone the [`plutus-apps`](https://github.com/input-output-hk/plutus-apps)
repository and run from the top-level:
```
nix build .#marconi
```
Or you may run from anywhere:
```
nix build github:input-output-hk/plutus-apps#marconi
```

Both commands will produce a `result` directory containing the executable
`result/bin/marconi`.

### Configuration Procedures

## How to Contribute

How do you contribute to it? What tools, methods, processes are required?

* Contributing documentation.
* Most of the info in Plutus-Apps and Plutus should also be included in Marconi.

## Making Builds

How do you build it?

### Build Procedures

## Storage Considerations and Accessing Data

# Troubleshooting

# FAQ

# Further Reading

