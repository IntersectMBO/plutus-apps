![Marconi-Logo_sm](https://user-images.githubusercontent.com/104791413/178748103-077ed7e0-6071-461f-b9ed-97b3b7bd5b24.png)

# Marconi Alpha Release

## The Cardano blockchain indexer for dApp developers

Marconi is a lightweight, customizable chain follower application and library for dApp developers who need to index and query the Cardano blockchain.

## Introduction

Marconi is a blockchain query solution that you can configure for the types of information that you need to index and according to how you need to structure your database schema. Marconi provides a way for Plutus off-chain code to have access to data that has already been stored on-chain by indexing portions of the blockchain. 

Marconi gives you the ability to customize the set of data that is indexed. It provides a library of functionality with which you can do the following:
* build your own indexers, 
* work only with the data that you care about for your application, 
* use whatever storage engine you prefer, and 
* support only the queries that you need to support.

### Scalability through Customized Solutions for Each Use Case

The philosophy behind Marconi is to maintain scalability by customizing the solution for each use case. You can filter the streams of information to transmit only the data you need for your dApp, minimizing network traffic and database storage. You can also customize database schemas and queries so that the dApps have the most efficient and scalable application-specific API to interact with the data.

### Indexing solution ### 

Any indexing solution must deal with blockchain rollbacks, which can happen as the network achieves consensus. Marconi is designed to deal with rollbacks as elegantly as possible while simultaneously finding an effective balance between memory, disk and CPU usage. 

Managing rollbacks is very simple and fast. Marconi drops the events that were rolled back. No complicated logic is required to undo the projection of the list of events on disk, which Marconi would need if it stored everything on disk as quickly as possible. 

Making `K` configurable makes the design quite scalable. Developers do not usually need to guard themselves against rollbacks by `K` blocks, so they can choose to store only 10 events in memory, for example, thereby sacrificing the speed by which Marconi handles rollbacks (since rollbacks on data that is persisted on disk is more expensive than data that is in memory). 

In the event of a restart, recovery is very simple. Marconi stores only fully confirmed transactions, so there is nothing to do other than to load the state persisted on disk and to follow the blockchain from the corresponding chain-point. 

## Queries

The indexed data is accessible through queries. There are no constraints on the format of queries or on the results. Both are identified by a type variable that the indexer exposes. The user, such as a dApp developer, provides the code for both. 

This is currently the only area where you can customize the indexer by providing functions that handle queries and storage. Note that the queries have to run on both the in-memory data and the data that is stored.

# Installation

Prerequisite: You already have a Cardano node running. 

## Example command for launching Cardano node 

```
cardano-node run \
    --config mainnet-config.json \
    --topology mainnet-topology.json \
    --database-path db \
    --socket-path /tmp/node-mainnet.sock \
    --port 3003
```

## Marconi executable

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

## Example command for launching Marconi

```
./result/bin/marconi --socket-path /tmp/node-mainnet.sock --mainnet --datum-db datum.sqlite3 --utxo-db utxo.sqlite3
```
