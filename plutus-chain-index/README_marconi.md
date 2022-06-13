
This is an initial draft for the Marconi README file. Currently this is a placeholder and a place to begin organizing the documentation. 

# Marconi

## The Cardano blockchain indexer for dApp developers

A lightweight solution for indexing and querying the Cardano blockchain

### Built by IOG in Haskell

* dApp developers can index whatever is important to them. 
* Marconi is an indexing solution for developers who need to store on-chain data in a local database. 

## Introduction

What is the vision for what Marconi will be? What role is it intended to serve in the broader community? What is the full narrative that goes with it? It's true purpose? 

This software component will be a Haskell library used by dApp providers. The component will be designed to allow dApp providers to index in the desired structure the information read from the Cardano blockchain which will be used for the dApp. 

### Documentation

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

*NOTES*

* Multiple indexers 
* UTXO by address
* We have an indexer that queries local DB
* Return my UTXO by address (my Cardano address) 

### Use Case 1

### Use Case 2

### Use Case 3

*NOTES*

A user can specify which ones they want to use for their specific applications. 
Cardano db sync is IOG's current indexing solution, but it uses lots of memory and days/week to sync. Marconi will be a scalable solution. Want to index whatever is important for the dApp developer. Like Scrolls, we can be selective for what is to be indexed. Scrolls can store into multiple databases. We might do that. It is currently focused on local DBs like SQL lite. 

## Example Queries 

Can we provide example queries? 

## Differentiators

Compare to existing tools. 

* How does this tool compare to: 
   * Cardano DB Sync
   * Scrolls
   * Oura
   * Cardano Chain Index?
   * Plutus Chain Index? 
* What makes this tool different from others? 
* Mention of any known trade-offs that went into the design.
* Scrolls uses Rust (developed by 3rd party). 
* Positives, negatives. The known trade-offs. 

### Developed by IOG

### Built in Haskell

## Functional Description

How does it work? 

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

