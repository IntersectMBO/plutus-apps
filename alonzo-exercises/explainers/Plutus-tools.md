# Plutus tools

The Alonzo hard fork brings core smart contract capabilities to the Cardano ledger by establishing the infrastructure and adding tools for functional smart contract development using Plutus.This means that users, developers, and organizations can now securely build decentralized applications (DApps) based on smart contract solutions. 

There are different tools that developers can use to evaluate and deploy smart contracts on Cardano. Below we take a look at these tools, their main features and point to the exact sources to use them. 

## Plutus Playground ## 
The [Plutus Playground](https://playground.plutus.iohkdev.io/) provides an environment for writing and testing smart contracts before they are released on the Cardano blockchain. It is a lightweight, web-based environment for exploratory Plutus development. As well as providing a web-based simulator for writing and executing smart contracts, the Plutus Playground also allows developers to access popular smart contracts that have already been written. [Tutorials](https://playground.plutus.iohkdev.io/tutorial/tutorials/index.html) are available within the Plutus Playground to help you get started.

The Plutus Playground can be accessed through a web browser and there is no need to install software. The interface is split into three sections:

- editor
- simulation
- transactions

The simulator shows how a contract will behave on the Cardano blockchain. An important aspect of this is that it can act as a training tool for people who do not have advanced developer skills because it demonstrates the working principles. Users can define and modify the wallets that interact with a contract, as well as the actions that affect the outcome. The results can then be evaluated to see what happens on the blockchain and how transactions occur. For more information, watch the [Plutus application compiling and testing tutorial](https://www.youtube.com/watch?v=DhRS-JvoCw8) or visit the [Plutus GitHub repository](https://github.com/input-output-hk/plutus).

## Plutus Application Backend ##
The Plutus Application Backend (PAB) is a key component of the Plutus Application Framework (PAF) that will enable developers to interact with smart contracts. 

The purpose of the PAB is to:

- provide a standardized environment for Plutus applications to run in
- provide disciplined state management
- present discoverable interfaces to the external clients
- track on-chain information for smart contract uses
- work in an emulated environment.

The PAB is an off-chain, backend service for managing and handling the requirements of the application instance throughout its lifecycle. This includes interaction with external clients (such as wallet frontends) and acts as an intermediary between Plutus Applications, the node, the wallet backend, and end-users. Such interaction is made possible by PAB commands and mock components that enable convenient simulations and integration of DApps.

We will be sharing links in due course when this is available.

## Plutus fee estimator ##
The Plutus fee estimator has been developed by IOG performance experts as an in-house tool for price benchmarking and comparison. It uses information from real-world Plutus transactions to predict the fees that will be charged for a transaction. The estimator can be used to calculate fees for actual transactions (e.g., to determine the fees that will be charged if the network parameters change), and also to estimate fees for individual script transactions or complete DApps before or during development. It may be useful to determine the effect of script changes or optimizations on fees.

Fee calculation requires three pieces of information:
- **The total on-chain transaction size in bytes**: a simple transaction, for example, is around 300 bytes, the one with metadata is around 650 bytes, and Plutus scripts are typically 4,000-8,000 bytes (future optimizations will reduce this).
- **The number of computational (CPU) steps** that the script uses: each step represents 1 picosecond of execution time on a benchmark machine. Typical scripts should consume less than 1,000,000,000 (1 millisecond).
- **The number of memory units** that the script uses: this represents the number of bytes that the script allocates. Typical scripts should consume less than 1,000,000 memory units (1MB of memory allocation).
- To use an estimator, a user just needs to fill in relative information, which can be obtained from the Plutus compiler after building a script in it. There is also no need to run a node for this, which significantly simplifies the process for general users. 

We will be sharing links in due course when this is available.
