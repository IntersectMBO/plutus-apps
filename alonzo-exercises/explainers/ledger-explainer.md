## Ledger explainer ##

Cardano, like any other blockchain, is a distributed ledger or database that records all transactions and blocks created on the chain. This database shares records across all participants and synchronizes continuously with blockchain activities to provide transparent and up-to-date information for anyone to access. 

[Cardano DB Sync](https://docs.cardano.org/cardano-components/cardano-db-sync) fetches such blockchain records and allows users to [query transaction and block details using CLI commands](https://docs.cardano.org/explore-cardano/cardano-architecture/working-with-db-sync). For more convenient and user-friendly data exploration, you can use the [Cardano Explorer](https://explorer.cardano.org/en.html) – a graphical user interface that presents details in a simple way.

A block producer validates each transaction before it is [submitted to a block](https://docs.cardano.org/new-to-cardano/cardano-nodes). The sender must have sufficient funds, so there is no double-spending, and all the nodes across the ledger must reach consensus.

Let’s take a closer look at how this works within the Shelley ledger and how Plutus scripts will change this process to support multi-asset transactions and smart contracts. 

### Transaction validation using Shelley native scripts ###

Cardano operates based on the unspent transaction output (UTXO) accounting model. This process means it uses transaction inputs and outputs as records to track fund transfers, ownership, and balances. Users’ funds are stored as unspent transaction outputs, each of which has a quantity that can be spent. Inputs are unspent outputs from previous transactions. As soon as an output is used as input in a transaction, it becomes spent and can never be used again. The output is specified by:

- **an address**: that contains a payment credential and an optional stake credential, either a public/verification key hash or a script hash. Technically, the stake credential can also be a pointer to the registration certificate.
- **a value**: reflects the actual ada amount that can be spent. 

A transaction must be signed by the owner of the private key (also referred to as the signing key), which corresponds to the payment credential included in the address. 

Cardano Shelley supported ada transactions solely. However, the Shelley formal specification introduced the concept of [multi-signature (multisig) scripts](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md#multi-signature-scripts), which, native by their nature, are captured entirely by ledger rules. This multisig scheme allows an unspent transaction output to be used as an input to a new transaction if a predefined combination of signatures is provided. For example, if two individuals have to sign the transaction simultaneously, two out of three keys have to be provided, and so on.
