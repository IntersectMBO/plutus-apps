# Alonzo Purple Testnet Exercise Sheet 7: "Interacting with Cardano Wallets"

In the previous set of exercises, you built some simple Distributed Applications (DApps) using Putus scripts and the node CLI. In this set of exercises, you will interact with the Cardano Wallet using a different set of CLI commands. The wallet interface provides a more user-centric set of commands that are designed to simplify interaction with wallets, including address manipulation. It is possible to mix node CLI and wallet CLI commands and to link these using Shell commands to build more complex DApps. You will normally be able to inspect the contents of the wallets using the Daedalus, Yoroi etc wallet UIs, but these may not be available immediately.

## Prerequisites

1. Complete [Exercise Sheet 6](6_Alonzo-purple-exercise-6.md)

2. Read the documentation on the Cardano Wallet and CLI at:


[https://github.com/input-output-hk/cardano-wallet](https://github.com/input-output-hk/cardano-wallet)

[https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface)

1. Make sure you have some Alonzo Purple Test Ada

## Objectives

In this set of exercises, we will make sure that you can:

1. Interact with the Cardano Wallet CLI to create and manage wallets
2. Build and submit transactions that use Plutus scripts
3. Build DApps that interact with wallets

## Exercises

1. Download the latest instance of `cardano-wallet` and start up the node and wallet interface as described in the documentation. You will probably find it easiest to use the pre-built Docker instance since it will download the images, set up the configuration files, and also set up the connections between the node and wallet CLI, but you can instead use the pre-built binaries, or build from source if you prefer. Be sure to use the correct testnet configuration information (IOG runs multiple Cardano testnets).

```
wget …
NETWORK=testnet docker-compose up
```

2. Check the node synchronisation status

```
cardano-wallet network information

Ok.

{
    "node_era": "byron",
    "node_tip": {
        "height": {
            "quantity": 14033,
            "unit": "block"
        },
        "time": "2019-07-28T08:01:16Z",
        "epoch_number": 0,
        "absolute_slot_number": 15063,
        "slot_number": 15063
    },
    "sync_progress": {
        "status": "syncing",
        "progress": {
            "quantity": 29.5,
            "unit": "percent"
        }
    }
}

```

You will be able to run most wallet CLI commands before the network is fully synced, but transaction submission will fail, and your information will not be up to date.

3. Generate a recovery phrase and create a new empty wallet called "Main Wallet".

```
cardano-wallet recovery-phrase generate
cardano-wallet wallet create from-recovery-phrase …
```

Note that if you are using Docker, you will need to run the create command in interactive mode so that you can enter the recovery phrase (use the `-i` option to Docker).

Add funds to the wallet. Check the funds in the wallet and the UTxO distribution. Note, do not use commands that are related to "random" addresses or wallets – these are limited to the Byron era. Shelley and later wallets use "Hierarchical Derivation" (HD) wallets.

4. It is good practice to limit the funds that could be at risk when using smart contracts. Create two new wallets: "Plutus Collateral" and "Plutus Script Payments". Move funds into each of these wallets to cover immediate needs so that you do not risk the funds in "Main Wallet". You will need to top up the "Script Payments" wallets as you execute Plutus scripts. You should not need to top up the "Collateral" wallet unless you make a mistake.

5. Use the wallet CLI to submit an Alonzo transaction that execute a Plutus validator script to pay out 100 ada every time it is sent one or more Lars tokens – take the collateral from one or more addresses in the Collateral wallet and fees from one or more addresses in the Payments wallet. Adapt your application so that it manages a store of 100 Lars tokens, burning or minting new tokens as necessary.

Congratulations! You are now able to use the Cardano wallet CLI to interact with your own DApps!

6. **(Optional Exercise – Easy)**

Check the funds in your wallets using a wallet UI such as Daedalus or Yoroi and move funds between your wallets. Confirm the underlying changes in your balances/UTxO distribution etc. using the wallet CLI commands.


7. **(Optional Exercise – Moderate)**

a. Set up a "signature pool" so that a transaction may be authorised by any one of a number of signatories from a given set. Use the pool to sign an example transaction that transfers funds from your main wallet to the Payments wallet.

b. Adapt the signature pool so that at least two signatories are needed.

c. Each signatory is "single-shot". Once a signatory has signed a transaction, they can no longer sign any future transactions. Adapt your signature pool to allow for this.

d. Allow the set of signatories to be updated so that new signatories to be added to the pool to replace those whose signatures are no longer valid.

8. **(Optional Exercise – Easy)**

[Assuming Daedalus has DApp signing capability.]

Sign a Testnet transaction using the DApp signing interface to Daedalus. 

**NB: Take care to pay for the transaction using a Testnet address/wallet and not a mainnet one!! Otherwise you wull spend real ada!!**

## Feedback

**Please let us know of any problems that you have encountered**

- Via the Discord channels for general questions.

- Via the issue tracker at [https://github.com/input-output-hk/cardano-node/issues](https://github.com/input-output-hk/cardano-node/issues) for any bugs in the node etc.  Please tag them as Alonzo-related.

- Via the issue tracker at [https://github.com/input-output-hk/plutus/issues](https://github.com/input-output-hk/plutus/issues) for any bugs or feature requests with plutus, playground, PAB etc.

- Via the issue tracker at [https://github.com/input-output-hk/Alonzo-testnet/issues](https://github.com/input-output-hk/Alonzo-testnet/issues) for any issues with the exercises.


