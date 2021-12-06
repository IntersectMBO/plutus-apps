# Alonzo Purple Testnet Exercise Sheet 3 "Submitting Transactions Containing Basic Plutus Scripts"

In the first exercise, you set up and ran a passive Cardano node that was connected to the Alonzo Purple testnet.  You may also have participated in the Alonzo hard fork in the optional [Exercise 2](2_Alonzo-purple-exercise-2.md).  In this exercise, you will build and submit transactions to the Testnet that contain pre-compiled Plutus scripts and use the node CLI to manage the test ada that you have obtained via payment addresses and UTxOs.

## Prerequisites

- Complete [Exercise Sheet 1](1_Alonzo-purple-exercise-1.md)
- Ensure that you have the correctly tagged version of the node and CLI
- Make sure you have some Alonzo Purple test ada
- Read the tutorial information on:
	- Payment addresses
	- How to build and submit a Cardano transaction
- You may also want to watch [Plutus Pioneer Program - Lecture #2](https://youtu.be/E5KRk5y9KjQ) for background on Plutus scripts

## Objectives

In this set of exercises, we will make sure that you can:

- Create new Payment Addresses and the Associated Private and Public Keys
- Use Payment Addresses to Fund Private “Wallets”
- Build and Submit Transactions to the Testnet Blockchain, including ones containing Plutus scripts.
- Spend funds that are controlled by the script address.

This is the core of what is required to execute Plutus scripts on the Cardano blockchain.

## Exercises
### Part 1: Generating keys and building simple transactions

1. Start a passive Cardano Node if you need to, as you did in [Exercise Sheet 1](1_Alonzo-purple-exercise-1.md).  Note that you will not need to change the configuration information unless the testnet has been reset, and the node will synchronise faster than when starting from scratch.

``cardano-node run --config …``

or 

``docker run cardano-node run --config …``


2.	Confirm the amount of Purple test Ada that you have obtained from the "faucet":

``cardano-cli query utxo …``

3.	Create a new payment address, `wallet.addr`.  Record your private and public keys. This will act as a “wallet” that you can use to fund transactions and to pay for Plutus script execution etc.

```
cardano-cli address key-gen …
…
```
4.	Transfer some of your test Ada to `wallet.addr` by building, signing and submitting a transaction to the blockchain, as described in the tutorial.

```
cardano-cli transaction build …
…
cardano-cli transaction submit …
```
Confirm that you have successfully funded your wallet.  The `transaction build` command will calculate the required fee for you.  You may need to wait a short while before the transaction is recorded on chain.  At this stage, all the nodes that are connected to the testnet chain will be able to verify that you have funded the address.

``cardano-cli query utxo …``


### Part 2:  Submit a transaction to lock funds.

We will first use a pre-built Plutus validator script that always returns `True`. This is the simplest possible validator script (though it is not very useful except as a placeholder/test script!).

1. Download the pre-built [AlwaysSucceeds.plutus](/resources/plutus-scripts/AlwaysSucceeds.plutus) Plutus script, and obtain the script address.

``
cardano-cli address build …
``

2. Choose your favourite number and hash it:

``
cardano-cli transaction hash-script-data --script-data-value …
``

3. Build a transaction that includes this hash on the transaction output.

```
cardano-cli transaction build \
      --tx-in … \
      --tx-in-collateral … \
      --tx-out-datum-hash … \
      …
```

The collateral is used to pay for a transaction that fails to execute.  The `transaction build` command checks that the script validation succeeds, so collateral is safe if the transaction is submitted.


4. Sign the transaction as usual, using the secret key for `wallet.addr`

``
cardano-cli transaction sign …
``

5. Submit the transaction to the chain. 

``
cardano-cli transaction submit …
``

6. Confirm that the funds are locked with the correct datum hash.


### Part 3:  Unlocking funds that are guarded by a Plutus script.

Now we want to create a transaction to return the locked funds. This validation script always passes but it is still important to use a datum value that matches the datum hash you used in part 2.

1. Build a transaction including the script and submit it. Pay for it using funds from `wallet.addr`.  Note that you will need to provide a "redeemer" in addition to the other information you gave previously.

```
cardano-cli transaction build \
      … \
      --tx-in-redeemer-value … \
```

2. Confirm that the funds have been unlocked and returned to your wallet. 

3. What happens if you give the wrong value when trying to unlock the funds?


### Optional Exercises

1.	Optional Exercise (Easy)

Submit a transaction containing the [AlwaysFails.plutus](/resources/plutus-scripts/AlwaysFails.plutus) Plutus script.  How does the outcome differ from [AlwaysSucceeds.plutus](/resources/plutus-scripts/AlwaysSucceeds.plutus)?

2.	Optional Exercise (Easy)

What other kinds of data can you include in a datum value?

3.	Optional Exercise (Moderate)

Write your own version of the `AlwaysSucceeds` Plutus script, compile your script and submit it to the Alonzo Testnet.  Verify that it has succeeded.  (Note: we will do this properly in [Exercise Sheet 4](4_Alonzo-purple-exercise-4.md) so completing this exercise will involve you reading ahead!)

4.	Optional Exercise (Moderate)

What ways do you have to record “state” information on-chain using Plutus scripts?

The next exercise will involve compiling and submitting some more complex Plutus scripts using your own node.

## Feedback

**Please let us know of any problems that you have encountered**

- Via the Discord channels for general questions.

- Via the issue tracker at [https://github.com/input-output-hk/cardano-node/issues](https://github.com/input-output-hk/cardano-node/issues) for any bugs in the node etc.  Please tag them as Alonzo-related.

- Via the issue tracker at [https://github.com/input-output-hk/plutus/issues](https://github.com/input-output-hk/plutus/issues) for any bugs or feature requests with plutus, playground, PAB etc.

- Via the issue tracker at [https://github.com/input-output-hk/Alonzo-testnet/issues](https://github.com/input-output-hk/Alonzo-testnet/issues) for any issues with the exercises.

