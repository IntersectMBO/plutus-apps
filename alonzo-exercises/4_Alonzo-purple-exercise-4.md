## Alonzo Purple Testnet Exercise Sheet 4: "Compiling and Submitting Simple Plutus Transactions"


In the third exercise, you submitted a pre-compiled transaction to the Alonzo testnet using the node CLI, and made sure you could use the test ada that had been given to you. In this exercise, you will write some very simple Plutus transactions, calculate the fees for those transactions, and submit them to the testnet.

## Prerequisites ##
1. Complete [Exercise 3](3_Alonzo-purple-exercise-3.md)
2. Start a passive Cardano node if you need to, and make sure that it has synced with the testnet. It should be in the Alonzo era.
3. Make sure you have some Alonzo Purple test ada
4. Check out the [resources](../../resources) directory for useful sources and scripts etc.

## Objectives ##

In the fourth set of exercises, we will make sure that you can:

1. Compile and submit simple Plutus transactions
2. Calculate fees for Plutus transactions
3. Determine what effect your Plutus transactions have had on the blockchain

## Exercises ##

1. Compile the `AlwaysSucceeds` Plutus script from [source](../../resources/plutus-sources/plutus-alwayssucceeds) and extract the serialised representation that the compiler has produced.

```
{
    `"type": "PlutusScriptV1"`,
    ….
}
```

You may also want to inspect the Plutus Core form – this is a representation of the compiled script that will be executed on the blockchain. Confirm that your script is identical to the pre-compiled version that you used in [Exercise 3](3_Alonzo-purple-exercise-3.md). If not, how is it different, and why?

2. Compile the `HelloWorld` Plutus script from [source](../../resources/plutus-sources/plutus-helloworld). Save the serialised Plutus script into a file `helloworld.plutus`.

3. Build a Cardano transaction that will submit `helloworld.plutus` for execution on the testnet.  As before, you will need to provide two input addresses: one to pay for the transaction fees and one to provide the collateral that is spent if the Plutus script fails to validate (to avoid risking all your funds, it is recommended that you use a dedicated payment address with limited funds to pay for the collateral rather than your main payment address!).

`cardano-cli transaction build …`

Submit the transaction to the testnet and confirm that your transaction has been recorded on the testnet blockchain. Note that you may need to wait a short time.

`cardano-cli transaction submit …`

4. Modify the `HelloWorld` Plutus script:

a. To succeed if  the datum is your name;

b. To succeed if the redeemer is also your birthday;

c. To take a datum and a redeemer and to succeed if the redeemer is the same as the datum;

d. To take a datum that represents a pair of integers and a redeemer that represents a list of integers and succeed if all elements in the redeemer are within the range specified by the values in the datum.

Compile each of the Plutus transactions, build the corresponding Cardano transactions, and submit these to the blockchain.  Test your transactions on various inputs.

5. Set up three new payment addresses: `payment.addr`, `wallet1.addr`, and `wallet2.addr` using the node CLI commands.  Transfer some ada to each of these addresses, and check that they have been funded.

Produce a transaction that sends 100 ada from `wallet1.addr` to `wallet2.addr` provided the correct “secret spending key” is provided as a redeemer and submit this.

Check that the funds have been transferred correctly between the wallets.

6. Produce a second transaction that sends some ada from `wallet2.addr` to `wallet1.addr` guarded by a different “secret spending key”.  Practice sending Ada between the “wallets” and observe the effect on the corresponding UTxO entries.

`cardano-cli query utxo …`

7. Optional Exercise (Easy)

Extend your transactions from Q5/6 so that each “wallet” always maintains a minimum balance.

8. Optional Exercise (Moderate)

Produce a Plutus “slot machine” that pays out a Lovelace “jackpot” if it is given a specific set of symbols as a redeemer (e.g. three Bells pays 100 Lovelace).  The machine takes a fixed fee in lovelace and pays any winnings from a pre-funded spending “pot”.  It should not pay out the jackpot if there are insufficient funds in the “pot”. Test your slot machines by exchanging scripts with other testnet users.

9. Optional Exercise (Easy)

Extend the slot machine from Q8 to pay different jackpots for different winning combinations.

The next exercise will involve compiling and submitting some more complex Plutus scripts using your own node.

## Feedback


**Please let us know of any problems that you have encountered**

- Via the Discord channels for general questions.

- Via the issue tracker at [https://github.com/input-output-hk/cardano-node/issues](https://github.com/input-output-hk/cardano-node/issues) for any bugs in the node etc.  Please tag them as Alonzo-related.

- Via the issue tracker at [https://github.com/input-output-hk/plutus/issues](https://github.com/input-output-hk/plutus/issues) for any bugs or feature requests with plutus, playground, PAB etc.

- Via the issue tracker at [https://github.com/input-output-hk/Alonzo-testnet/issues](https://github.com/input-output-hk/Alonzo-testnet/issues) for any issues with the exercises.









