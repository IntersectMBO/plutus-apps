# Alonzo Purple Testnet Exercise Sheet 5: "Multi-Asset Tokens"


In the fourth exercise, you wrote some simple transactions using datums and redeemers. In this exercise, we will practice managing multi-asset tokens.

## Prerequisites ##

1. Complete [Exercise Sheet 4](4_Alonzo-purple-exercise-4.md)

2. Start a passive Cardano node if you need to, and make sure that it has synced with the testnet.

3. Make sure you have some Alonzo Purple test ada

1. Read the tutorial information on:

    1. Native tokens and Mary-era _minting policies_
    2. How to manage native tokens with Plutus, including Plutus _minting scripts_

## Objectives ##

In the fifth set of exercises, we will make sure that you can:

1. Mint native tokens using both Mary-era and Plutus scripts
2. Redeem native tokens
3. Mint non-fungible tokens, including taking payments
4. Manage time-based scripts

## Exercises ##

1. Create a set of private/public signing keys, _shelley_, and two _payment addresses, mary_ and _percy_. Fund the addresses with some test Ada.

2. Define a Mary-era _minting script_ (a "multi-signature" script) that allows _shelley_ to create new **Ozymandian** tokens. Define a _minting policy_ for the **Shelley** currency that uses this _minting script_. **Do not use Plutus scripts at this stage.**

3. Mint 1000 new **Ozymandians** in the _percy_ address by building and submitting a transaction. Check that they have been successfully minted.

```
cardano-cli query utxo –address $(cat percy)
```

4. Define a second _minting script_ that allows _shelley_ to create new **SkyLark** tokens. Mint 100 **SkyLark** tokens and send them to _percy_. Check that the tokens have been received and then send 75 **SkyLark** tokens to _mary._

5. What is the least amount of **Ada** that you need to keep in the _mary_ and _percy_ addresses? What is the least amount of **Ozymandians** or **SkyLarks** that you can keep in an address?

6. You want to _burn_ some of your **Ozymandians** in the _percy_ address_._ How do you do this? What happens to your **Ada** balances when you burn your tokens?

7. Define a Plutus _minting script_ that allows you to mint a variable number of **Ozymandian** and **SkyLark** tokens (with the numbers supplied via a redeemer). Verify that this works as you expect.

8. Define a Plutus _minting script_ that allows you to mint a single instance of a _non-fungible token_. Your script should take a payment from a user-supplied address and pass this payment to an address of your choice.

9. Adapt your solution from Exercise 8 so that you conduct a Dutch auction on your _non-fungible token._ For example, start the bidding at 1000 Ada and reduce the price by 1 Ada every second. Sell the non-fungible token to the first client that offers to pay at least the current price. When the price falls below your hidden _reserve_, reject all future bids.

10. Adapt your solution from Exercise 9 so that the auction for the non-fungible token starts at a predetermined time. Reject all bids that arrive before the time.

11. **Optional Exercise (Easy to Moderate)**

	Publicise your _non-fungible token sale_ and participate in other token sales. Aim to collect the most interesting set of non-fungible tokens. When selling your tokens, you may want to record some metadata on the chain (e.g. representing a digital image or the purchaser&#39;s identity) as well as transferring the non-fungible token itself. How can you do this?

12. **Optional Exercise (Moderate)**

	Implement a token "factory".  You should accept the token, the _minting policy_ and the required number of tokens as parameters to your script and mint the required number of tokens.  Does your solution work for both *fungible* and *non-fungible* tokens?  How do you deal with third-party signatories?  Test your solution by allowing another testnet user to mint new tokens using your factory.
	
## Feedback


**Please let us know of any problems that you have encountered**

- Via the Discord channels for general questions.

- Via the issue tracker at [https://github.com/input-output-hk/cardano-node/issues](https://github.com/input-output-hk/cardano-node/issues) for any bugs in the node etc.  Please tag them as Alonzo-related.

- Via the issue tracker at [https://github.com/input-output-hk/plutus/issues](https://github.com/input-output-hk/plutus/issues) for any bugs or feature requests with plutus, playground, PAB etc.

- Via the issue tracker at [https://github.com/input-output-hk/Alonzo-testnet/issues](https://github.com/input-output-hk/Alonzo-testnet/issues) for any issues with the exercises.


