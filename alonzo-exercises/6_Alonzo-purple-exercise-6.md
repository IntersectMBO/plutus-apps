# Alonzo Purple Testnet Exercise Sheet 6: "Simple DApps" #

In the previous set of exercises, you interacted with native tokens. In this set of exercises, you will build some more substantial applications.

## Prerequisites ##

1. Complete [Exercise Sheet 5](5_Alonzo-purple-exercise-5.md)

2. Start a passive Cardano node if you need to, and make sure that it has synced with the testnet.

3. Make sure you have some Alonzo Purple test ada

## Objectives ##

In this set of exercises, we will make sure that you can:

1. Build more substantial DApps
2. Adapt the DApps to changing external situations

## Exercises ##

1. Build a _faucet_ application that dispenses funds on request. You should supply 1,000 Lovelace for each request.

2. Extend your faucet so that it dispenses different amounts of Lovelace if specific API keys are provided. For example

| **API Key** | **Amount** |
| --- | --- |
| Secret1 | 10,000 lovelace |
| Secret2 | 100,000 lovelace |

3. Test your _faucet_ by funding a number of addresses. Also fund the same address multiple times. Do you encounter any problems? If so, how can you fix them?

4. A "stable coin" aims to maintain a constant value. A _reserve_ of some asset is used to maintain the value of the stable coin. Build an application that models a stable coin using test Ada to fund the reserve. Each time a stable coin is minted or burned, an equivalent amount of ada should be transferred to/from the reserve.

5. Extend your stable coin model so that it is stable against some external currency (for example, Euros, US dollars, or Japanese Yen). Additional ada must be injected into or removed from the reserve each time the ada to currency exchange rate changes. Test your stable coin model against a series of rate changes.

6. Publicise your stable coin and exchange coins with other Testnet users. How effective is your model at maintaining a stable exchange rate? What problems do you encounter and how can you fix these.

## Feedback


**Please let us know of any problems that you have encountered**

- Via the Discord channels for general questions.

- Via the issue tracker at [https://github.com/input-output-hk/cardano-node/issues](https://github.com/input-output-hk/cardano-node/issues) for any bugs in the node etc.  Please tag them as Alonzo-related.

- Via the issue tracker at [https://github.com/input-output-hk/plutus/issues](https://github.com/input-output-hk/plutus/issues) for any bugs or feature requests with plutus, playground, PAB etc.

- Via the issue tracker at [https://github.com/input-output-hk/Alonzo-testnet/issues](https://github.com/input-output-hk/Alonzo-testnet/issues) for any issues with the exercises.


