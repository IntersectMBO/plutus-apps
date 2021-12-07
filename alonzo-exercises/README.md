# Alonzo-testnet
*This is a migration of the [Alonzo-testnet](https://github.com/input-output-hk/Alonzo-testnet) repository that was used for the Cardano Alonzo testnet rollout program. The following has has some links updated, no major changes to the content have been made.*

Here you will find a set of exercises and support material to use on the Alonzo testnet. We will gradually add materials to this repository through the testnet phases to help you understand the testnet and make use of its features.

The planned syllabus is outlined below.  Exercises will be deployed here and linked from this page as they are finalised.

## Target Audience

The exercises are designed to help those who are unfamiliar with the Cardano node to write and submit Plutus scripts on the Alonzo testnets using the node CLI, so that they can develop their own simple DApps.  Those who are already familiar with the node may choose to skip some of the earlier exercises, of course. Note that there will be no Cardano wallet or Plutus application backend support prior to Alonzo Purple, so functionality will be limited compared with the full Plutus experience.

## Useful Resources

### Documentation

[Instructions for installing the Cardano node (pre-compiled, cabal, nix & Docker)](https://docs.cardano.org/getting-started/installing-the-cardano-node)

[Plutus Scripts and other useful Resources](./resources/)

[Plutus Explainer (Technical Terminology)](./explainers/Plutus-explainer.md)

[Plutus Tutorials](https://plutus.readthedocs.io/en/latest/plutus/tutorials/index.html)

[Cardano Documentation](https://docs.cardano.org/plutus/learn-about-plutus)

### Executable Nodes

[Alonzo-1.30.1 node Linux binary](https://hydra.iohk.io/build/7739415)

[Alonzo-1.30.1 node MacOSX binary](https://hydra.iohk.io/build/7739444)

[Alonzo-1.30.1 node Windows binary](https://hydra.iohk.io/build/7739339)

[Alonzo-1.30.1 node Docker image](https://hub.docker.com/layers/inputoutput/cardano-node/1.30.1/images/sha256-039dfe8ed474b03fba84746b744787a1002ba068313b9d742eeebfaab2d2b0c0?context=explore)

### Network Configurations

[Alonzo Purple and Cardano Testnet configuration files](https://hydra.iohk.io/build/7366583/download/1/index.html)

## Exercise 1: Getting Started

1. Introduction to the Alonzo Testnet.
1. Setting up a node to connect to the testnet.
1. Obtaining test Ada.  


[Exercise 1](./1_Alonzo-purple-exercise-1.md)
[(Model Solutions)](./solutions/exercise1)

## Exercise 2: Hard Fork (Optional)

See an update proposal submitted to the chain and follow along as we go through the hard fork to the Alonzo era.  

(*Note that this may be a one-off event for each Testnet since we will need to start the network from scratch - the timing will be notified on the relevant Discord channel*).

[Exercise 2](./2_Alonzo-purple-exercise-2.md)

## Exercise 3: Addresses and Transactions

1. Managing payment addresses.
1. Building, signing and submitting transactions on chain.  
1. Submitting a transaction containing a pre-built Plutus script.

[Exercise 3](./3_Alonzo-purple-exercise-3.md)
[(Model Solution)](./solutions/exercise3/e3SampleSolution.md)

## Exercise 4: Compiling and Submitting Simple Plutus Scripts


1. Compiling simple Plutus transactions. 
2. Submitting simple Plutus transactions. 
3. Calculating fees for Plutus transactions.
4. Determining what effect your Plutus transactions have had on the blockchain.

[Exercise 4](./4_Alonzo-purple-exercise-4.md)
[(Model Solution)](./solutions/exercise4/Exercise4-solution.md)

## Exercise 5: Managing Native Tokens


1. Minting native tokens using both Mary-era forging scripts
1. Minting native tokens using Plutus forging scripts
1. Redeeming/burning native tokens
1. Minting non-fungible tokens
1. Time-based scripts


[Exercise 5](./5_Alonzo-purple-exercise-5.md)

## Exercise 6: DApps


1. Building more substantial DApps
2. Interacting with external events


[Exercise 6](./6_Alonzo-purple-exercise-6.md)

## Exercise 7: Interacting with the Wallet (Coming Soon)


1. The Cardano Wallet CLI
2. Submitting transactions with Plutus Scripts from the wallet CLI
3. Some simple DApps using the wallet CLI


[Exercise 7](./7_Alonzo-purple-exercise-7.md)

## Feedback


We welcome feedback on any issues you have encountered:

- Via the Discord channels for general questions or discussion.

- Via the issue tracker at [https://github.com/input-output-hk/cardano-node/issues](https://github.com/input-output-hk/cardano-node/issues) for any bugs or feature requests in the node etc.  Please tag them as Alonzo-related.

- Via the issue tracker at [https://github.com/input-output-hk/plutus/issues](https://github.com/input-output-hk/plutus/issues) for any bugs or feature requests with plutus, playground, PAB etc.

- Via the issue tracker at [https://github.com/input-output-hk/Alonzo-testnet/issues](https://github.com/input-output-hk/Alonzo-testnet/issues) for any issues with the exercises.

### Can I join in if I don't yet have an invite to the IOG-run Testnet?

Yes, if you have sufficient technical expertise, you are more than welcome to set up your own private cluster and to try the exercises!
We have provided sample solutions to help if you encounter problems and our tutorials and other documentation are all available for you to use.
As we open up the Alonzo testnets, we will then be able to integrate you with other community users, including providing access to the Testnet Discord channels.
We recommend that you follow the [Alonzo Purple exercises](.).
You may find the following setup guides useful.

[How to launch a testnet](https://github.com/input-output-hk/cardano-node/blob/master/doc/getting-started/launching-a-testnet.md)

[Scripts for setting up a testnet](https://github.com/input-output-hk/cardano-node-tests#tests-development)

[How to Make a Shelley blockchain from scratch](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/shelley-genesis.md)
