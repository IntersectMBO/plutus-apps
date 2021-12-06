# Alonzo Purple Testnet Exercise 1 "Getting started"

## Welcome to the Alonzo Purple Testnet!  

We are excited to have you join us on the journey to delivering Plutus scripts for Cardano.  We will be running through a series of structured tests on the brand-new Alonzo-era Cardano node so that we can improve the experience for other Plutus users.  At this stage, the software and documentation are both very new, so there will certainly be improvements that can be made.  You will also be using the Cardano node directly rather than the Plutus Application Backend (PAB) that we are developing to simplify the process of submitting and executing Plutus scripts. Your role in helping us identify and make these improvements is very important.  Your feedback is essential and will be listened to!

## Prerequisites

- Attend the Alonzo Testnet Briefing Meeting or Read the Testnet Briefing Notes (currently restricted to invitees).
- Read the Alonzo Testnet Code of Conduct.  This governs your behaviour and responsibilities on the Alonzo Testnets (currently restricted to invitees).
- Register yourself on Discord and join the designated channels (currently also estricted to invitees).
- Make sure you can access:


	1. The IOHK Tutorial Documentation at: [https://docs.cardano.org/introduction](https://docs.cardano.org/introduction)
	2. The Alonzo Purple Testnet configuration (see main [README.md](../../README.md))
	3. Either, the correct binary image for your operating system, or the Docker image (see main [README.md](../../README.md)).
	4. The associated documentation at: [https://docs.cardano.org/getting-started/installing-the-cardano-node](https://docs.cardano.org/getting-started/installing-the-cardano-node)


- You should be comfortable with using Shell commands and have a basic understanding of the Cardano ecosystem.

- Set up your platform.  Unless you are experienced with building the node from source, you should use the pre-built Node and CLI binaries that are supplied by IOG.

- You will need a Physical Host Machine, Virtual Machine or AWS instance with **at least 2 cores, 8GB of RAM, and at least 10GB of free storage.**
- If you are using Docker.
Download and install the latest Docker software for your system.  If you are using Docker Desktop, ensure that you allow **at least 6GB of memory.**
- If you are using the binary files.
Make sure you are running a recent stable version of Linux (eg Ubuntu 20.04 or 18.04, Windows 10, or MacOS 10.14 or later).  


- You should need only a bare version of the system (no additional installation).

- Make sure you are on a network that is not firewalled. In particular, we will be using TCP/IP port 3001 by default to establish connections with other nodes, so this will need to be open to the world.

**This should be everything that you need to get you up and running.**
 If you get stuck at any point or have any questions, there is a dedicated Discord channel for `exercise-1` and IOG staff will also run periodic "office hours" when they will be happy to answer questions.

## Objectives

In the first set of exercises, we will make sure that you can:

1. Set up and run an Alonzo-capable node;
2. Connect your node to the Alonzo Purple testnet blockchain;
3. Obtain some test ada to use in the testnet.

## Exercises

Download and install the correct version of the Cardano node and CLI software.  

Unless you are already an expert, we do not recommend that you build binaries from source — we will not be able to help you if you encounter problems with the build. Make sure that your search path includes the location for all the binaries that you have downloaded.

Note that we will be updating the software regularly to add features, fix bugs, improve performance, so you will need to use the correctly tagged version of the node at each stage.  Please check this.

1. Verify your versions of `cardano-node` and `cardano-cli`:

```
		cardano-node –-version

		cardano-cli –-version
```
or

```
		docker run cardano-node run --version
```

(see the [documentation](https://github.com/input-output-hk/Alonzo-testnet/blob/main/documentation/docker.md) for information on how to run `cardano-cli` from the Docker image and other tips. Note that you should not use `NETWORK=mainnet` in the `docker` command if you are connecting to a Testnet, of course!)

2. Create a working directory for your node instance.  Download the genesis files, topology file, and config file for the Alonzo Purple testnet and store them in your working directory.  

![](images/configurations.png)

3. Start the node using the configuration information that you have downloaded:

```
		cardano-node run \
		--topology alonzo-purple-topology.json \
		--database-path db \
		--socket-path node.socket \
		--port 3001 \
		--config alonzo-purple-config.json
```
![](images/node_running.png)

**You are running a so-called “passive” node.  Your node will not participate in block production or verification, and it will connect to the dedicated IOG Testnet relay nodes to obtain information from the network.**

Make sure that you set the `CARDANO_NODE_SOCKET_PATH` environment variable correctly (for example, `export CARDANO_NODE_SOCKET_PATH="$HOME"/alonzo-purple/node.socket`).  You may want to update your `.bashrc` or other Shell configuration files so this is done automatically in future.

Check that your instance of the node is properly connected to the Alonzo Purple Testnet and is fully synchronised.  You may need to wait a few minutes.  Use the relevant **cardano-cli commands** to query the tip of the blockchain.

Congratulations!  You have a working node connected to the public Alonzo Cardano network!

4. Use `cardano-cli` to generate payment keys and address
6. Log on to the dedicated Discord channel for `alonzo-purple` and introduce yourself.  In addition to your peers, you will meet IOG staff who are responsible for running the testnet, stake pool operators who are responsible for producing blocks and ensuring that the Alonzo system works, community advisors and others. Please follow the rules on good conduct!
5. Obtain test Ada from the "faucet" that is specified in the Discord channel

The next full exercise [Exercise 3](3_Alonzo-purple-exercise-3.md) will involve building, signing and submitting simple Plutus transactions using your own node.  

Before then, you might want to participate in the Hard Fork Event using the optional [Exercise 2](2_Alonzo-purple-exercise-2.md).

## Feedback


**Please let us know of any problems that you have encountered**

- Via the Discord channels for general questions.

- Via the issue tracker at [https://github.com/input-output-hk/cardano-node/issues](https://github.com/input-output-hk/cardano-node/issues) for any bugs in the node etc.  Please tag them as Alonzo-related.

- Via the issue tracker at [https://github.com/input-output-hk/plutus/issues](https://github.com/input-output-hk/plutus/issues) for any bugs or feature requests with plutus, playground, PAB etc.

- Via the issue tracker at [https://github.com/input-output-hk/Alonzo-testnet/issues](https://github.com/input-output-hk/Alonzo-testnet/issues) for any issues with the exercises.


