# Alonzo Purple Testnet Exercise 2 (Optional): "The Hard Fork to Alonzo"

**Note that this may be a one-off event for each testnet since we will need to start the network from scratch - the timing will be notified on the relevant Discord channel**

## Prerequisites ##

 - Complete [Exercise Sheet 1](1_Alonzo-purple-exercise-1.md)
 - Start a passive Cardano node
 - Make sure that you have set the CARDANO_NODE_SOCKET_PATH environment variable correctly.
 - Install [lnav](http://lnav.org/) (optional) if you would like to use this to monitor the log files on Linux.
 - Join the network at the specified times:
	- Before the hard fork
	- Once the update proposal has been issued by IOG DevOps
	- During the hard fork, if possible (otherwise, after the hard fork)
 - Join the `exercise-2` Discord channel so you can follow along with your cohort.



## Objectives

In this optional set of exercises, we will:

- Check the testnet era
- See an Update Proposal submitted to the testnet blockchain
- Follow along as the testnet “hard forks” to Alonzo


## Exercises

**Before the hard fork**

1. Start a passive Cardano node as you did in [Exercise Sheet 1](1_Alonzo-purple-exercise-1.md). Note that you will not need to reset the configuration information, and the node will synchronise faster than when starting from scratch.  **Make sure the log output is being sent to a file.**

```
      cardano-node run --config … > node.log
```
  or

```
        docker run cardano-node run --config …
```

2. Check the network era from another terminal window

```
		cardano-cli query tip --testnet-magic …
		{
		    …
		    "era": "Mary"
		}
```

3. Once the Update Proposal has been issued

	Check that your node has seen the Alonzo Update Proposal
	
```
		grep -2 -i update node.log
		…
	    … HardForkUpdateInEra S (S (S (Z …
		…
```

(use `grep --context=2` on MacOS).

Note that you may also see update proposals for previous eras.

The network should still be in the Mary era

```
		cardano-cli query tip --testnet-magic …
		…
```

4. Monitor the logs

```
        tail -f -n2 node.log
        …
```

or (if you have installed the `lnav` command)

```
        lnav -t -c ‘:goto 100%’ node.log
```

*(When running `lnav`, the `G` command will take you to the end of the log, `g` will take you to the start)*

You will see the node change era as the hard fork happens (it may take a short while for the change to propagate across the network).

```
        … HardForkUpdateTransitionConfirmed … 
        … 
        … HardForkUpdateTransitionDone …
```

**After the hard fork**

5.	Check the logs for the hard fork event

```
          grep -2 -i HardForkUpdate node.log
          … HardForkUpdateTransitionDone
```

(again use `--context=2` on MacOS)

6. Confirm that the testnet is  in the Alonzo era

```
        cardano-cli query tip --testnet-magic …
        {
          …
          "era": "Alonzo"
        }
```

**Congratulations!  You have successfully negotiated the “hard fork” and your node is now running in the Alonzo era!**


7. Additional Exercise (Moderate)

	Use some of your test ada to build and submit a simple transaction before and after the hard fork (eg a funds transfer from your original payment address to a new address that you have created - don’t try to run a Plutus script yet – we will do that in [Exercise Sheet 3](3_Alonzo-purple-exercise-3.md)!).  Do you notice any differences between the two transactions (inspect the files that are built by the transaction build-raw and transaction sign commands)?  How do you check whether your transactions have succeeded?

  Now that we have gone through the hard fork into Alonzo, the next exercise will involve building, signing and submitting simple Plutus transactions using your own node.  

## Feedback ##

**Please let us know of any problems that you have encountered**

- Via the Discord channels for general questions.

- Via the issue tracker at [https://github.com/input-output-hk/cardano-node/issues](https://github.com/input-output-hk/cardano-node/issues) for any bugs in the node etc.  Please tag them as Alonzo-related.

- Via the issue tracker at [https://github.com/input-output-hk/plutus/issues](https://github.com/input-output-hk/plutus/issues) for any bugs or feature requests with plutus, playground, PAB etc.

- Via the issue tracker at [https://github.com/input-output-hk/Alonzo-testnet/issues](https://github.com/input-output-hk/Alonzo-testnet/issues) for any issues with the exercises.

