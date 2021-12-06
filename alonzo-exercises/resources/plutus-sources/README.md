This directory is adapted from the Plutus examples in [cardano-node](https://github.com/input-output-hk/cardano-node).  This version includes `plutus-helloworld` which instantiates the example with a different script (the `HelloWorld` script needed for the Exercises).

The original sources and further examples can be found in:

[https://github.com/input-output-hk/cardano-node/tree/master/plutus-example](https://github.com/input-output-hk/cardano-node/tree/master/plutus-example)

[https://github.com/input-output-hk/cardano-node/tree/master/scripts/plutus](https://github.com/input-output-hk/cardano-node/tree/master/scripts/plutus)


# plutus-example

This directory demonstates end-to-end examples of creating and executing Plutus scripts on chain.

This is done roughly in the following steps:

1. Write your Plutus **on chain** code.
2. Serialize your Plutus on chain code to the text envelope format (`cardano-cli` expects this format).
3. Create your transaction with the accompanying Plutus script(s).
4. Submit transaction to execute Plutus script.

# plutus-alwayssucceeds

This directory contains sources for a Plutus script that "always succeeds"

# plutus-helloworld

This directory contains a few "Hello World" script examples. There are three versions: the latest addition uses bytestring literal. The other two are using an integer literal and a bytestring parameter (both are alternative workarounds for when the Plutus interpreter didn't yet accept byte string literals).

# plutus-deadline

This directory contains some timelocking examples. There are three versions: [Deadline.hs](./plutus-deadline/src/Cardano/PlutusDeadline/Deadline.hs) is a validation script using a constant posix time deadline, [DeadlinePolicy.hs](./plutus-deadline/src/Cardano/PlutusDeadline/DeadlinePolicy.hs) is a minting policy with a constant posix time deadline, and [DeadlineRedeemer.hs](./plutus-deadline/src/Cardano/PlutusDeadline/DeadlineRedeemer.hs) is a minting policy that applies the numeric redeemer as the posix time deadline.

## FAQ

### Where is the off chain code?

The off chain code is used for transaction construction. In this case we construct the transaction with `cardano-cli` and therefore we don't need to write any off chain code.

### Where can I learn about Plutus scripts in more detail?

Our education director, Lars Brünjes, has an excellent series of [tutorials](https://youtu.be/IEn6jUo-0vU) on youtube. We will not attempt to provide an indepth explanation of Plutus in this repository.
