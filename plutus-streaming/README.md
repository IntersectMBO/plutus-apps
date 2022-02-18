# Plutus Streaming PoC

Just testing ideas on a streaming interface.

# CLI

You can run the CLI in many ways:

From Genesis

```
$ cabal run -- main --socket-path /tmp/node.socket
```

Passing a starting point

```
$ cabal run -- main --socket-path /tmp/node.socket --slot-no 53427524 --block-hash 5e2bde4e504a9888a4f218dafc79a7619083f97d48684fcdba9dc78190df8f99
```

Running different examples

```
$ cabal run -- main --socket-path /tmp/node.socket  --example HowManyBlocksBeforeRollback
```

```
$ cabal run -- main --socket-path /tmp/node.socket  --example HowManyBlocksBeforeRollbackImpurely
```
