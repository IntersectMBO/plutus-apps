# marconi-sidechain

`marconi-sidechain` is a lightweight chain follower application for the Sidechain project to index and query specific information from the Cardano blockchain.

## Purpose

The purpose of ``marconi-sidechain`` is to encapsulate a subset of the indexers provided in [Marconi Chain Index](../marconi-chain-index) into a cohesive set which are required by the Sidechain team and which provides an interface to allow non-Haskell applications to query the indexed information.

## Interface

The interface for marconi-sidechain uses [JSON-RPC](http://www.simple-is-better.org/rpc/#differences-between-1-0-and-2-0) over HTTP built on top of [Marconi Chain Index](../marconi-chain-index/README.md).

```
             Running on a single machine                    Internet
+----------------------------------------------------+
|                                                    |                  +-------------+
|   +----------------+                +-----------+  |                  |             |
|   |                | node-to-client |           |  |     JSON-RPC     |  sidechain  |
|   |  cardano-node  +----------------+  marconi  +--+------------------+ application |
|   |                |      IPC       | sidechain |  |       HTTP       |             |
|   +----------------+                +----+------+  |                  +------------ +
|                                          |         |
|                                          |         |
|                                      +---+----+    |
|                                      | SQLite |    |
|                                      +--------+    |
|                                                    |
+----------------------------------------------------+
```

## Prerequisites

* [GHC](https://www.haskell.org/downloads/) (`==8.10.7`)
* [Cabal](https://www.haskell.org/cabal/download.html) (`>=3.4.0.0`)
* [Nix](https://nixos.org/download.html) (`>=2.5.1`)
  * Enable [IOHK's binary cache](https://iohk.zendesk.com/hc/en-us/articles/900000673963-Installing-Nix-on-Linux-distribution-and-setting-up-IOHK-binaries) or else you will build the world!
* [cardano-node](https://github.com/input-output-hk/cardano-node/releases/tag/1.35.4) (`==1.35.4`) running on preview testnet, pre-production testnet or mainnet

## How to build from source

### Cabal build

TODO

### Nix build

The `marconi-sidechain` executable is available as a nix flake.

If inside the `plutus-apps` repository, you can run from the top-level:

```
$ nix build .#marconi-sidechain
```

Or you may run from anywhere:

```
$ nix build github:input-output-hk/plutus-apps#marconi-sidechain
```

Both commands will produce a `result` directory containing the executable
`result/bin/marconi-sidechain`.

### Cabal+Nix build

To build `marconi-sidechain` from the source files, use the following commands:

```sh
git clone git@github.com:input-output-hk/plutus-apps.git
nix develop
cabal clean && cabal update # Optional, but makes sure you start clean
cabal build marconi-sidechain
```

The above process will build the executable in your local environment at this location:

```sh
cabal exec -- which marconi-sidechain
```

Or you can run the executable directly with:

```sh
cabal run marconi-sidechain:exe:marconi-sidechain -- --help
```

## Command line summary

Run `marconi-sidechain`, `$(cabal exec -- which marconi-sidechain) --help` or `cabal run marconi-sidechain:exe:marconi-sidechain -- --help` for a general synopsis of the command line options depending on your installation method.

See [this automatically generated golden file](./test/Spec/Golden/Cli/marconi-sidechain___help.help) for the up-to-date help command output.

## How to run

We are assuming that:

* you have a local running cardano-node instance.
* you've set the following environment variables:
  * `CARDANO_NODE_SOCKET_PATH`: socket path of your local cardano-node instance
  * `MARCONI_DB_DIRECTORY`: directory in which to create the various SQLite database files

The most minimal way to run the executable is as follows:

```sh
$(cabal exec -- which marconi-sidechain) \
    --testnet-magic 1 \
    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
    --db-dir "$MARCONI_DB_DIRECTORY" \
```

This command will do two things:

* from the last chainpoint (if none, from genesis), fetch blocks from the local node, extract required data and index them in the database.
* run a JSON-RPC server which will listen for any queries on the indexed data.

Using the `--addresses-to-index`, you can instruct `marconi-sidechain` to index target addresses.
By default, all addresses are indexed in the database.

Some example addresses from pre-production-testnet are:

```
addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc \
addr_test1vp8cprhse9pnnv7f4l3n6pj0afq2hjm6f7r2205dz0583egagfjah \
addr_test1wpzvcmq8yuqnnzerzv0u862hmc4tc8xlm74wtsqmh56tgpc3pvx0f \
addr_test1wrn2wfykuhswv4km08w0zcl5apmnqha0j24fa287vueknasq6t4hc \
addr_test1wr9gquc23wc7h8k4chyaad268mjft7t0c08wqertwms70sc0fvx8w \
```

## Querying JSON-RPC server

There is single endpoint from which the client can send requests using a `POST` request: `/json-rpc` (or `http:localhost:3000/json-rpc`)

The body of HTTP request must contain a JSON of the following format:

```json
{ "jsonrpc": "2.0"
, "method": "<METHOD>"
, "params": "<PARAMETERS>"
, "id": 0
}
```

The `id` field should be a random ID representing your request. The response will have that same ID.

### JSON-RPC API Methods

#### echo

Healthcheck method to test that the JSON-RPC server is responding.

**Parameters**: None

**Returns**: Nothing

**Example**:

```sh
$ curl -d '{"jsonrpc": "2.0", "method": "echo", "params": "", "id": 0}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc | jq
{
  "id": 0,
  "jsonrpc": "2.0",
  "result": []
}
```

#### getTargetAddresses

Retrieves user provided addresses.

**Parameters**: None

**Returns**: List of Bech32 addresses

**Example**:

Assuming the user started the `marconi-sidechain` executable with the address `addr_test1qz0ru2w9suwv8mcskg8r9ws3zvguekkkx6kpcnn058pe2ql2ym0y64huzhpu0wl8ewzdxya0hj0z5ejyt3g98lpu8xxs8faq0m` as the address to index.

```sh
$ curl -d '{"jsonrpc": "2.0" , "method": "getTargetAddresses" , "params": "", "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc | jq
{
  "id": 1,
  "jsonrpc": "2.0",
  "result": ["addr_test1qz0ru2w9suwv8mcskg8r9ws3zvguekkkx6kpcnn058pe2ql2ym0y64huzhpu0wl8ewzdxya0hj0z5ejyt3g98lpu8xxs8faq0m"]
}
```

#### getCurrentSyncedPoint (NOT IMPLEMENTED YET)

Retrieves the chain point from which the indexers are synced at.

**Parameters**: None

**Returns**: Chain point consisted of a slot number and a block header hash.

**Example**:

```sh
$ curl -d '{"jsonrpc": "2.0" , "method": "getCurrentSyncedPoint" , "params": "", "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc | jq
{
  "id": 1,
  "jsonrpc": "2.0",
  "result": {
      "blockHeaderHash": "6161616161616161616161616161616161616161616161616161616161616161",
      "slotNo": 1,
      "tag": "ChainPoint"
  }
}

OR

{
  "id": 1,
  "jsonrpc": "2.0",
  "result": {
    "tag": "ChainPointAtGenesis"
  }
}
```

#### getUtxoFromAddress

Retrieves user provided addresses.

**Parameters**: Address encoded in the Bech32 representation

**Returns**: List of UTXOs linked to the provided address.

**Example**:

```sh
$ curl -d '{"jsonrpc": "2.0" , "method": "getUtxoFromAddress" , "params": "addr_test1qz0ru2w9suwv8mcskg8r9ws3zvguekkkx6kpcnn058pe2ql2ym0y64huzhpu0wl8ewzdxya0hj0z5ejyt3g98lpu8xxs8faq0m", "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc | jq
{
  "id": 1,
  "jsonrpc": "2.0",
  "result": [
      {
          "blockHeaderHash": "6161616161616161616161616161616161616161616161616161616161616161",
          "slotNo": 1,
          "utxo": {
              "address": "addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc",
              "datum": null,
              "datumHash": null,
              "inlineScript": null,
              "inlineScriptHash": null,
              "txId": "ec7d3bd7c6a3a31368093b077af0db46ceac77956999eb842373e08c6420f000",
              "txIx": 0,
              "value": {
                  "lovelace": 10000000
              }
          }
      },
      {
          "blockHeaderHash": "6161616161616161616161616161616161616161616161616161616161616161",
          "slotNo": 1,
          "utxo": {
              "address": "addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc",
              "datum": "34",
              "datumHash": "eb8649214997574e20c464388a172420d25403682bbbb80c496831c8cc1f8f0d",
              "inlineScript": {
                  "script": {
                      "cborHex": "49484701000022220011",
                      "description": "",
                      "type": "PlutusScriptV1"
                  },
                  "scriptLanguage": "PlutusScriptLanguage PlutusScriptV1"
              },
              "inlineScriptHash": "284d60f7e56f5fd54faed4c50fd5cab0307da1c4034d6a92c5dbb940",
              "txId": "ec7d3bd7c6a3a31368093b077af0db46ceac77956999eb842373e08c6420f000",
              "txIx": 0,
              "value": {
                  "lovelace": 10000000
              }
          }
      }
  ]
}
```

#### getTxWithMintingPolicy (NOT IMPLEMENTED YET)

Retrieves transactions that include a minting policy for minting/burning tokens.

**Parameters**: Hash of the minting policy

**Returns**: List of transaction IDs

**Example**:

```sh
$ curl -d '{"jsonrpc": "2.0" , "method": "getTxWithMintingPolicy" , "params": "...", "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc | jq
{
  "id": 1,
  "jsonrpc": "2.0",
  "result": [
      {
          "assetName": "6d7961737365746e616d65",
          "blockHeaderHash": "6161616161616161616161616161616161616161616161616161616161616161",
          "policyId": "284d60f7e56f5fd54faed4c50fd5cab0307da1c4034d6a92c5dbb940",
          "quantity": -10,
          "redeemerData": "34",
          "redeemerIdx": 0,
          "slotNo": 1,
          "txId": "ec7d3bd7c6a3a31368093b077af0db46ceac77956999eb842373e08c6420f000"
      },
      {
          "assetName": "6d7961737365746e616d65",
          "blockHeaderHash": "6161616161616161616161616161616161616161616161616161616161616161",
          "policyId": "284d60f7e56f5fd54faed4c50fd5cab0307da1c4034d6a92c5dbb940",
          "quantity": 10,
          "redeemerData": "34",
          "redeemerIdx": 0,
          "slotNo": 1,
          "txId": "ec7d3bd7c6a3a31368093b077af0db46ceac77956999eb842373e08c6420f000"
      }
  ]
}
```

#### getStakePoolDelegationByEpoch (NOT IMPLEMENTED YET)

Retrieves the stake pool delegation per epoch.

**Parameters**: Epoch number

**Returns**: List of stake pool IDs with the amount of staked lovelace

**Example**:

```sh
$ curl -d '{"jsonrpc": "2.0" , "method": "getStakePoolDelegationByEpoch" , "params": "...", "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc | jq
{
  "id": 1,
  "jsonrpc": "2.0",
  "result": [
    {
      "poolId": "...",
      "lovelace": 10000000
    }
  ]
}
```

#### getNonceByEpoch (NOT IMPLEMENTED YET)

Retrieves transactions that include a minting policy for minting/burning tokens.

**Parameters**: Epoch number

**Returns**: Nonce

**Example**:

```sh
$ curl -d '{"jsonrpc": "2.0" , "method": "getNonceByEpoch " , "params": 398, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc | jq
{
  "id": 1,
  "jsonrpc": "2.0",
  "result": "8aa754b018e7d9bca65527bc3a03ae3802f74baf92564fc8941fe234373c87a3"
  }
}
```

[test-json-rpc.http](./examples/test-json-rpc.http) contains additional example usage.
