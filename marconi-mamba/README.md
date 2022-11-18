
# Marconi-Mamba

Marconi-Mamba is a lightweight, customizable chain follower application and library for the Mamba project to index and query specific information from the Cardano blockchain.
## Purpose

The purpose of Marconi-Mamba is to make the core Marconi APIs available to non-Haskell applications.

## Interface

The interface for Marconi-Mamba uses [JSON-RPC](http://www.simple-is-better.org/rpc/#differences-between-1-0-and-2-0) over HTTP built on top of [Marconi](../marconi/README.md).


```
             Running on a single machine                    Internet
+----------------------------------------------------+
|                                                    |                  +-------------+
|   +----------------+                +-----------+  |                  |             |
|   |                | node-to-client |           |  |     JSON-RPC     |    mamba    |
|   |  cardano-node  +----------------+  marconi  +--+------------------+ application |
|   |                |      IPC       |   mamba   |  |       HTTP       |             |
|   +----------------+                +----+------+  |                  +------------ +
|                                          |         |
|                                          |         |
|                                      +---+----+    |
|                                      | SQLite |    |
|                                      +--------+    |
|                                                    |
+----------------------------------------------------+
```

## Requirements
* Local instance of a compatible version of [cardano-node](https://github.com/input-output-hk/plutus-apps/blob/main/cabal.project#L246).
* [Nix](https://nixos.org/download.html): the package manager
* Enable [IOHK's binary cache](https://iohk.zendesk.com/hc/en-us/articles/900000673963-Installing-Nix-on-Linux-distribution-and-setting-up-IOHK-binaries)

## Building from source
To build Marconi-Mamba from the source files, use the following commands: 

``` sh
git clone git@github.com:input-output-hk/plutus-apps.git
nix-shell
cabal clean
cabal update
cabal build marconi-mamba
```

The above process will build the executable in your local environment at this location:

``` sh
 cabal exec -- which marconi-mamba
```

## Command line summary

The following is a general synopsis of the command line options: 

``` sh
$(cabal exec -- which marconi-mamba) --help
marconi-mamba - Cardano blockchain indexer

Usage: marconi-mamba --socket-path FILE --utxo-db FILE [--http-port HTTP-PORT]
                     (--mainnet | --testnet-magic NATURAL)
                     (--addresses-to-index ARG)

Available options:
  --socket-path FILE       Socket path to node.
  --utxo-db FILE           Path to the utxo database.
  --http-port HTTP-PORT    JSON-RPC http port number. Default is port 3000.
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --addresses-to-index ARG Becch32 Shelley addresses to index, i.e.,
                           "--address-to-index address-1 --address-to-index
                           address-2 ..."
  -h,--help                Show this help text
```

## Example of using Marconi-Mamba

To use Marconi-Mamba, follow these steps: 
1. Invoke the JSON-RPC server
2. Interrogate the JSON-RPC endpoints
3. Interrogate the REST endpoints

These steps are described in more detail below. 

## Invoking the JSON-RPC server

The following is an example shell script for executing Marconi-Mamba in [preview-testnet](https://book.world.dev.cardano.org/environments.html#preview-testnet). 

### Requirement

Compatible version of [cardano-node](https://github.com/input-output-hk/plutus-apps/blob/main/cabal.project#L246) must be running with the socket-path as outlined below:

``` sh
#!/usr/bin/env sh

CARDANO_NODE_DIR=../cardano-node
MAINNET_DIR="$CARDANO_NODE_DIR/.cardano-node/mainnet"
PREVIEW_TESTNET_DIR="$CARDANO_NODE_DIR/.cardano-node/preview-testnet"

CONFIG_DIR=$PREVIEW_TESTNET_DIR
CARDANO_NODE_SOCKET_PATH="$CONFIG_DIR/cardano-node.socket"

DB="./.marconidb/4"
mkdir -p $DB

$(cabal exec -- which marconi-mamba) \
    --testnet-magic 2 \
    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
    --utxo-db "$DB/utxo-db" \
    --addresses-to-index addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc \
    --addresses-to-index addr_test1vp8cprhse9pnnv7f4l3n6pj0afq2hjm6f7r2205dz0583egagfjah \
    --addresses-to-index addr_test1wpzvcmq8yuqnnzerzv0u862hmc4tc8xlm74wtsqmh56tgpc3pvx0f \
    --addresses-to-index addr_test1wrn2wfykuhswv4km08w0zcl5apmnqha0j24fa287vueknasq6t4hc \
    --addresses-to-index addr_test1qr30nkfx28r452r3006kytnpvn39zv7c2m5uqt4zrg35mly35pesdyk43wnxk3edkkw74ak56n4zh67reqjhcfp3mm7qtyekt4 \
    --addresses-to-index addr_test1wr9gquc23wc7h8k4chyaad268mjft7t0c08wqertwms70sc0fvx8w \
    --addresses-to-index addr_test1vqeux7xwusdju9dvsj8h7mca9aup2k439kfmwy773xxc2hcu7zy99
```

## Interrogating the JSON-RPC endpoints

``` sh
|-----------+-----------+------------------------+---------------------------------------------|
| HTTP Verb | Endpoints | RPC method             | Description                                 |
|-----------+-----------+------------------------+---------------------------------------------|
| POST      | json-rpc  | addresseesBech32Report | Retrieves user provided addresses           |
| POST      | json-rpc  | utxoReport             | Retrieves TxRefs for an address             |
| POST      | json-rpc  | utxoReportx            | Retrieves TxRefs for all provided addresses |
| POST      | JSON-rpc  | echo                   | echo's user input to console                |
|-----------+-----------+------------------------+---------------------------------------------|
```

## Interrogating the REST endpoints

``` sh
|-----------+-----------+-----------------------------------|
| HTTP Verb | Endpoints | Description                       |
|-----------+-----------+-----------------------------------|
| GET       | addresses | Retrieves user provided addresses |
| GET       | time      | current local time                |
|-----------+-----------+-----------------------------------|
```

## Examples

Here is a curl script to exploit the JSON-RPC server:

``` sh
curl -d '{"jsonrpc": "2.0" , "method": "utxoTxOutReport" , "params": "addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc"  , "id": 12}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc

{
  "id": 12,
  "jsonrpc": "2.0",
  "result": {
    "bech32Address": "addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc",
    "txOutRefs": [
      "00022d3d74304c089dc76620d6a39089c008862165c69200d82a34f53a6b93dc#0",
      "000b106461150f070409f1183efc832a4a95c25f0083777648a1a2f3f9dbe8bc#0",
      "001351bd6e2f09e275c4ab32570f639fb1ac17d9e6bf6ae8bf7d451aa368336f#0",
      "00167959f52fab36abad607384a8254ce0e3d4d014147712f7caffbbe6b51385#0",
      "0017ae0b0a0466c9728ae5040afa484d4f9797da2935db045f08de85aaec32ce#0",
      "001a23cceafe105d7efd235cf258a7d0d78f2ce335607e8d52e7e25618c7bd90#0",
      "001c537acf3c98d84d5cb419243dc1f8f14075a787e85360dced600b9b4ef5bb#0",
      "00260d5e8f70dd061ae3ab4c090ab4d65e49f6501fde980eff1f35d66520f014#0",
      "002c625c00587f65c606904bb661d3021d45a97ed254d13f117895a5a5dddfba#0",
      "003dc4490c76a2aa7d3abe53a88918cdbee118fd5cf6436ea2c7e797abd03cdb#0"
    ]
  }
}
```

[test-json-rpc.http](./examples/test-json-rpc.http) contains additional example usage.
