
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
| POST      | json-rpc  | utxoJsonReport         | Retrieves TxRefs for an address             |
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
curl -d '{"jsonrpc": "2.0" , "method": "utxoJsonReport" , "params": "addr_test1qzzxxkwnz4k60fjdjqspt58c8xe069kemfw4gljnnqtc4aarszs09x52vy8kfknj0rrr9400e39ufz5tuct74h52kcrqaytqk7", "id": 19}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc | jq

{
  "id": 19,
  "jsonrpc": "2.0",
  "result": {
    "urAddress": "addr_test1qzzxxkwnz4k60fjdjqspt58c8xe069kemfw4gljnnqtc4aarszs09x52vy8kfknj0rrr9400e39ufz5tuct74h52kcrqaytqk7",
    "urReport": [
      {
        "_urBlockHash": "8ccc256dda1f8c499dd91beb9e19e0a794463e876c5602b74c82997e31f16bde",
        "_urBlockNo": {
          "unBlockNo": 910611
        },
        "_urSlotNo": 41980,
        "_urUtxo": {
          "_address": "addr_test1qzzxxkwnz4k60fjdjqspt58c8xe069kemfw4gljnnqtc4aarszs09x52vy8kfknj0rrr9400e39ufz5tuct74h52kcrqaytqk7",
          "_datum": null,
          "_datumHash": null,
          "_inlineScript": null,
          "_inlineScriptHash": null,
          "_txId": "6e858a057b93315e066c15468689cbda13e59c2efb70471ad256938233f6174a",
          "_txIx": 0,
          "_value": {
            "lovelace": 200000000
          }
        }
      },
      {
        "_urBlockHash": "4af235480bc67b5e3d6678e749ef92b76744b5d95e6dba41e0c08f0cef620743",
        "_urBlockNo": {
          "unBlockNo": 911036
        },
        "_urSlotNo": 41992,
        "_urUtxo": {
          "_address": "addr_test1qzzxxkwnz4k60fjdjqspt58c8xe069kemfw4gljnnqtc4aarszs09x52vy8kfknj0rrr9400e39ufz5tuct74h52kcrqaytqk7",
          "_datum": null,
          "_datumHash": null,
          "_inlineScript": null,
          "_inlineScriptHash": null,
          "_txId": "7f698f247bb9d1294b757b36e029dedb18be4f18d788c924c01d7d8bec80bf2f",
          "_txIx": 0,
          "_value": {
            "lovelace": 2000000000
          }
        }
      }
    ]
  }
}

```

[test-json-rpc.http](./examples/test-json-rpc.http) contains additional example usage.
