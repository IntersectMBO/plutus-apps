marconi-mamba
--

marconi-mamba is a [JSON-RPC](http://www.simple-is-better.org/rpc/#differences-between-1-0-and-2-0) over HTTP built on top of the [marconi](../marconi/README.md).

## What is it

The purpose of marconi-mamba is to make the core Marcoin APIs available to non-Haskell applications.

## How do I use it
* Building from source
* Using marconi-mamba

### Requirements
* local instance of cardano-node, version 1.35.3
* suitable environment for plutus-platform development, [see plutus-starter](https://github.com/input-output-hk/plutus-starter) for detail.

### Building from source
To build from source we assume you have a suitable environment for plutus-platform development, [see plutus-starter](https://github.com/input-output-hk/plutus-starter) for detail.

``` sh
git clone git@github.com:input-output-hk/plutus-apps.git
nix-shell
cabal clean
cabal update
cabal build marconi-mamba
```
The above process will build the executalbe in your local environment at:

``` sh
 cabal exec -- which marconi-mamba

```

### Using marconi-mamba

#### Command line summary

The general synopsis is as follows:

``` sh
$(cabal exec -- which marconi-mamba) --help
$(cabal exec -- which marconi-mamba) --help
marconi-mamba - Cardano blockchain indexer

Usage: marconi-mamba --socket-path FILE --utxo-db FILE [--http-port HTTP-PORT]
                     (--mainnet | --testnet-magic NATURAL)
                     (--addresses-to-index ARG)

Available options:
  --socket-path FILE       Socket path to node
  --utxo-db FILE           Path to the utxo database.
  --http-port HTTP-PORT    JSON-RPC http port number, default is port 3000.
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --addresses-to-index ARG Becch32 Shelley addresses to index. i.e
                           "--address-to-index address-1 --address-to-index
                           address-2 ..."
  -h,--help                Show this help text
```

#### Example usage

The following is a an example shell script for executing marconi-mamba in [preview-testnet](https://book.world.dev.cardano.org/environments.html#preview-testnet)
**Assumption**
cardano-node version 1.35.3 is running with the socket-path as outlined below:

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

To interact with the server:

``` sh
curl -d '{"jsonrpc": "2.0" , "method": "utxoTxOutReport" , "params": "addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc"  , "id": 12}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc

```
[test-json-rpc.http](./marconi-mamba/examples/test-json-rpc.http) contains additional example usage
