## marconi-sidechain examples

This library demonstrates end-to-end examples of creating and executing a sample JSON-RPC server and client.

### Example JSON-RPC server

The main purpose of this example is to run marconi-sidechain in isolation without a need for cardano-node.

#### Usage

``` sh
$(cabal exec -- which  examples-json-rpc-server) --help
Usage: examples-json-rpc-server [-d|--utxo-db FILENAME]
                                (--addresses-to-index ARG)

Available options:
  -d,--utxo-db FILENAME    Path to the utxo database.
                           (default: "./.marconidb/4/utxo-db")
  --addresses-to-index ARG Becch32 Shelley addresses to index. i.e
                           "--address-to-index address-1 --address-to-index
                           address-2 ..."
  -h,--help                Show this help text
```

### Assumption

The --utxo-db points to a file that already contains utxo data.
