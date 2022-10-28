marconi-mamba
--

This is an initial draft for the Marconi Mamba README file. Going forward, we will need to discuss and refine this document and/or the implementation so that they match.

## Description
marconi-mamba is a [JSON-RPC](http://www.simple-is-better.org/rpc/#differences-between-1-0-and-2-0) HTTP server on top of the [marconi](../marconi/README.md)

## Playing with the JSON-RPC
There is an example client/server JSON-RPC to experiment with the JSON-RPC clien/server. Here are the steps to build and execute the examples:

#### Build the project

``` sh
cabal build all
cd marconi-mamba
cabal build
```

#### Execute the JSON-RPC server example

``` sh
$(cabal exec -- which examples-jsonrpc-server)
```

#### Execute the JSON-RPC client example

``` sh
 $(cabal exec -- which examples-jsonrpc-client)
```

## Experimenting with marconi-mamba

Use the CLI to configure the runtime time environment. To get a list of available options:

``` sh
$(cabal exec -- which marconi-mamba) --help
marconi-mamba - Cardano blockchain indexer

Usage: marconi-mamba --socket-path FILE --utxo-db FILE [--http-port HTTP-PORT]
                     (--mainnet | --testnet-magic NATURAL)
                     --addresses-to-index Address

Available options:
  --socket-path FILE       Socket path to node
  --utxo-db FILE           Path to the utxo database.
  --http-port HTTP-PORT    JSON-RPC http port number, default is port 3000.
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --addresses-to-index Address
                           White space separated list of addresses to index. i.e
                           "address-1 address-2 address-3 ..."
  -h,--help                Show this help text
```


Here is a sample invocation :

``` sh
$(cabal exec -- which marconi-mamba)
    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
    --testnet-magic 2 \
    --utxo-db "$CONFIG_DIR/.dbMarconi/utxo.sqlite3" \
    --addresses-to-index  ``"addr1q837de0y7j3ncegph2a8mc0e86q9evwtekd3ejhlpr97wclrumj7fa9r83jsrw460hslj05qtjcuhnvmrn907zxtua3skv7yyl"
```

#### Test with the rest-client

To test with the [rest-client](https://github.com/pashky/restclient.el)
- execute the JSON-RPC server example as outlined above
- execute the invidual tests in [test-with-rest-client.http](./examples/test-with-rest-client.http)
