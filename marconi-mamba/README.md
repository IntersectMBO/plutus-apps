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
cd ./marconi-mamba/examples
./start-json-rpc-server.sh
```
Note that the example json-rpc-server relies on test data which may change in future.

#### Execute the JSON-RPC client example

``` sh
 $(cabal exec -- which examples-jsonrpc-client)
```

## Experimenting with marconi-mamba

Use the CLI to configure the runtime time environment. To get a list of available options:

``` sh
$(cabal exec -- which marconi-mamba) --help

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


Here is a sample invocation :

``` sh
$(cabal exec -- which marconi-mamba) \
    --testnet-magic 2 \
    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
    --utxo-db "$DB/utxo-db" \
    --addresses-to-index addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc \
    --addresses-to-index addr_test1vp8cprhse9pnnv7f4l3n6pj0afq2hjm6f7r2205dz0583egagfjah \
    --addresses-to-index addr_test1wpzvcmq8yuqnnzerzv0u862hmc4tc8xlm74wtsqmh56tgpc3pvx0f \
    --addresses-to-index addr_test1vrvf7yfr2h79mtzqrpcn0ql98xrhs63k85w64u8py7709zsm6tsr6 \
    --addresses-to-index addr_test1vqeux7xwusdju9dvsj8h7mca9aup2k439kfmwy773xxc2hcu7zy99
```
Assumption:
A suitable version of cardano-node is running and the environment variable `CARDANO_NODE_SOCKET_PATH` is set.

#### Test with the rest-client

To test with the [rest-client](https://github.com/pashky/restclient.el)
- execute the JSON-RPC server example as outlined above
- execute the invidual tests in [test-with-rest-client.http](./examples/test-with-rest-client.http)
