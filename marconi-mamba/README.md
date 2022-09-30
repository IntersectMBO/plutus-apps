marconi-mamba
--

This is an initial draft for the Marconi Mamba README file. Going forward, we will need to discuss and refine this document and/or the implementation so that they match.

## Description
marconi-mamba is a [JSON-RPC](http://www.simple-is-better.org/rpc/#differences-between-1-0-and-2-0) HTTP server on top of the [marconi](../marconi/README.md)

## Playing with the RPC
There is an example client/server JSON-RPC to experiment with the marconi-mamba. Here are the steps to build and execute the examples:

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

#### Test with the rest-client
To test with the [rest-client](https://github.com/pashky/restclient.el)
- execute the JSON-RPC server example as outlined above
- execute the invidual tests in [test-with-rest-client.http](./examples/test-with-rest-client.http)
