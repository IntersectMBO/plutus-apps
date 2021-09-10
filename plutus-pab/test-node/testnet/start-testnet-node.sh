#!/bin/bash

cabal exec cardano-node -- run \
    --config ./testnet-config.json \
    --topology ./testnet-topology.json \
    --database-path db \
    --socket-path /tmp/node-server.sock \
    --port 3003
