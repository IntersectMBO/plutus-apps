#!/usr/bin/env bash

# Run a mainnet Cardano node.
# Example usage:
#
# NODE_DIR=../cardano-node \
#   ./scripts/cardano-node.sh

set -euo pipefail

node_config_files=(
  "config.json"
  "db-sync-config.json"
  "submit-api-config.json"
  "topology.json"
  "byron-genesis.json"
  "shelley-genesis.json"
  "alonzo-genesis.json"
)

trap 'kill $cardano_node_pid; exit' INT TERM QUIT ERR EXIT

mkdir -p "$NODE_DIR"
# Download config files
for x in "${node_config_files[@]}"; do
  wget -nc https://book.world.dev.cardano.org/environments/mainnet/"$x" -P "$NODE_DIR"
done

# Launch node
cardano-node run \
  --config "$NODE_DIR"/config.json \
  --topology "$NODE_DIR"/topology.json \
  --database-path "$NODE_DIR"/db/ \
  --socket-path "$NODE_DIR"/db/node.socket \
  --host-addr 127.0.0.1 \
  --port 1337 &

cardano_node_pid=$!
wait $cardano_node_pid
