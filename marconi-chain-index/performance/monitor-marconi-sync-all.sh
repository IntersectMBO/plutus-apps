#!/usr/bin/env bash

NETWORKID=764824073
NETWORK=mainnet

monitor_indexer () {
  indexer=$1
  echo "Syncing ${indexer} indexer..."
  case $indexer in
    utxo)
      indexers_cli="--disable-datum --disable-script-tx --disable-address-datum --disable-mintburn --disable-epoch-stakepool-size"
      ;;
    address-datum)
      indexers_cli="--disable-datum --disable-script-tx --disable-utxo --disable-mintburn --disable-epoch-stakepool-size"
      ;;
    mintburn)
      indexers_cli="--disable-datum --disable-script-tx --disable-utxo --disable-address-datum --disable-epoch-stakepool-size"
      ;;
    epochsdd)
      indexers_cli="--disable-datum --disable-script-tx --disable-utxo --disable-address-datum --disable-mintburn"
      ;;
  esac

  cabal build marconi-chain-index
  $(cabal exec -- which marconi-chain-index) \
    -s "$HOME"/cardano-node/${NETWORK}/cardano-node.socket \
    --testnet-magic ${NETWORKID} \
    ${indexers_cli} \
    -d ~/cardano-node/${NETWORK}/marconi-chain-index \
    --node-config-path "$HOME"/cardano-node/${NETWORK}/${NETWORK}-config.json >> marconi-chain-index-"${indexer}".log &
  pid=$!
  echo $pid
  ./monitor-marconi-sync.sh marconi-chain-index-"${indexer}".log $pid >> >(tee marconi-chain-index-"${indexer}"-monitor.log)

}

# Uncomment the ones you want to use
# monitor_indexer "utxo"
# monitor_indexer "address-datum"
# monitor_indexer "mintburn"
# monitor_indexer "epochsdd"

trap 'kill $(jobs -p)' EXIT
