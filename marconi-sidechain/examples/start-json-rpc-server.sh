#!/usr/bin/env sh

DB_DIR="~/dev/var/iohk/marconi/1675803710"
UTXO_FILE="utxodb"

DB="$DB_DIR/$UTXO_FILE"
mkdir -p $DB_DIR
ls -la $DB_DIR

## Noes
## We use db-utils-exe/exe/Main.hs to get a list of most re occuring addresses from utxo-db
## Therefor these addresses my not always be the best addresses to use.
##
cabal run examples-json-rpc-server -- \
    --utxo-db "$DB" \
    --addresses-to-index addr_test1wrpn0ad8rj3pgfpzae5tghpf325nyvh94zfkj3kzgvxzvcc2zuac6 \
    --addresses-to-index addr_test1vryusxht8rgz4g6twrjz4y8gss66w202vtfyk84wahmguzgh5mejc \
    --addresses-to-index addr_test1wpzewuy339m9l8ax7gz8g08q2j9478lgntzv9qhmwvy3hwg0cwsaf
