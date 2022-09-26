module Main (main) where

import Options.Applicative qualified as O
import Plutus.Script.Evaluation.Dump (dumpScriptEvents)
import Plutus.Script.Evaluation.Options (parserInfo)

{-
Example:

AWS_ACCESS_KEY_ID=<key> \
AWS_SECRET_ACCESS_KEY=<secret> \
AWS_DEFAULT_REGION=us-east-1 \
AWS_ENDPOINT_URL=https://s3.devx.iog.io \
S3_DUMP_DIR=s3://plutus/mainnet-script-dump-1-35-3/ \
  cabal v2-run plutus-script-evaluation-test:dump-script-events -- \
  --socket-path $HOME/mainnet/db/node.socket \
  --config $HOME/mainnet/mainnet-config.json \
  --mainnet \
  --blocks-per-file 50000 \
  --events-per-file 50000 \
  --dir $HOME/cardano-dump

-}

main :: IO ()
main = dumpScriptEvents =<< O.execParser parserInfo
