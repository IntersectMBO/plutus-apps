module Main (main) where

import Options.Applicative qualified as O
import Plutus.Script.Evaluation.Dump (dumpScriptEvents)
import Plutus.Script.Evaluation.Options (parserInfo)

{-
Example:

AWS_ACCESS_KEY_ID=plutus \
AWS_SECRET_ACCESS_KEY=plutuskey \
AWS_DEFAULT_REGION=us-east-1 \
AWS_ENDPOINT_URL=https://s3.devx.iog.io \
S3_DUMP_DIR=s3://plutus/mainnet-script-dump/ \
  cabal v2-run plutus-script-evaluation-test:dump-script-events -- \
  --socket-path $HOME/cardano/db/node.socket \
  --config $HOME/cardano/mainnet-config.json \
  --mainnet \
  --blocks-per-file 50000 \
  --events-per-file 50000 \
  --dir $HOME/cardano-dump

-}

main :: IO ()
main = dumpScriptEvents =<< O.execParser parserInfo
