module Main (main) where

import Options.Applicative qualified as O
import Plutus.Script.Evaluation.Dump (dumpScriptEvents)
import Plutus.Script.Evaluation.Options (parserInfo)

{-
Example:

cabal v2-run plutus-script-evaluation-test:dump-script-events -- \
  --socket-path $HOME/cardano/db/node.socket \
  --config $HOME/cardano/mainnet-config.json \
  --mainnet \
  --blocks-per-file 2000 \
  --dir $HOME/cardano-dump

-}

main :: IO ()
main = dumpScriptEvents =<< O.execParser parserInfo
