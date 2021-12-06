import Prelude

import Cardano.Api
import Cardano.Api.Shelley

import Cardano.Ledger.Alonzo.Data qualified as Alonzo
import Plutus.V1.Ledger.Api qualified as Plutus

import Cardano.PlutusExample.MintingScriptPurple (apiExamplePlutusMintingScriptPurple, mintingScriptPurpleShortBs)

main :: IO ()
main = do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber 42)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m mintingScriptPurpleShortBs [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr   -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope "minting-policy-purple.plutus" Nothing apiExamplePlutusMintingScriptPurple
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
