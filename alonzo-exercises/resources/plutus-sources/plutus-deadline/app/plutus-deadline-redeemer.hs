
import Prelude
import System.Environment

import Cardano.Api
import Cardano.Api.Shelley

import Cardano.Ledger.Alonzo.Data qualified as Alonzo
import Plutus.V1.Ledger.Api qualified as Plutus

import Data.ByteString.Short qualified as SBS

import Cardano.PlutusDeadline.DeadlineRedeemer (deadlineScript, deadlineScriptShortBs)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptnum = if nargs > 0 then read (args!!0) else 42
  let scriptname = if nargs > 1 then args!!1 else  "deadline-redeemer.plutus"
  putStrLn $ "Writing output to: " ++ scriptname
  writePlutusScript scriptnum scriptname deadlineScript deadlineScriptShortBs



writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr   -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
