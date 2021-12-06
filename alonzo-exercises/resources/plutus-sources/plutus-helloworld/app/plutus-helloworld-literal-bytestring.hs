import Data.String
import Prelude
import System.Environment

import Cardano.Api

import Plutus.V1.Ledger.Api qualified as Plutus

import Data.ByteString.Short qualified as SBS

import Cardano.PlutusExample.HelloWorldLiteralByteString (helloWorldSBS, helloWorldSerialised)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptData = if nargs > 0 then fromString (args!!0) else "Hello World!"
  let scriptname = if nargs > 1 then args!!1 else  "result.plutus"
  putStrLn $ "Writing output to: " ++ scriptname
  writePlutusScript scriptData scriptname helloWorldSerialised helloWorldSBS

writePlutusScript :: Plutus.BuiltinByteString -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptData filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS
                              [ Plutus.toData scriptData
                              , Plutus.toData ()
                              , Plutus.toData ()
                              ]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr   -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
