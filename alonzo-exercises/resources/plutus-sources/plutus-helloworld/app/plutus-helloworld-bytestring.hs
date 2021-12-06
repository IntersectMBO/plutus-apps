
import Prelude
import System.Environment

import Cardano.Api
import Cardano.Api.Shelley

import Data.Aeson (encode)
import Data.ByteString.Short qualified as SBS

import Plutus.V1.Ledger.Api qualified as Plutus
import Plutus.V1.Ledger.Contexts

import PlutusTx.Prelude as P (BuiltinByteString)

import Cardano.PlutusExample.HelloWorldByteStringParametric (hello, helloWorldSBS, helloWorldSerialised)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptname = if nargs > 1 then args!!1 else  "result.plutus"
  putStrLn $ "Writing output to: " ++ scriptname
  writePlutusScript hello scriptname helloWorldSerialised helloWorldSBS



writePlutusScript :: P.BuiltinByteString -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript datum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let
            pData = Plutus.toData datum
            (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS
                              [ pData
                              , Plutus.toData ()
                              , Plutus.toData dummyContext ]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr   -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
                print $ "Datum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData pData)
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

dummyContext :: ScriptContext
dummyContext = ScriptContext dummyTxInfo (Spending dummyOutRef)
  where
    dummyOutRef :: TxOutRef
    dummyOutRef = TxOutRef (Plutus.TxId "") 0
    dummyTxInfo :: TxInfo
    dummyTxInfo = TxInfo
      { txInfoInputs = []
      , txInfoOutputs = []
      , txInfoFee = mempty
      , txInfoMint = mempty
      , txInfoDCert = []
      , txInfoWdrl = []
      , txInfoValidRange = Plutus.always
      , txInfoSignatories = []
      , txInfoData = []
      , txInfoId = Plutus.TxId ""
      }
