import Prelude
import System.Environment

import Cardano.Api.Shelley

import Plutus.V1.Ledger.Api qualified as Plutus
import Plutus.V1.Ledger.Contexts

import Data.ByteString.Short qualified as SBS

import Cardano.PlutusExample.Sum (sumDataSBS, sumDataSerialised, sumWrappedSBS, sumWrappedSerialised)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptnum = if nargs > 0 then read (args!!0) else 42
  let scriptname = if nargs > 1 then args!!1 else  "result.plutus"
  writePlutusScript scriptnum scriptname sumDataSerialised sumDataSBS
  writePlutusScript scriptnum scriptname sumWrappedSerialised sumWrappedSBS

writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m -> let
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS
                              [ Plutus.toData scriptnum
                              , Plutus.toData (sum [1..scriptnum])
                              , Plutus.toData dummyContext ]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr   -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
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
