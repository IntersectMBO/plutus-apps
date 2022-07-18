{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Plutus.Blockfrost.Utils where

import Data.Aeson
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.String
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text (drop, take)
import Text.Hex (decodeHex, encodeHex)
import Text.Read (readMaybe)

import Blockfrost.Client as Blockfrost
import Cardano.Api hiding (AssetId, Block, Value)
import Cardano.Api.Shelley qualified as Api
import Ledger.Tx (TxOutRef (..))
import Ledger.Tx.CardanoAPI
import Money (Approximation (Round), DecimalConf (..), SomeDiscrete, UnitScale, defaultDecimalConf, discreteToDecimal,
              scale, someDiscreteAmount, someDiscreteCurrency)
import Plutus.V1.Ledger.Address qualified as LA
import Plutus.V1.Ledger.Api (Credential (..), adaSymbol, adaToken, fromBuiltin, toBuiltin)
import Plutus.V1.Ledger.Api qualified (DatumHash, RedeemerHash)
import Plutus.V1.Ledger.Scripts qualified as PS
import Plutus.V1.Ledger.TxId qualified as Ledger
import Plutus.V1.Ledger.Value hiding (Value)
import Plutus.V1.Ledger.Value qualified as Ledger (Value)



class Show a => ToBlockfrostScriptHash a where
  toBlockfrostScriptHash :: a -> Blockfrost.ScriptHash
  toBlockfrostScriptHash = fromString . show

instance ToBlockfrostScriptHash PS.ValidatorHash
instance ToBlockfrostScriptHash PS.MintingPolicyHash
instance ToBlockfrostScriptHash PS.StakeValidatorHash

class Show a => ToBlockfrostDatumHash a where
  toBlockfrostDatumHash :: a -> Blockfrost.DatumHash
  toBlockfrostDatumHash = fromString . show

instance ToBlockfrostDatumHash Plutus.V1.Ledger.Api.DatumHash
instance ToBlockfrostDatumHash Plutus.V1.Ledger.Api.RedeemerHash

fromSucceed :: Result a -> a
fromSucceed (Error a)   = error $ show a
fromSucceed (Success a) = a

toBlockfrostTxHash :: TxOutRef -> TxHash
toBlockfrostTxHash = TxHash . pack . show . txOutRefId

toBlockfrostRef :: TxOutRef -> (TxHash, Integer)
toBlockfrostRef ref = (toBlockfrostTxHash ref, txOutRefIdx ref)

toBlockfrostAssetId :: AssetClass -> AssetId
toBlockfrostAssetId ac = fromString (polId ++ tName)
  where
    (cs, tn) = unAssetClass ac

    polId :: String
    polId = (unpack . encodeHex . fromBuiltin . unCurrencySymbol) cs

    tName :: String
    tName = (unpack . encodeHex . fromBuiltin . unTokenName) tn

textToDatumHash :: Text -> PS.DatumHash
textToDatumHash = PS.DatumHash . toBuiltin . fromJust . decodeHex

toPlutusAddress :: Blockfrost.Address -> Either String LA.Address
toPlutusAddress bAddr = case deserialized of
    Nothing -> Left "Error deserializing the Address"
    Just des -> case fromCardanoAddress (Api.shelleyAddressInEra @ShelleyEra des) of
        Left err   -> Left ("Error parsing address " ++ show err)
        Right addr -> Right addr
  where
    deserialized :: Maybe (Api.Address ShelleyAddr)
    deserialized = deserialiseAddress AsShelleyAddress (unAddress bAddr)

credentialToAddress :: NetworkId -> Credential -> Blockfrost.Address
credentialToAddress netId c = case toCardanoAddress netId pAddress of
    Left err   -> error $ show err
    Right addr -> mkAddress $ serialiseAddress addr
  where
    pAddress :: LA.Address
    pAddress = case c of
      PubKeyCredential pkh     -> LA.pubKeyHashAddress pkh
      ScriptCredential valHash -> LA.scriptHashAddress valHash

utxoToRef :: AddressUtxo -> TxOutRef
utxoToRef utxo = TxOutRef { txOutRefId=utxoToTxId utxo
                          , txOutRefIdx=_addressUtxoOutputIndex utxo
                          }

utxoToTxId :: AddressUtxo -> Ledger.TxId
utxoToTxId utxo =
    Ledger.TxId $ toBuiltin $ fromJust $ decodeHex $ unTxHash $ _addressUtxoTxHash utxo

amountsToValue :: [Blockfrost.Amount] -> Ledger.Value
amountsToValue = foldr ((<>). blfAmountToValue) (singleton "" "" 0)

blfAmountToValue :: Blockfrost.Amount -> Ledger.Value
blfAmountToValue amt = case amt of
                          AdaAmount lov  -> lovelacesToValue lov
                          AssetAmount ds -> discreteCurrencyToValue ds

discreteCurrencyToValue :: Money.SomeDiscrete -> Ledger.Value
discreteCurrencyToValue sd = singleton pid tn quant
  where
    pid :: CurrencySymbol
    pid = fromString $ unpack $ Text.take 56 $ someDiscreteCurrency sd

    tn :: TokenName
    tn =  TokenName $ toBuiltin $ fromJust $ decodeHex $ Text.drop 56 $ someDiscreteCurrency sd

    quant :: Integer
    quant = someDiscreteAmount sd

lovelaceConfig :: Money.DecimalConf
lovelaceConfig = Money.defaultDecimalConf
  { Money.decimalConf_digits = 0
  , Money.decimalConf_scale =
        Money.scale (Proxy @(Money.UnitScale "ADA" "lovelace"))
  }

lovelacesToMInt :: Lovelaces -> Maybe Integer
lovelacesToMInt = readMaybe . unpack . Money.discreteToDecimal lovelaceConfig Money.Round

lovelacesToValue :: Lovelaces -> Ledger.Value
lovelacesToValue lov = case lovelacesToMInt lov of
  Nothing  -> singleton adaSymbol adaToken 0
  Just int -> singleton adaSymbol adaToken int
