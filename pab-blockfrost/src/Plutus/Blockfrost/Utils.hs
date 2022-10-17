{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Plutus.Blockfrost.Utils where

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
import Ledger.Slot qualified as Ledger (Slot (..), SlotRange)
import Ledger.Tx (TxOutRef (..))
import Ledger.Tx qualified as LT (ScriptTag (..), TxId (TxId))
import Ledger.Tx.CardanoAPI
import Money (Approximation (Round), DecimalConf (..), SomeDiscrete, UnitScale, defaultDecimalConf, discreteToDecimal,
              scale, someDiscreteAmount, someDiscreteCurrency)
import Plutus.V1.Ledger.Address qualified as LA
import Plutus.V1.Ledger.Api (Credential (..), fromBuiltin, toBuiltin)
import Plutus.V1.Ledger.Api qualified (DatumHash, RedeemerHash)
import Plutus.V1.Ledger.Interval (always, from, interval, to)
import Plutus.V1.Ledger.Scripts qualified as PS
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

toBlockfrostTxHash :: LT.TxId -> TxHash
toBlockfrostTxHash = TxHash . pack . show

toBlockfrostTxHashes :: [LT.TxId] -> [TxHash]
toBlockfrostTxHashes = map toBlockfrostTxHash

toBlockfrostRef :: TxOutRef -> (TxHash, Integer)
toBlockfrostRef ref = (toBlockfrostTxHash (txOutRefId ref), txOutRefIdx ref)

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

textToScriptHash :: Text -> PS.ScriptHash
textToScriptHash = PS.ScriptHash . toBuiltin . fromJust . decodeHex

textToRedeemerHash :: Text -> PS.RedeemerHash
textToRedeemerHash = PS.RedeemerHash . toBuiltin . fromJust . decodeHex

toPlutusScriptTag :: ValidationPurpose -> LT.ScriptTag
toPlutusScriptTag = \case
    Spend  -> LT.Spend
    Mint   -> LT.Mint
    Cert   -> LT.Cert
    Reward -> LT.Reward

toPlutusAddress :: Blockfrost.Address -> Either String LA.Address
toPlutusAddress bAddr = case deserialized of
    Nothing  -> Left "Error deserializing the Address"
    Just des -> Right $ fromCardanoAddress des
  where
    deserialized :: Maybe (Api.Address ShelleyAddr)
    deserialized = deserialiseAddress AsShelleyAddress (unAddress bAddr)

credentialToAddress :: NetworkId -> Credential -> Blockfrost.Address
credentialToAddress netId c = case toCardanoAddressInEra netId pAddress of
    Left err   -> error $ show err
    Right addr -> mkAddress $ serialiseAddress addr
  where
    pAddress :: LA.Address
    pAddress = case c of
      PubKeyCredential pkh     -> LA.pubKeyHashAddress pkh
      ScriptCredential valHash -> LA.scriptHashAddress valHash

txHashToTxId :: TxHash -> LT.TxId
txHashToTxId = LT.TxId .toBuiltin . fromJust . decodeHex . unTxHash

utxoToRef :: AddressUtxo -> TxOutRef
utxoToRef utxo = TxOutRef { txOutRefId=utxoToTxId utxo
                          , txOutRefIdx=_addressUtxoOutputIndex utxo
                          }

utxoToTxId :: AddressUtxo -> LT.TxId
utxoToTxId = txHashToTxId . _addressUtxoTxHash

txoToRef :: UtxoInput -> TxOutRef
txoToRef txo = TxOutRef { txOutRefId=txoToTxId txo
                        , txOutRefIdx=_utxoInputOutputIndex txo
                        }

-- We are forced to use blockfrost-client v0.3.1 by the cardano-wallet.
-- In that version, _utxoInputTxHash returns a Text instead of a TxHash
txoToTxId :: UtxoInput -> LT.TxId
txoToTxId = txHashToTxId . _utxoInputTxHash

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

textToSlot :: Text -> Ledger.Slot
textToSlot = maybe (error "Failed to convert text to slot") Ledger.Slot . (readMaybe . unpack)

-- the functions "to", "from" and "interval" includes the parameters inside the validity range, meanwhile
-- blockfrost gives us an [) range, so we need to take one from the Right bound
toPlutusSlotRange :: Maybe Text -> Maybe Text -> Ledger.SlotRange
toPlutusSlotRange Nothing Nothing            = always
toPlutusSlotRange Nothing (Just after)       = to (textToSlot after - 1)
toPlutusSlotRange (Just before) Nothing      = from (textToSlot before)
toPlutusSlotRange (Just before) (Just after) = interval (textToSlot before) (textToSlot after - 1)
