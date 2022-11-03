{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}

{-# OPTIONS_GHC -Wno-orphans    #-}

{-|

Interface to the transaction types from 'cardano-api'

-}
module Ledger.Tx.CardanoAPI.Internal(
  CardanoBuildTx(..)
  , SomeCardanoApiTx(..)
  , txOutRefs
  , unspentOutputsTx
  , fromCardanoTxId
  , fromCardanoTxIn
  , fromCardanoTxOutToPV1TxInfoTxOut
  , fromCardanoTxOutToPV2TxInfoTxOut
  , fromCardanoTxOutDatumHash
  , fromCardanoTxOutDatum
  , fromCardanoTxOutValue
  , fromCardanoAddressInEra
  , fromCardanoAddress
  , fromCardanoAssetId
  , fromCardanoAssetName
  , fromCardanoMintValue
  , fromCardanoValue
  , fromCardanoPolicyId
  , fromCardanoFee
  , fromCardanoValidityRange
  , fromCardanoScriptInEra
  , fromCardanoPaymentKeyHash
  , fromCardanoScriptData
  , fromCardanoPlutusScript
  , fromCardanoScriptInAnyLang
  , fromCardanoLovelace
  , fromTxScriptValidity
  , toTxScriptValidity
  , scriptDataFromCardanoTxBody
  , plutusScriptsFromTxBody
  , makeTransactionBody
  , toCardanoTxIn
  , toCardanoTxOut
  , toCardanoTxOutDatum
  , toCardanoTxOutDatumHash
  , toCardanoTxOutDatumHashFromDatum
  , toCardanoTxOutDatumInline
  , toCardanoTxOutDatumInTx
  , toCardanoTxOutNoDatum
  , toCardanoTxOutValue
  , toCardanoAddressInEra
  , toCardanoAssetId
  , toCardanoAssetName
  , toCardanoPolicyId
  , toCardanoValue
  , toCardanoLovelace
  , toCardanoFee
  , adaToCardanoValue
  , toCardanoValidityRange
  , toCardanoScriptInEra
  , toCardanoPaymentKeyHash
  , toCardanoScriptData
  , toCardanoScriptDataHash
  , toCardanoScriptHash
  , toCardanoStakeKeyHash
  , toCardanoPlutusScript
  , toCardanoScriptInAnyLang
  , toCardanoReferenceScript
  , toCardanoTxId
  , ToCardanoError(..)
  , FromCardanoError(..)
  , deserialiseFromRawBytes
  , zeroExecutionUnits
  , tag
  , withIsCardanoEra) where

import Cardano.Api qualified as C
import Cardano.Api.Byron qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Data.Tracer (ToObject)
import Cardano.Chain.Common (addrToBase58)
import Cardano.Ledger.Alonzo.Language qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Alonzo.TxWitness qualified as Alonzo
import Cardano.Ledger.Core qualified as Ledger
import Codec.Serialise (Serialise, deserialiseOrFail)
import Codec.Serialise qualified as Codec
import Codec.Serialise.Decoding (Decoder, decodeBytes, decodeSimple)
import Codec.Serialise.Encoding (Encoding (Encoding), Tokens (TkBytes, TkSimple))
import Control.Applicative ((<|>))
import Control.Lens ((&), (.~), (<&>), (?~))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseFail, prependFailure, typeMismatch)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.OpenApi (NamedSchema (NamedSchema), OpenApiType (OpenApiObject), byteSchema, declareSchemaRef, properties,
                     required, sketchSchema, type_)
import Data.OpenApi qualified as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Ledger.Ada qualified as Ada
import Ledger.Ada qualified as P
import Ledger.Address qualified as P
import Ledger.Scripts qualified as P
import Ledger.Slot qualified as P
import Ledger.Tx.CardanoAPITemp (makeTransactionBody')
import Plutus.Script.Utils.V1.Scripts qualified as PV1
import Plutus.Script.Utils.V2.Scripts qualified as PV2
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Credential qualified as Credential
import Plutus.V1.Ledger.Tx qualified as PV1
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty (pretty), colon, viaShow, (<+>))

newtype CardanoBuildTx = CardanoBuildTx { getCardanoBuildTx :: C.TxBodyContent C.BuildTx C.BabbageEra }
  deriving (Eq, Show)

instance ToJSON CardanoBuildTx where
  toJSON = error "TODO: ToJSON CardanoBuildTx"

instance FromJSON CardanoBuildTx where
  parseJSON _ = parseFail "TODO: FromJSON CardanoBuildTx"

instance OpenApi.ToSchema CardanoBuildTx where
  -- TODO: implement the schema
  declareNamedSchema _ = return $ NamedSchema (Just "CardanoBuildTx") mempty

instance (Typeable era, Typeable mode) => OpenApi.ToSchema (C.EraInMode era mode) where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "EraInMode") $ sketchSchema C.BabbageEraInCardanoMode

instance (Typeable era) => OpenApi.ToSchema (C.Tx era) where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "Tx") byteSchema

-- | Cardano tx from any era.
data SomeCardanoApiTx where
  SomeTx :: C.IsCardanoEra era => C.Tx era -> C.EraInMode era C.CardanoMode -> SomeCardanoApiTx

instance Eq SomeCardanoApiTx where
  (SomeTx tx1 C.ByronEraInCardanoMode) == (SomeTx tx2 C.ByronEraInCardanoMode)     = tx1 == tx2
  (SomeTx tx1 C.ShelleyEraInCardanoMode) == (SomeTx tx2 C.ShelleyEraInCardanoMode) = tx1 == tx2
  (SomeTx tx1 C.AllegraEraInCardanoMode) == (SomeTx tx2 C.AllegraEraInCardanoMode) = tx1 == tx2
  (SomeTx tx1 C.MaryEraInCardanoMode) == (SomeTx tx2 C.MaryEraInCardanoMode)       = tx1 == tx2
  (SomeTx tx1 C.AlonzoEraInCardanoMode) == (SomeTx tx2 C.AlonzoEraInCardanoMode)   = tx1 == tx2
  (SomeTx tx1 C.BabbageEraInCardanoMode) == (SomeTx tx2 C.BabbageEraInCardanoMode) = tx1 == tx2
  _ == _                                                                           = False

deriving instance Show SomeCardanoApiTx

instance Serialise SomeCardanoApiTx where
  encode (SomeTx tx eraInMode) = encodedMode eraInMode <> Encoding (TkBytes (C.serialiseToCBOR tx))
    where
      encodedMode :: C.EraInMode era C.CardanoMode -> Encoding
      -- 0 and 1 are for ByronEraInByronMode and ShelleyEraInShelleyMode
      encodedMode C.ByronEraInCardanoMode   = Encoding (TkSimple 2)
      encodedMode C.ShelleyEraInCardanoMode = Encoding (TkSimple 3)
      encodedMode C.AllegraEraInCardanoMode = Encoding (TkSimple 4)
      encodedMode C.MaryEraInCardanoMode    = Encoding (TkSimple 5)
      encodedMode C.AlonzoEraInCardanoMode  = Encoding (TkSimple 6)
      encodedMode C.BabbageEraInCardanoMode = Encoding (TkSimple 7)
  decode = do
    w <- decodeSimple
    case w of
      2 -> decodeTx C.AsByronEra C.ByronEraInCardanoMode
      3 -> decodeTx C.AsShelleyEra C.ShelleyEraInCardanoMode
      4 -> decodeTx C.AsAllegraEra C.AllegraEraInCardanoMode
      5 -> decodeTx C.AsMaryEra C.MaryEraInCardanoMode
      6 -> decodeTx C.AsAlonzoEra C.AlonzoEraInCardanoMode
      7 -> decodeTx C.AsBabbageEra C.BabbageEraInCardanoMode
      _ -> fail "Unexpected value while decoding Cardano.Api.EraInMode"
    where
      decodeTx :: C.IsCardanoEra era => C.AsType era -> C.EraInMode era C.CardanoMode -> Decoder s SomeCardanoApiTx
      decodeTx asType eraInMode = do
        bytes <- decodeBytes
        tx <- either (const $ fail "Failed to decode Cardano.Api.Tx") pure $ C.deserialiseFromCBOR (C.AsTx asType) bytes
        pure $ SomeTx tx eraInMode

instance ToJSON SomeCardanoApiTx where
  toJSON (SomeTx tx eraInMode) =
    object [ "tx" .= C.serialiseToTextEnvelope Nothing tx
           , "eraInMode" .= eraInMode
           ]

-- | Converting 'SomeCardanoApiTx' to JSON.
--
-- If the "tx" field is from an unknown era, the JSON parser will print an
-- error at runtime while parsing.
instance FromJSON SomeCardanoApiTx where
  parseJSON v = parseByronInCardanoModeTx v
            <|> parseShelleyEraInCardanoModeTx v
            <|> parseAllegraEraInCardanoModeTx v
            <|> parseMaryEraInCardanoModeTx v
            <|> parseAlonzoEraInCardanoModeTx v
            <|> parseBabbageEraInCardanoModeTx v
            <|> parseEraInCardanoModeFail v

-- | Run code that needs an `IsCardanoEra` constraint while you only have an `EraInMode` value.
withIsCardanoEra :: C.EraInMode era C.CardanoMode -> (C.IsCardanoEra era => r) -> r
withIsCardanoEra C.ByronEraInCardanoMode r   = r
withIsCardanoEra C.ShelleyEraInCardanoMode r = r
withIsCardanoEra C.AllegraEraInCardanoMode r = r
withIsCardanoEra C.MaryEraInCardanoMode r    = r
withIsCardanoEra C.AlonzoEraInCardanoMode r  = r
withIsCardanoEra C.BabbageEraInCardanoMode r = r

parseByronInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseByronInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse ByronEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsByronEra)

parseShelleyEraInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseShelleyEraInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse ShelleyEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsShelleyEra)

parseMaryEraInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseMaryEraInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse MaryEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsMaryEra)

parseAllegraEraInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseAllegraEraInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse AllegraEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsAllegraEra)

parseAlonzoEraInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseAlonzoEraInCardanoModeTx =
  parseSomeCardanoTx "Failed to parse AlonzoEra 'tx' field from SomeCardanoApiTx"
                     (C.AsTx C.AsAlonzoEra)

-- TODO Uncomment the implementation once Cardano.Api adds a FromJSON instance
-- for 'EraInMode BabbageEra CardanoMode':
-- https://github.com/input-output-hk/cardano-node/pull/3837
parseBabbageEraInCardanoModeTx :: Aeson.Value -> Parser SomeCardanoApiTx
parseBabbageEraInCardanoModeTx (Aeson.Object v) =
    SomeTx
    <$> (v .: "tx" >>= \envelope -> either (const $ parseFail "Failed to parse BabbageEra 'tx' field from SomeCardanoApiTx")
                                           pure
                                           $ C.deserialiseFromTextEnvelope (C.AsTx C.AsBabbageEra) envelope)
    <*> pure C.BabbageEraInCardanoMode -- This is a workaround that only works because we tried all other eras first
parseBabbageEraInCardanoModeTx invalid =
  prependFailure "parsing SomeCardanoApiTx failed, "
      (typeMismatch "Object" invalid)
  -- parseSomeCardanoTx "Failed to parse BabbageEra 'tx' field from SomeCardanoApiTx"
  --                    (C.AsTx C.AsBabbageEra)

parseEraInCardanoModeFail :: Aeson.Value -> Parser SomeCardanoApiTx
parseEraInCardanoModeFail _ = fail "Unable to parse 'eraInMode'"

parseSomeCardanoTx
  :: ( FromJSON (C.EraInMode era C.CardanoMode)
     , C.IsCardanoEra era
     )
  => String
  -> C.AsType (C.Tx era)
  -> Aeson.Value
  -> Parser SomeCardanoApiTx
parseSomeCardanoTx errorMsg txAsType (Aeson.Object v) =
  SomeTx
    <$> (v .: "tx" >>= \envelope -> either (const $ parseFail errorMsg)
                                           pure
                                           $ C.deserialiseFromTextEnvelope txAsType envelope)
    <*> v .: "eraInMode"
parseSomeCardanoTx _ _ invalid =
    prependFailure "parsing SomeCardanoApiTx failed, "
      (typeMismatch "Object" invalid)

instance OpenApi.ToSchema SomeCardanoApiTx where
  declareNamedSchema _ = do
    txSchema <- declareSchemaRef (Proxy :: Proxy (C.Tx C.BabbageEra))
    eraInModeSchema <- declareSchemaRef (Proxy :: Proxy (C.EraInMode C.BabbageEra C.CardanoMode))
    return $ NamedSchema (Just "SomeCardanoApiTx") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~
          [ ("tx", txSchema)
          , ("eraInMode", eraInModeSchema)
          ]
      & required .~ [ "tx", "eraInMode" ]

txOutRefs :: SomeCardanoApiTx -> [(PV1.TxOut, PV1.TxOutRef)]
txOutRefs (SomeTx (C.Tx txBody@(C.TxBody C.TxBodyContent{..}) _) _) =
  mkOut <$> zip [0..] plutusTxOuts
  where
    mkOut (i, o) = (o, PV1.TxOutRef (fromCardanoTxId $ C.getTxId txBody) i)
    plutusTxOuts = fromCardanoTxOutToPV1TxInfoTxOut <$> txOuts

unspentOutputsTx :: SomeCardanoApiTx -> Map PV1.TxOutRef PV1.TxOut
unspentOutputsTx tx = Map.fromList $ swap <$> txOutRefs tx

-- | Given a 'C.TxScriptValidity era', if the @era@ supports scripts, return a
-- @True@ or @False@ depending on script validity. If the @era@ does not support
-- scripts, always return @True@.
fromTxScriptValidity :: C.TxScriptValidity era -> Bool
fromTxScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptValid)    = True
fromTxScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptInvalid)  = False
fromTxScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptValid)   = True
fromTxScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptInvalid) = False
fromTxScriptValidity C.TxScriptValidityNone                                                       = True

toTxScriptValidity :: C.ShelleyBasedEra era -> Bool -> C.TxScriptValidity era
toTxScriptValidity C.ShelleyBasedEraAlonzo True  = C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptValid
toTxScriptValidity C.ShelleyBasedEraAlonzo False = C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptInvalid
toTxScriptValidity C.ShelleyBasedEraBabbage True  = C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptValid
toTxScriptValidity C.ShelleyBasedEraBabbage False = C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptInvalid
toTxScriptValidity _ _ = C.TxScriptValidityNone

-- | Given a 'C.TxBody from a 'C.Tx era', return the datums and redeemers along
-- with their hashes.
scriptDataFromCardanoTxBody
  :: C.TxBody era
  -> (Map P.DatumHash P.Datum, PV1.Redeemers)
scriptDataFromCardanoTxBody C.ByronTxBody {} = (mempty, mempty)
scriptDataFromCardanoTxBody (C.ShelleyTxBody _ _ _ C.TxBodyNoScriptData _ _) =
  (mempty, mempty)
scriptDataFromCardanoTxBody
  (C.ShelleyTxBody _ _ _ (C.TxBodyScriptData _ (Alonzo.TxDats' dats) (Alonzo.Redeemers' reds)) _ _) =

  let datums = Map.fromList
             $ fmap ( (\d -> (P.datumHash d, d))
                    . P.Datum
                    . fromCardanoScriptData
                    . C.fromAlonzoData
                    )
             $ Map.elems dats
      redeemers = Map.fromList
                $ map (\(ptr, rdmr) ->
                        ( redeemerPtrFromCardanoRdmrPtr ptr
                        , P.Redeemer
                         $ fromCardanoScriptData
                         $ C.fromAlonzoData
                         $ fst rdmr
                        )
                      )
                $ Map.toList reds
   in (datums, redeemers)

redeemerPtrFromCardanoRdmrPtr :: Alonzo.RdmrPtr -> PV1.RedeemerPtr
redeemerPtrFromCardanoRdmrPtr (Alonzo.RdmrPtr rdmrTag ptr) = PV1.RedeemerPtr t (toInteger ptr)
  where
    t = case rdmrTag of
      Alonzo.Spend -> PV1.Spend
      Alonzo.Mint  -> PV1.Mint
      Alonzo.Cert  -> PV1.Cert
      Alonzo.Rewrd -> PV1.Reward

-- | Extract plutus scripts from a Cardano API tx body.
--
-- Note that Plutus scripts are only supported in Alonzo era and onwards.
plutusScriptsFromTxBody :: C.TxBody era -> Map P.ScriptHash (P.Versioned P.Script)
plutusScriptsFromTxBody C.ByronTxBody {} = mempty
plutusScriptsFromTxBody (C.ShelleyTxBody shelleyBasedEra _ scripts _ _ _) =
  Map.fromList $ mapMaybe (fromLedgerScript shelleyBasedEra) scripts
--
-- | Convert a script from a Cardano api in shelley based era to a Plutus script along with it's hash.
--
-- Note that Plutus scripts are only supported in Alonzo era and onwards.
fromLedgerScript
  :: C.ShelleyBasedEra era
  -> Ledger.Script (C.ShelleyLedgerEra era)
  -> Maybe (P.ScriptHash, P.Versioned P.Script)
fromLedgerScript C.ShelleyBasedEraShelley _      = Nothing
fromLedgerScript C.ShelleyBasedEraAllegra _      = Nothing
fromLedgerScript C.ShelleyBasedEraMary _         = Nothing
fromLedgerScript C.ShelleyBasedEraAlonzo script  = fromLedgerPlutusScript script
fromLedgerScript C.ShelleyBasedEraBabbage script = fromLedgerPlutusScript script

-- | Convert a `cardano-ledger` Plutus script from the Alonzo era and onwards to
-- a 'Script' along with it's hash.
fromLedgerPlutusScript :: Alonzo.Script a -> Maybe (P.ScriptHash, P.Versioned P.Script)
fromLedgerPlutusScript Alonzo.TimelockScript {} = Nothing
fromLedgerPlutusScript (Alonzo.PlutusScript Alonzo.PlutusV1 bs) =
  let script = fmap (\s -> (PV1.scriptHash s, P.Versioned s P.PlutusV1))
             $ deserialiseOrFail
             $ BSL.fromStrict
             $ SBS.fromShort bs
   in either (const Nothing) Just script
fromLedgerPlutusScript (Alonzo.PlutusScript Alonzo.PlutusV2 bs) =
  let script = fmap (\s -> (PV2.scriptHash s, P.Versioned s P.PlutusV2))
             $ deserialiseOrFail
             $ BSL.fromStrict
             $ SBS.fromShort bs
   in either (const Nothing) Just script

makeTransactionBody
    :: Map Alonzo.RdmrPtr Alonzo.ExUnits
    -> CardanoBuildTx
    -> Either ToCardanoError (C.TxBody C.BabbageEra)
makeTransactionBody exUnits (CardanoBuildTx txBodyContent) =
  first (TxBodyError . C.displayError) $ makeTransactionBody' exUnits txBodyContent

fromCardanoTxIn :: C.TxIn -> PV1.TxOutRef
fromCardanoTxIn (C.TxIn txId (C.TxIx txIx)) = PV1.TxOutRef (fromCardanoTxId txId) (toInteger txIx)

toCardanoTxIn :: PV1.TxOutRef -> Either ToCardanoError C.TxIn
toCardanoTxIn (PV1.TxOutRef txId txIx) = C.TxIn <$> toCardanoTxId txId <*> pure (C.TxIx (fromInteger txIx))

fromCardanoTxId :: C.TxId -> PV1.TxId
fromCardanoTxId txId = PV1.TxId $ PlutusTx.toBuiltin $ C.serialiseToRawBytes txId

toCardanoTxId :: PV1.TxId -> Either ToCardanoError C.TxId
toCardanoTxId (PV1.TxId bs) =
    tag "toCardanoTxId"
    $ deserialiseFromRawBytes C.AsTxId $ PlutusTx.fromBuiltin bs

-- TODO Handle reference script once 'P.TxOut' supports it (or when we use
-- exclusively 'C.TxOut' in all the codebase).
fromCardanoTxOutToPV1TxInfoTxOut :: C.TxOut C.CtxTx era -> PV1.TxOut
fromCardanoTxOutToPV1TxInfoTxOut (C.TxOut addr value datumHash _) =
    PV1.TxOut
    (fromCardanoAddressInEra addr)
    (fromCardanoTxOutValue value)
    (fromCardanoTxOutDatumHash datumHash)

fromCardanoTxOutToPV2TxInfoTxOut :: C.TxOut C.CtxTx era -> PV2.TxOut
fromCardanoTxOutToPV2TxInfoTxOut (C.TxOut addr value datum refScript) =
    PV2.TxOut
    (fromCardanoAddressInEra addr)
    (fromCardanoTxOutValue value)
    (fromCardanoTxOutDatum datum)
    (refScriptToScriptHash refScript)

refScriptToScriptHash :: C.ReferenceScript era -> Maybe PV2.ScriptHash
refScriptToScriptHash C.ReferenceScriptNone = Nothing
refScriptToScriptHash (C.ReferenceScript _ (C.ScriptInAnyLang _ s)) =
    let (PV2.ValidatorHash h) = fromCardanoScriptHash $ C.hashScript s
     in Just $ PV2.ScriptHash h

toCardanoTxOut
    :: C.NetworkId
    -> (PV2.OutputDatum -> Either ToCardanoError (C.TxOutDatum ctx C.BabbageEra))
    -> PV2.TxOut
    -> Either ToCardanoError (C.TxOut ctx C.BabbageEra)
toCardanoTxOut networkId fromHash (PV2.TxOut addr value datum _rs) =
    C.TxOut <$> toCardanoAddressInEra networkId addr
            <*> toCardanoTxOutValue value
            <*> fromHash datum
            <*> pure C.ReferenceScriptNone -- fixme

fromCardanoAddressInEra :: C.AddressInEra era -> P.Address
fromCardanoAddressInEra (C.AddressInEra C.ByronAddressInAnyEra address) = fromCardanoAddress address
fromCardanoAddressInEra (C.AddressInEra _ address)                      = fromCardanoAddress address

fromCardanoAddress :: C.Address addrtype -> P.Address
fromCardanoAddress (C.ByronAddress address) =
    P.Address plutusCredential Nothing
    where
      plutusCredential :: Credential.Credential
      plutusCredential =
          Credential.PubKeyCredential
        $ PV1.PubKeyHash
        $ PlutusTx.toBuiltin
        $ addrToBase58 address
fromCardanoAddress (C.ShelleyAddress _ paymentCredential stakeAddressReference) =
    P.Address (fromCardanoPaymentCredential (C.fromShelleyPaymentCredential paymentCredential))
        $ fromCardanoStakeAddressReference (C.fromShelleyStakeReference stakeAddressReference)

toCardanoAddressInEra :: C.NetworkId -> P.Address -> Either ToCardanoError (C.AddressInEra C.BabbageEra)
toCardanoAddressInEra networkId (P.Address addressCredential addressStakingCredential) =
    C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) <$>
        (C.makeShelleyAddress networkId
            <$> toCardanoPaymentCredential addressCredential
            <*> toCardanoStakeAddressReference addressStakingCredential)

fromCardanoPaymentCredential :: C.PaymentCredential -> Credential.Credential
fromCardanoPaymentCredential (C.PaymentCredentialByKey paymentKeyHash) = Credential.PubKeyCredential (fromCardanoPaymentKeyHash paymentKeyHash)
fromCardanoPaymentCredential (C.PaymentCredentialByScript scriptHash) = Credential.ScriptCredential (fromCardanoScriptHash scriptHash)

toCardanoPaymentCredential :: Credential.Credential -> Either ToCardanoError C.PaymentCredential
toCardanoPaymentCredential (Credential.PubKeyCredential pubKeyHash) = C.PaymentCredentialByKey <$> toCardanoPaymentKeyHash (P.PaymentPubKeyHash pubKeyHash)
toCardanoPaymentCredential (Credential.ScriptCredential validatorHash) = C.PaymentCredentialByScript <$> toCardanoScriptHash validatorHash

fromCardanoPaymentKeyHash :: C.Hash C.PaymentKey -> PV1.PubKeyHash
fromCardanoPaymentKeyHash paymentKeyHash = PV1.PubKeyHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes paymentKeyHash

toCardanoPaymentKeyHash :: P.PaymentPubKeyHash -> Either ToCardanoError (C.Hash C.PaymentKey)
toCardanoPaymentKeyHash (P.PaymentPubKeyHash (PV1.PubKeyHash bs)) =
    let bsx = PlutusTx.fromBuiltin bs
        tg = "toCardanoPaymentKeyHash (" <> show (BS.length bsx) <> " bytes)"
    in tag tg $ deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) bsx

fromCardanoScriptHash :: C.ScriptHash -> P.ValidatorHash
fromCardanoScriptHash scriptHash = P.ValidatorHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes scriptHash

toCardanoScriptHash :: P.ValidatorHash -> Either ToCardanoError C.ScriptHash
toCardanoScriptHash (P.ValidatorHash bs) = tag "toCardanoScriptHash" $ deserialiseFromRawBytes C.AsScriptHash $ PlutusTx.fromBuiltin bs

fromCardanoStakeAddressReference :: C.StakeAddressReference -> Maybe Credential.StakingCredential
fromCardanoStakeAddressReference C.NoStakeAddress = Nothing
fromCardanoStakeAddressReference (C.StakeAddressByValue stakeCredential) =
    Just (Credential.StakingHash $ fromCardanoStakeCredential stakeCredential)
fromCardanoStakeAddressReference C.StakeAddressByPointer{} = Nothing

toCardanoStakeAddressReference :: Maybe Credential.StakingCredential -> Either ToCardanoError C.StakeAddressReference
toCardanoStakeAddressReference Nothing = pure C.NoStakeAddress
toCardanoStakeAddressReference (Just (Credential.StakingHash credential)) =
    C.StakeAddressByValue <$> toCardanoStakeCredential credential
toCardanoStakeAddressReference (Just Credential.StakingPtr{}) = Left StakingPointersNotSupported

fromCardanoStakeCredential :: C.StakeCredential -> Credential.Credential
fromCardanoStakeCredential (C.StakeCredentialByKey stakeKeyHash) = Credential.PubKeyCredential (fromCardanoStakeKeyHash stakeKeyHash)
fromCardanoStakeCredential (C.StakeCredentialByScript scriptHash) = Credential.ScriptCredential (fromCardanoScriptHash scriptHash)

toCardanoStakeCredential :: Credential.Credential -> Either ToCardanoError C.StakeCredential
toCardanoStakeCredential (Credential.PubKeyCredential pubKeyHash) = C.StakeCredentialByKey <$> toCardanoStakeKeyHash pubKeyHash
toCardanoStakeCredential (Credential.ScriptCredential validatorHash) = C.StakeCredentialByScript <$> toCardanoScriptHash validatorHash

fromCardanoStakeKeyHash :: C.Hash C.StakeKey -> PV1.PubKeyHash
fromCardanoStakeKeyHash stakeKeyHash = PV1.PubKeyHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes stakeKeyHash

toCardanoStakeKeyHash :: PV1.PubKeyHash -> Either ToCardanoError (C.Hash C.StakeKey)
toCardanoStakeKeyHash (PV1.PubKeyHash bs) = tag "toCardanoStakeKeyHash" $ deserialiseFromRawBytes (C.AsHash C.AsStakeKey) (PlutusTx.fromBuiltin bs)

fromCardanoTxOutValue :: C.TxOutValue era -> PV1.Value
fromCardanoTxOutValue (C.TxOutAdaOnly _ lovelace) = fromCardanoLovelace lovelace
fromCardanoTxOutValue (C.TxOutValue _ value)      = fromCardanoValue value

toCardanoTxOutValue :: PV1.Value -> Either ToCardanoError (C.TxOutValue C.BabbageEra)
toCardanoTxOutValue value = C.TxOutValue C.MultiAssetInBabbageEra <$> toCardanoValue value

fromCardanoTxOutDatumHash :: C.TxOutDatum C.CtxTx era -> Maybe P.DatumHash
fromCardanoTxOutDatumHash C.TxOutDatumNone       = Nothing
fromCardanoTxOutDatumHash (C.TxOutDatumHash _ h) =
    Just $ P.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatumHash (C.TxOutDatumInTx _ d) =
    Just $ P.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d))
fromCardanoTxOutDatumHash (C.TxOutDatumInline _ d) =
    Just $ P.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d))

fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> PV2.OutputDatum
fromCardanoTxOutDatum C.TxOutDatumNone       =
    PV2.NoOutputDatum
fromCardanoTxOutDatum (C.TxOutDatumHash _ h) =
    PV2.OutputDatumHash $ PV2.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatum (C.TxOutDatumInTx _ d) =
    PV2.OutputDatumHash $ PV2.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d))
fromCardanoTxOutDatum (C.TxOutDatumInline _ d) =
    PV2.OutputDatum $ PV2.Datum $ fromCardanoScriptData d

toCardanoTxOutNoDatum  :: C.TxOutDatum C.CtxTx C.BabbageEra
toCardanoTxOutNoDatum = C.TxOutDatumNone

toCardanoTxOutDatumInTx :: PV2.Datum -> C.TxOutDatum C.CtxTx C.BabbageEra
toCardanoTxOutDatumInTx =
    C.TxOutDatumInTx C.ScriptDataInBabbageEra . C.fromPlutusData . PV2.builtinDataToData . PV2.getDatum

toCardanoTxOutDatumInline :: PV2.Datum -> C.TxOutDatum C.CtxTx C.BabbageEra
toCardanoTxOutDatumInline =
      C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra
    . C.fromPlutusData . PV2.builtinDataToData . PV2.getDatum

toCardanoTxOutDatumHashFromDatum :: PV2.Datum -> C.TxOutDatum ctx C.BabbageEra
toCardanoTxOutDatumHashFromDatum =
      C.TxOutDatumHash C.ScriptDataInBabbageEra
    . C.hashScriptData
    . C.fromPlutusData
    . PV2.builtinDataToData
    . PV2.getDatum

toCardanoTxOutDatumHash :: P.DatumHash -> Either ToCardanoError (C.TxOutDatum ctx C.BabbageEra)
toCardanoTxOutDatumHash datumHash = C.TxOutDatumHash C.ScriptDataInBabbageEra <$> toCardanoScriptDataHash datumHash

toCardanoTxOutDatum :: PV2.OutputDatum -> Either ToCardanoError (C.TxOutDatum C.CtxTx C.BabbageEra)
toCardanoTxOutDatum PV2.NoOutputDatum        = pure toCardanoTxOutNoDatum
toCardanoTxOutDatum (PV2.OutputDatum d)      = pure $ toCardanoTxOutDatumInline d
toCardanoTxOutDatum (PV2.OutputDatumHash dh) = toCardanoTxOutDatumHash dh

toCardanoScriptDataHash :: P.DatumHash -> Either ToCardanoError (C.Hash C.ScriptData)
toCardanoScriptDataHash (P.DatumHash bs) =
    tag "toCardanoTxOutDatumHash" (deserialiseFromRawBytes (C.AsHash C.AsScriptData) (PlutusTx.fromBuiltin bs))

fromCardanoMintValue :: C.TxMintValue build era -> PV1.Value
fromCardanoMintValue C.TxMintNone              = mempty
fromCardanoMintValue (C.TxMintValue _ value _) = fromCardanoValue value


adaToCardanoValue :: P.Ada -> C.Value
adaToCardanoValue (P.Lovelace n) = C.valueFromList [(C.AdaAssetId, C.Quantity n)]

fromCardanoValue :: C.Value -> Value.Value
fromCardanoValue (C.valueToList -> list) =
    foldMap fromSingleton list
  where
    fromSingleton (fromCardanoAssetId -> assetClass, C.Quantity quantity) =
        Value.assetClassValue assetClass quantity

toCardanoValue :: Value.Value -> Either ToCardanoError C.Value
toCardanoValue =
    fmap C.valueFromList . traverse toSingleton . Value.flattenValue
  where
    toSingleton (cs, tn, q) =
        toCardanoAssetId (Value.assetClass cs tn) <&> (, C.Quantity q)

fromCardanoPolicyId :: C.PolicyId -> P.MintingPolicyHash
fromCardanoPolicyId (C.PolicyId scriptHash) = P.MintingPolicyHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes scriptHash)

toCardanoPolicyId :: P.MintingPolicyHash -> Either ToCardanoError C.PolicyId
toCardanoPolicyId (P.MintingPolicyHash bs) =
    tag "toCardanoPolicyId" $
        tag (show (BS.length (PlutusTx.fromBuiltin bs)) <> " bytes")
            (deserialiseFromRawBytes C.AsPolicyId (PlutusTx.fromBuiltin bs))

fromCardanoAssetName :: C.AssetName -> Value.TokenName
fromCardanoAssetName (C.AssetName bs) = Value.TokenName $ PlutusTx.toBuiltin bs

toCardanoAssetName :: Value.TokenName -> Either ToCardanoError C.AssetName
toCardanoAssetName (Value.TokenName bs) =
    tag "toCardanoAssetName" $
        tag (show (BS.length (PlutusTx.fromBuiltin bs)) <> " bytes")
            (deserialiseFromRawBytes C.AsAssetName (PlutusTx.fromBuiltin bs))

fromCardanoAssetId :: C.AssetId -> Value.AssetClass
fromCardanoAssetId C.AdaAssetId = Value.assetClass Ada.adaSymbol Ada.adaToken
fromCardanoAssetId (C.AssetId policyId assetName) =
    Value.assetClass
        (Value.mpsSymbol . fromCardanoPolicyId $ policyId)
        (fromCardanoAssetName assetName)

toCardanoAssetId :: Value.AssetClass -> Either ToCardanoError C.AssetId
toCardanoAssetId (Value.AssetClass (currencySymbol, tokenName))
    | currencySymbol == Ada.adaSymbol && tokenName == Ada.adaToken =
        pure C.AdaAssetId
    | otherwise =
        C.AssetId
            <$> toCardanoPolicyId (Value.currencyMPSHash currencySymbol)
            <*> toCardanoAssetName tokenName

fromCardanoFee :: C.TxFee era -> PV1.Value
fromCardanoFee (C.TxFeeImplicit _)          = mempty
fromCardanoFee (C.TxFeeExplicit _ lovelace) = fromCardanoLovelace lovelace

toCardanoFee :: PV1.Value -> Either ToCardanoError (C.TxFee C.BabbageEra)
toCardanoFee value = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra <$> toCardanoLovelace value

fromCardanoLovelace :: C.Lovelace -> PV1.Value
fromCardanoLovelace (C.lovelaceToQuantity -> C.Quantity lovelace) = Ada.lovelaceValueOf lovelace

toCardanoLovelace :: PV1.Value -> Either ToCardanoError C.Lovelace
toCardanoLovelace value =
    if value == Ada.lovelaceValueOf lovelace
        then pure . C.quantityToLovelace . C.Quantity $ lovelace
        else Left ValueNotPureAda
    where
        Ada.Lovelace lovelace = Ada.fromValue value

fromCardanoValidityRange :: (C.TxValidityLowerBound era, C.TxValidityUpperBound era) -> P.SlotRange
fromCardanoValidityRange (l, u) = PV1.Interval (fromCardanoValidityLowerBound l) (fromCardanoValidityUpperBound u)

toCardanoValidityRange
    :: P.SlotRange -> Either ToCardanoError (C.TxValidityLowerBound C.BabbageEra, C.TxValidityUpperBound C.BabbageEra)
toCardanoValidityRange (PV1.Interval l u) = (,) <$> toCardanoValidityLowerBound l <*> toCardanoValidityUpperBound u

fromCardanoValidityLowerBound :: C.TxValidityLowerBound era -> PV1.LowerBound P.Slot
fromCardanoValidityLowerBound C.TxValidityNoLowerBound = PV1.LowerBound PV1.NegInf True
fromCardanoValidityLowerBound (C.TxValidityLowerBound _ slotNo) = PV1.LowerBound (PV1.Finite $ fromCardanoSlotNo slotNo) True

toCardanoValidityLowerBound :: PV1.LowerBound P.Slot -> Either ToCardanoError (C.TxValidityLowerBound C.BabbageEra)
toCardanoValidityLowerBound (PV1.LowerBound PV1.NegInf _) = pure C.TxValidityNoLowerBound
toCardanoValidityLowerBound (PV1.LowerBound (PV1.Finite slotNo) closed)
    = pure . C.TxValidityLowerBound C.ValidityLowerBoundInBabbageEra
    . toCardanoSlotNo
    $ if slotNo < 0 then 0 else if closed then slotNo else slotNo + 1
toCardanoValidityLowerBound (PV1.LowerBound PV1.PosInf _) = Left InvalidValidityRange

fromCardanoValidityUpperBound :: C.TxValidityUpperBound era -> PV1.UpperBound P.Slot
fromCardanoValidityUpperBound (C.TxValidityNoUpperBound _) = PV1.UpperBound PV1.PosInf True
fromCardanoValidityUpperBound (C.TxValidityUpperBound _ slotNo) = PV1.UpperBound (PV1.Finite $ fromCardanoSlotNo slotNo) False

toCardanoValidityUpperBound :: PV1.UpperBound P.Slot -> Either ToCardanoError (C.TxValidityUpperBound C.BabbageEra)
toCardanoValidityUpperBound (PV1.UpperBound PV1.PosInf _) = pure $ C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra
toCardanoValidityUpperBound (PV1.UpperBound (PV1.Finite slotNo) closed)
    = pure . C.TxValidityUpperBound C.ValidityUpperBoundInBabbageEra . toCardanoSlotNo $ if closed then slotNo + 1 else slotNo
toCardanoValidityUpperBound (PV1.UpperBound PV1.NegInf _) = Left InvalidValidityRange

fromCardanoSlotNo :: C.SlotNo -> P.Slot
fromCardanoSlotNo (C.SlotNo w64) = P.Slot (toInteger w64)

toCardanoSlotNo :: P.Slot -> C.SlotNo
toCardanoSlotNo (P.Slot i) = C.SlotNo (fromInteger i)

fromCardanoScriptData :: C.ScriptData -> PV1.BuiltinData
fromCardanoScriptData = PV1.dataToBuiltinData . C.toPlutusData

toCardanoScriptData :: PV1.BuiltinData -> C.ScriptData
toCardanoScriptData = C.fromPlutusData . PV1.builtinDataToData

fromCardanoScriptInEra :: C.ScriptInEra era -> Maybe (P.Versioned P.Script)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV1InAlonzo (C.PlutusScript C.PlutusScriptV1 script)) =
    Just (P.Versioned (fromCardanoPlutusScript script) P.PlutusV1)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV1InBabbage (C.PlutusScript C.PlutusScriptV1 script)) =
    Just (P.Versioned (fromCardanoPlutusScript script) P.PlutusV1)
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV2InBabbage (C.PlutusScript C.PlutusScriptV2 script)) =
    Just (P.Versioned (fromCardanoPlutusScript script) P.PlutusV2)
fromCardanoScriptInEra (C.ScriptInEra _ C.SimpleScript{}) = Nothing

toCardanoScriptInEra :: P.Versioned P.Script -> Either ToCardanoError (C.ScriptInEra C.BabbageEra)
toCardanoScriptInEra (P.Versioned script P.PlutusV1) = C.ScriptInEra C.PlutusScriptV1InBabbage . C.PlutusScript C.PlutusScriptV1 <$> toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script
toCardanoScriptInEra (P.Versioned script P.PlutusV2) = C.ScriptInEra C.PlutusScriptV2InBabbage . C.PlutusScript C.PlutusScriptV2 <$> toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script

fromCardanoPlutusScript :: C.HasTypeProxy lang => C.PlutusScript lang -> P.Script
fromCardanoPlutusScript = Codec.deserialise . BSL.fromStrict . C.serialiseToRawBytes

toCardanoPlutusScript
    :: C.SerialiseAsRawBytes plutusScript
    => C.AsType plutusScript
    -> P.Script
    -> Either ToCardanoError plutusScript
toCardanoPlutusScript asPlutusScriptType =
    tag "toCardanoPlutusScript"
    . deserialiseFromRawBytes asPlutusScriptType . BSL.toStrict . Codec.serialise

fromCardanoScriptInAnyLang :: C.ScriptInAnyLang -> Maybe (P.Versioned P.Script)
fromCardanoScriptInAnyLang (C.ScriptInAnyLang _sl (C.SimpleScript _ssv _ss)) = Nothing
fromCardanoScriptInAnyLang (C.ScriptInAnyLang _sl (C.PlutusScript psv ps)) = Just $ case psv of
     C.PlutusScriptV1 -> P.Versioned (fromCardanoPlutusScript ps) P.PlutusV1
     C.PlutusScriptV2 -> P.Versioned (fromCardanoPlutusScript ps) P.PlutusV2

toCardanoScriptInAnyLang :: P.Versioned P.Script -> Either ToCardanoError C.ScriptInAnyLang
toCardanoScriptInAnyLang (P.Versioned script P.PlutusV1) =
  C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) . C.PlutusScript C.PlutusScriptV1
    <$> toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script
toCardanoScriptInAnyLang (P.Versioned script P.PlutusV2) =
  C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) . C.PlutusScript C.PlutusScriptV2
    <$> toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script

toCardanoReferenceScript :: Maybe (P.Versioned P.Script) -> Either ToCardanoError (C.ReferenceScript C.BabbageEra)
toCardanoReferenceScript (Just script) = C.ReferenceScript C.ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> toCardanoScriptInAnyLang script
toCardanoReferenceScript Nothing = pure C.ReferenceScriptNone

deserialiseFromRawBytes :: C.SerialiseAsRawBytes t => C.AsType t -> ByteString -> Either ToCardanoError t
deserialiseFromRawBytes asType = maybe (Left DeserialisationError) Right . C.deserialiseFromRawBytes asType

tag :: String -> Either ToCardanoError t -> Either ToCardanoError t
tag s = first (Tag s)

data FromCardanoError
    = SimpleScriptsNotSupported
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToObject)

instance Pretty FromCardanoError where
    pretty SimpleScriptsNotSupported        = "Simple scripts are not supported"

data ToCardanoError
    = TxBodyError String -- ^ A C.TxBodyError converted to String
    | DeserialisationError
    | InvalidValidityRange
    | ValueNotPureAda
    | OutputHasZeroAda
    | StakingPointersNotSupported
    | SimpleScriptsNotSupportedToCardano
    | MissingInputValidator
    | MissingDatum
    | MissingMintingPolicy
    | ScriptPurposeNotSupported PV1.ScriptTag
    | MissingMintingPolicyRedeemer
    | MissingStakeValidator
    | UnsupportedPlutusVersion P.Language
    | Tag String ToCardanoError
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty ToCardanoError where
    pretty (TxBodyError err)                  = "TxBodyError" <> colon <+> pretty err
    pretty DeserialisationError               = "ByteString deserialisation failed"
    pretty InvalidValidityRange               = "Invalid validity range"
    pretty ValueNotPureAda                    = "Fee values should only contain Ada"
    pretty OutputHasZeroAda                   = "Transaction outputs should not contain zero Ada"
    pretty StakingPointersNotSupported        = "Staking pointers are not supported"
    pretty SimpleScriptsNotSupportedToCardano = "Simple scripts are not supported"
    pretty MissingMintingPolicy               = "Missing minting policy"
    pretty (ScriptPurposeNotSupported p)      = "Script purpose not supported:" <+> viaShow p
    pretty MissingMintingPolicyRedeemer       = "Missing minting policy redeemer"
    pretty (UnsupportedPlutusVersion v)       = "Plutus version not supported:" <+> viaShow v
    pretty MissingInputValidator              = "Missing input validator."
    pretty MissingDatum                       = "Missing required datum."
    pretty MissingStakeValidator              = "Missing stake validator."
    pretty (Tag t err)                        = pretty t <> colon <+> pretty err

zeroExecutionUnits :: C.ExecutionUnits
zeroExecutionUnits = C.ExecutionUnits 0 0
