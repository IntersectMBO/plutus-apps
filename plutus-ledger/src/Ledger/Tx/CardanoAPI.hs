{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ViewPatterns       #-}

{-# OPTIONS_GHC -Wno-orphans        #-}

{-|

Interface to the transaction types from 'cardano-api'

-}
module Ledger.Tx.CardanoAPI
  ( module Plutus.V2.Ledger.Api
  , module Plutus.V2.Ledger.Tx
  , CardanoBuildTx(..)
  , SomeCardanoApiTx(..)
  , txOutRefs
  , unspentOutputsTx
  , fromCardanoTxId
  , fromCardanoTxIn
  , fromCardanoTxInsCollateral
  , fromCardanoTxOut
  , fromCardanoTxOutDatumHash
  , fromCardanoTxOutDatum
  , fromCardanoAddressInEra
  , fromCardanoAddress
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
  , fromTxScriptValidity
  , toTxScriptValidity
  , scriptDataFromCardanoTxBody
  , plutusScriptsFromTxBody
  , makeTransactionBody
  , toCardanoTxBody
  , toCardanoTxBodyContent
  , toCardanoTxIn
  , toCardanoTxInsCollateral
  , toCardanoTxInWitness
  , toCardanoTxOut
  , toCardanoTxOutUnsafe
  , toCardanoTxOutDatumHash
  , toCardanoTxOutValue
  , toCardanoAddressInEra
  , toCardanoMintValue
  , toCardanoValue
  , toCardanoFee
  , toCardanoValidityRange
  , toCardanoPlutusV1ScriptInEra
  , toCardanoPlutusV2ScriptInEra
  , toCardanoPaymentKeyHash
  , toCardanoScriptData
  , toCardanoScriptDataHash
  , toCardanoScriptHash
  , toCardanoTxId
  , toCardanoTxOutDatum
  , ToCardanoError(..)
  , FromCardanoError(..)
  , deserialiseFromRawBytes
  ) where

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
import Control.Lens ((&), (.~), (?~))
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseFail, prependFailure, typeMismatch)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.OpenApi (NamedSchema (NamedSchema), OpenApiType (OpenApiObject), byteSchema, declareSchemaRef, properties,
                     required, sketchSchema, type_)
import Data.OpenApi qualified as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Ledger.Ada qualified as Ada
import Ledger.Address qualified as P
import Ledger.Params qualified as P
import Ledger.Scripts qualified as P
import Ledger.Slot qualified as P
import Ledger.Tx.CardanoAPITemp (makeTransactionBody')
import Ledger.Tx.Internal qualified as P
import Ledger.Value (currencyMPSHash, flattenValue, mpsSymbol)
import Plutus.Script.Utils.V2.Scripts (scriptHash)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Tx
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

instance Pretty CardanoBuildTx where
  pretty (CardanoBuildTx txBodyContent) = viaShow txBodyContent

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

instance Pretty SomeCardanoApiTx where
  pretty = viaShow

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

txOutRefs :: SomeCardanoApiTx -> [(TxOut, TxOutRef)]
txOutRefs (SomeTx (C.Tx txBody@(C.TxBody C.TxBodyContent{..}) _) _) =
  mkOut <$> zip [0..] plutusTxOuts
  where
    mkOut (i, o) = (o, TxOutRef (fromCardanoTxId $ C.getTxId txBody) i)
    plutusTxOuts = mapMaybe (either (const Nothing) Just . fromCardanoTxOut) txOuts

unspentOutputsTx :: SomeCardanoApiTx -> Map.Map TxOutRef TxOut
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
  -> (Map.Map P.DatumHash P.Datum, Redeemers)
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

redeemerPtrFromCardanoRdmrPtr :: Alonzo.RdmrPtr -> RedeemerPtr
redeemerPtrFromCardanoRdmrPtr (Alonzo.RdmrPtr rdmrTag ptr) = RedeemerPtr t (toInteger ptr)
  where
    t = case rdmrTag of
      Alonzo.Spend -> Spend
      Alonzo.Mint  -> Mint
      Alonzo.Cert  -> Cert
      Alonzo.Rewrd -> Reward

-- | Extract plutus scripts from a Cardano API tx body.
--
-- Note that Plutus scripts are only supported in Alonzo era and onwards.
plutusScriptsFromTxBody :: C.TxBody era -> Map.Map P.ScriptHash P.Script
plutusScriptsFromTxBody C.ByronTxBody {} = mempty
plutusScriptsFromTxBody (C.ShelleyTxBody shelleyBasedEra _ scripts _ _ _) =
  Map.fromList $ mapMaybe (fromLedgerScript shelleyBasedEra) scripts

-- | Convert a script from a Cardano api in shelley based era to a Plutus script along with it's hash.
--
-- Note that Plutus scripts are only supported in Alonzo era and onwards.
fromLedgerScript
  :: C.ShelleyBasedEra era
  -> Ledger.Script (C.ShelleyLedgerEra era)
  -> Maybe (P.ScriptHash, P.Script)
fromLedgerScript C.ShelleyBasedEraShelley _      = Nothing
fromLedgerScript C.ShelleyBasedEraAllegra _      = Nothing
fromLedgerScript C.ShelleyBasedEraMary _         = Nothing
fromLedgerScript C.ShelleyBasedEraAlonzo script  = fromLedgerPlutusScript script
fromLedgerScript C.ShelleyBasedEraBabbage script = fromLedgerPlutusScript script

-- | Convert a `cardano-ledger` Plutus script from the Alonzo era and onwards to
-- a 'Script' along with it's hash.
fromLedgerPlutusScript :: Alonzo.Script a -> Maybe (P.ScriptHash, P.Script)
fromLedgerPlutusScript Alonzo.TimelockScript {} = Nothing
fromLedgerPlutusScript (Alonzo.PlutusScript Alonzo.PlutusV1 bs) =
  let script = fmap (\s -> (P.scriptHash s, s))
             $ deserialiseOrFail
             $ BSL.fromStrict
             $ SBS.fromShort bs
   in either (const Nothing) Just script
fromLedgerPlutusScript (Alonzo.PlutusScript Alonzo.PlutusV2 bs) =
  let script = fmap (\s -> (scriptHash s, s))
             $ deserialiseOrFail
             $ BSL.fromStrict
             $ SBS.fromShort bs
   in either (const Nothing) Just script

toCardanoTxBodyContent
    :: P.Params -- ^ Parameters to use.
    -> [P.PaymentPubKeyHash] -- ^ Required signers of the transaction
    -> P.Tx
    -> Either ToCardanoError CardanoBuildTx
toCardanoTxBodyContent P.Params{P.pProtocolParams, P.pNetworkId} sigs P.Tx{..} = do
    txIns <- traverse toCardanoTxInBuild $ Set.toList txInputs
    txInsReference <- traverse (\(TxIn ref _) -> toCardanoTxIn ref) $ Set.toList txReferenceInputs
    txInsCollateral <- toCardanoTxInsCollateral txCollateral
    txOuts <- traverse (toCardanoTxOut pNetworkId lookupDatum lookupScript) txOutputs
    txFee' <- toCardanoFee txFee
    txValidityRange <- toCardanoValidityRange txValidRange
    txMintValue <- toCardanoMintValue txRedeemers txMint txMintScripts
    txExtraKeyWits <- C.TxExtraKeyWitnesses C.ExtraKeyWitnessesInBabbageEra <$> traverse toCardanoPaymentKeyHash sigs
    pure $ CardanoBuildTx $ C.TxBodyContent
        { txIns = txIns
        , txInsReference = C.TxInsReference C.ReferenceTxInsScriptsInlineDatumsInBabbageEra txInsReference
        , txInsCollateral = txInsCollateral
        , txOuts = txOuts
        , txTotalCollateral = C.TxTotalCollateralNone -- TODO Change when going to Babbage era txs
        , txReturnCollateral = C.TxReturnCollateralNone -- TODO Change when going to Babbage era txs
        , txFee = txFee'
        , txValidityRange = txValidityRange
        , txMintValue = txMintValue
        , txProtocolParams = C.BuildTxWith $ Just pProtocolParams
        , txScriptValidity = C.TxScriptValidityNone
        , txExtraKeyWits
        -- unused:
        , txMetadata = C.TxMetadataNone
        , txAuxScripts = C.TxAuxScriptsNone
        , txWithdrawals = C.TxWithdrawalsNone
        , txCertificates = C.TxCertificatesNone
        , txUpdateProposal = C.TxUpdateProposalNone
        }

  where
    -- FIXME what do we need to do here if
    lookupDatum :: OutputDatum -> Either ToCardanoError (C.TxOutDatum C.CtxTx C.BabbageEra)
    lookupDatum NoOutputDatum =
      pure C.TxOutDatumNone
    lookupDatum (OutputDatumHash dh) = do
      case Map.lookup dh txData of
        Nothing -> C.TxOutDatumHash C.ScriptDataInBabbageEra <$> toCardanoScriptDataHash dh
        Just da -> pure $ C.TxOutDatumInTx C.ScriptDataInBabbageEra (toCardanoScriptData $ P.getDatum da)
    lookupDatum (OutputDatum da) =
      pure $ C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra (toCardanoScriptData $ P.getDatum da)

    lookupScript :: Maybe ScriptHash -> Either ToCardanoError (C.ReferenceScript C.BabbageEra)
    lookupScript = const $ throwError ReferenceScriptNotSupported

toCardanoTxBody ::
    P.Params -- ^ Parameters to use.
    -> [P.PaymentPubKeyHash] -- ^ Required signers of the transaction
    -> P.Tx
    -> Either ToCardanoError (C.TxBody C.BabbageEra)
toCardanoTxBody params sigs tx = do
    txBodyContent <- toCardanoTxBodyContent params sigs tx
    makeTransactionBody mempty txBodyContent

makeTransactionBody
    :: Map.Map Alonzo.RdmrPtr Alonzo.ExUnits
    -> CardanoBuildTx
    -> Either ToCardanoError (C.TxBody C.BabbageEra)
makeTransactionBody exUnits (CardanoBuildTx txBodyContent) =
  first (TxBodyError . C.displayError) $ makeTransactionBody' exUnits txBodyContent

fromCardanoTxIn :: C.TxIn -> TxOutRef
fromCardanoTxIn (C.TxIn txId (C.TxIx txIx)) = TxOutRef (fromCardanoTxId txId) (toInteger txIx)

toCardanoTxInBuild :: TxIn -> Either ToCardanoError (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))
toCardanoTxInBuild (TxIn txInRef (Just txInType)) = (,) <$> toCardanoTxIn txInRef <*> (C.BuildTxWith <$> toCardanoTxInWitness txInType)
toCardanoTxInBuild (TxIn _ Nothing) = Left MissingTxInType

toCardanoTxIn :: TxOutRef -> Either ToCardanoError C.TxIn
toCardanoTxIn (TxOutRef txId txIx) = C.TxIn <$> toCardanoTxId txId <*> pure (C.TxIx (fromInteger txIx))

fromCardanoTxId :: C.TxId -> TxId
fromCardanoTxId txId = TxId $ PlutusTx.toBuiltin $ C.serialiseToRawBytes txId

toCardanoTxId :: TxId -> Either ToCardanoError C.TxId
toCardanoTxId (TxId bs) =
    tag "toCardanoTxId"
    $ deserialiseFromRawBytes C.AsTxId $ PlutusTx.fromBuiltin bs

fromCardanoTxInsCollateral :: C.TxInsCollateral era -> Set.Set TxIn
fromCardanoTxInsCollateral C.TxInsCollateralNone       = mempty
fromCardanoTxInsCollateral (C.TxInsCollateral _ txIns) = Set.fromList $ fmap (pubKeyTxIn . fromCardanoTxIn) txIns

toCardanoTxInsCollateral :: Set.Set TxIn -> Either ToCardanoError (C.TxInsCollateral C.BabbageEra)
toCardanoTxInsCollateral = fmap (C.TxInsCollateral C.CollateralInBabbageEra) . traverse (toCardanoTxIn . txInRef) . Set.toList

toCardanoTxInWitness :: TxInType -> Either ToCardanoError (C.Witness C.WitCtxTxIn C.BabbageEra)
toCardanoTxInWitness ConsumePublicKeyAddress = pure (C.KeyWitness C.KeyWitnessForSpending)
toCardanoTxInWitness ConsumeSimpleScriptAddress = Left SimpleScriptsNotSupportedToCardano -- TODO: Better support for simple scripts
toCardanoTxInWitness
    (ConsumeScriptAddress
        (P.Validator validator)
        (P.Redeemer redeemer)
        (P.Datum datum))
    = C.ScriptWitness C.ScriptWitnessForSpending <$>
        (C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1
        <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) validator)
        <*> pure (C.ScriptDatumForTxIn $ toCardanoScriptData datum)
        <*> pure (toCardanoScriptData redeemer)
        <*> pure zeroExecutionUnits
        )

toCardanoMintWitness :: Redeemers -> Int -> P.MintingPolicy -> Either ToCardanoError (C.ScriptWitness C.WitCtxMint C.BabbageEra)
toCardanoMintWitness redeemers idx (P.MintingPolicy script) = do
    let redeemerPtr = RedeemerPtr Mint (fromIntegral idx)
    P.Redeemer redeemer <- maybe (Left MissingMintingPolicyRedeemer) Right (Map.lookup redeemerPtr redeemers)
    C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1
        <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script)
        <*> pure C.NoScriptDatumForMint
        <*> pure (C.fromPlutusData $ toData redeemer)
        <*> pure zeroExecutionUnits

fromCardanoTxOut :: C.TxOut C.CtxTx era -> Either FromCardanoError TxOut
fromCardanoTxOut (C.TxOut addr value outputDatum refScript) =
    TxOut
    <$> fromCardanoAddressInEra addr
    <*> pure (fromCardanoTxOutValue value)
    <*> pure (fromCardanoTxOutDatum outputDatum)
    <*> pure (fromCardanoReferenceScript refScript)

fromCardanoReferenceScript :: C.ReferenceScript era -> Maybe ScriptHash
fromCardanoReferenceScript (C.ReferenceScript _rtisidsie sial) = fmap scriptHash $ fromCardanoScriptInAnyLang sial
fromCardanoReferenceScript C.ReferenceScriptNone               = Nothing

toCardanoTxOut
    :: C.NetworkId
    -> (OutputDatum -> Either ToCardanoError (C.TxOutDatum ctx C.BabbageEra))
    -> (Maybe ScriptHash -> Either ToCardanoError (C.ReferenceScript C.BabbageEra))
    -> TxOut
    -> Either ToCardanoError (C.TxOut ctx C.BabbageEra)
toCardanoTxOut networkId fromOutputDatum fromScriptHash (TxOut addr value outputDatum refScript) =
    C.TxOut <$> toCardanoAddressInEra networkId addr
            <*> toCardanoTxOutValue value
            <*> fromOutputDatum outputDatum
            <*> fromScriptHash refScript

toCardanoTxOutUnsafe
    :: C.NetworkId
    -> (OutputDatum -> Either ToCardanoError (C.TxOutDatum ctx C.BabbageEra))
    -> (Maybe ScriptHash -> Either ToCardanoError (C.ReferenceScript C.BabbageEra))
    -> TxOut
    -> Either ToCardanoError (C.TxOut ctx C.BabbageEra)
toCardanoTxOutUnsafe networkId fromOutputDatum fromScriptHash (TxOut addr value outputDatum refScript) =
    C.TxOut <$> toCardanoAddressInEra networkId addr
            <*> toCardanoTxOutValueUnsafe value
            <*> fromOutputDatum outputDatum
            <*> fromScriptHash refScript

fromCardanoAddressInEra :: C.AddressInEra era -> Either FromCardanoError P.Address
fromCardanoAddressInEra (C.AddressInEra C.ByronAddressInAnyEra address) = fromCardanoAddress address
fromCardanoAddressInEra (C.AddressInEra _ address)                      = fromCardanoAddress address

fromCardanoAddress :: C.Address addrtype -> Either FromCardanoError P.Address
fromCardanoAddress (C.ByronAddress address) =
    Right $ P.Address plutusCredential Nothing
    where
      plutusCredential :: Credential
      plutusCredential =
          PubKeyCredential
        $ PubKeyHash
        $ PlutusTx.toBuiltin
        $ addrToBase58 address
fromCardanoAddress (C.ShelleyAddress _ paymentCredential stakeAddressReference) =
    P.Address (fromCardanoPaymentCredential (C.fromShelleyPaymentCredential paymentCredential))
        <$> fromCardanoStakeAddressReference (C.fromShelleyStakeReference stakeAddressReference)

toCardanoAddressInEra :: C.NetworkId -> P.Address -> Either ToCardanoError (C.AddressInEra C.BabbageEra)
toCardanoAddressInEra networkId (P.Address addressCredential addressStakingCredential) =
    C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) <$>
        (C.makeShelleyAddress networkId
            <$> toCardanoPaymentCredential addressCredential
            <*> toCardanoStakeAddressReference addressStakingCredential)

fromCardanoPaymentCredential :: C.PaymentCredential -> Credential
fromCardanoPaymentCredential (C.PaymentCredentialByKey paymentKeyHash) = PubKeyCredential (fromCardanoPaymentKeyHash paymentKeyHash)
fromCardanoPaymentCredential (C.PaymentCredentialByScript sh) = ScriptCredential (fromCardanoScriptHash sh)

toCardanoPaymentCredential :: Credential -> Either ToCardanoError C.PaymentCredential
toCardanoPaymentCredential (PubKeyCredential pubKeyHash) = C.PaymentCredentialByKey <$> toCardanoPaymentKeyHash (P.PaymentPubKeyHash pubKeyHash)
toCardanoPaymentCredential (ScriptCredential validatorHash) = C.PaymentCredentialByScript <$> toCardanoScriptHash validatorHash

fromCardanoPaymentKeyHash :: C.Hash C.PaymentKey -> PubKeyHash
fromCardanoPaymentKeyHash paymentKeyHash = PubKeyHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes paymentKeyHash

toCardanoPaymentKeyHash :: P.PaymentPubKeyHash -> Either ToCardanoError (C.Hash C.PaymentKey)
toCardanoPaymentKeyHash (P.PaymentPubKeyHash (PubKeyHash bs)) =
    let bsx = PlutusTx.fromBuiltin bs
        tg = "toCardanoPaymentKeyHash (" <> show (BS.length bsx) <> " bytes)"
    in tag tg $ deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) bsx

fromCardanoScriptHash :: C.ScriptHash -> P.ValidatorHash
fromCardanoScriptHash sh = P.ValidatorHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes sh

toCardanoScriptHash :: P.ValidatorHash -> Either ToCardanoError C.ScriptHash
toCardanoScriptHash (P.ValidatorHash bs) = tag "toCardanoScriptHash" $ deserialiseFromRawBytes C.AsScriptHash $ PlutusTx.fromBuiltin bs

fromCardanoStakeAddressReference :: C.StakeAddressReference -> Either FromCardanoError (Maybe StakingCredential)
fromCardanoStakeAddressReference C.NoStakeAddress = pure Nothing
fromCardanoStakeAddressReference (C.StakeAddressByValue stakeCredential) =
    pure $ Just (StakingHash $ fromCardanoStakeCredential stakeCredential)
fromCardanoStakeAddressReference C.StakeAddressByPointer{} = pure Nothing

toCardanoStakeAddressReference :: Maybe StakingCredential -> Either ToCardanoError C.StakeAddressReference
toCardanoStakeAddressReference Nothing = pure C.NoStakeAddress
toCardanoStakeAddressReference (Just (StakingHash credential)) =
    C.StakeAddressByValue <$> toCardanoStakeCredential credential
toCardanoStakeAddressReference (Just StakingPtr{}) = Left StakingPointersNotSupported

fromCardanoStakeCredential :: C.StakeCredential -> Credential
fromCardanoStakeCredential (C.StakeCredentialByKey stakeKeyHash) = PubKeyCredential (fromCardanoStakeKeyHash stakeKeyHash)
fromCardanoStakeCredential (C.StakeCredentialByScript sh) = ScriptCredential (fromCardanoScriptHash sh)

toCardanoStakeCredential :: Credential -> Either ToCardanoError C.StakeCredential
toCardanoStakeCredential (PubKeyCredential pubKeyHash) = C.StakeCredentialByKey <$> toCardanoStakeKeyHash pubKeyHash
toCardanoStakeCredential (ScriptCredential validatorHash) = C.StakeCredentialByScript <$> toCardanoScriptHash validatorHash

fromCardanoStakeKeyHash :: C.Hash C.StakeKey -> PubKeyHash
fromCardanoStakeKeyHash stakeKeyHash = PubKeyHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes stakeKeyHash

toCardanoStakeKeyHash :: PubKeyHash -> Either ToCardanoError (C.Hash C.StakeKey)
toCardanoStakeKeyHash (PubKeyHash bs) = tag "toCardanoStakeKeyHash" $ deserialiseFromRawBytes (C.AsHash C.AsStakeKey) (PlutusTx.fromBuiltin bs)

fromCardanoTxOutValue :: C.TxOutValue era -> Value
fromCardanoTxOutValue (C.TxOutAdaOnly _ lovelace) = fromCardanoLovelace lovelace
fromCardanoTxOutValue (C.TxOutValue _ value)      = fromCardanoValue value

toCardanoTxOutValue :: Value -> Either ToCardanoError (C.TxOutValue C.BabbageEra)
toCardanoTxOutValue value = do
    when (Ada.fromValue value == mempty) (Left OutputHasZeroAda)
    C.TxOutValue C.MultiAssetInBabbageEra <$> toCardanoValue value

toCardanoTxOutValueUnsafe :: Value -> Either ToCardanoError (C.TxOutValue C.BabbageEra)
toCardanoTxOutValueUnsafe value = C.TxOutValue C.MultiAssetInBabbageEra <$> toCardanoValue value

fromCardanoTxOutDatumHash :: C.TxOutDatum C.CtxTx era -> Maybe P.DatumHash
fromCardanoTxOutDatumHash C.TxOutDatumNone       = Nothing
fromCardanoTxOutDatumHash (C.TxOutDatumHash _ h) = Just $ P.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatumHash (C.TxOutDatumInTx _ d) = Just $ P.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d))
fromCardanoTxOutDatumHash (C.TxOutDatumInline _ d) = Just $ P.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d))

fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> OutputDatum
fromCardanoTxOutDatum C.TxOutDatumNone       = NoOutputDatum
fromCardanoTxOutDatum (C.TxOutDatumHash _ h) = OutputDatumHash $ DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatum (C.TxOutDatumInTx _ d) = OutputDatum $ Datum $ fromCardanoScriptData d
fromCardanoTxOutDatum (C.TxOutDatumInline _ d) = OutputDatum $ Datum $ fromCardanoScriptData d

toCardanoTxOutDatum :: OutputDatum -> Either ToCardanoError (C.TxOutDatum ctx C.BabbageEra)
toCardanoTxOutDatum NoOutputDatum        = pure C.TxOutDatumNone
toCardanoTxOutDatum (OutputDatumHash dh) = C.TxOutDatumHash C.ScriptDataInBabbageEra <$> toCardanoScriptDataHash dh
toCardanoTxOutDatum (OutputDatum da)     = pure $ C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra $ toCardanoScriptData (getDatum da)

toCardanoTxOutDatumHash :: Maybe P.DatumHash -> Either ToCardanoError (C.TxOutDatum ctx C.BabbageEra)
toCardanoTxOutDatumHash Nothing          = pure C.TxOutDatumNone
toCardanoTxOutDatumHash (Just datumHash) = C.TxOutDatumHash C.ScriptDataInBabbageEra <$> toCardanoScriptDataHash datumHash

toCardanoScriptDataHash :: P.DatumHash -> Either ToCardanoError (C.Hash C.ScriptData)
toCardanoScriptDataHash (P.DatumHash bs) = tag "toCardanoTxOutDatumHash" (deserialiseFromRawBytes (C.AsHash C.AsScriptData) (PlutusTx.fromBuiltin bs))

fromCardanoMintValue :: C.TxMintValue build era -> Value
fromCardanoMintValue C.TxMintNone              = mempty
fromCardanoMintValue (C.TxMintValue _ value _) = fromCardanoValue value

toCardanoMintValue
    :: Redeemers
    -> Value
    -> Map.Map P.MintingPolicyHash P.MintingPolicy
    -> Either ToCardanoError (C.TxMintValue C.BuildTx C.BabbageEra)
toCardanoMintValue redeemers value mps =
    let indexedMps = Prelude.zip [0..] $ Map.toList mps
     in C.TxMintValue C.MultiAssetInBabbageEra
        <$> toCardanoValue value
        <*> (C.BuildTxWith . Map.fromList <$> traverse indexedMpsToCardanoMintWitness indexedMps)
 where
    indexedMpsToCardanoMintWitness (idx, (mph, mp)) =
        (,) <$> toCardanoPolicyId mph
            <*> toCardanoMintWitness redeemers idx mp

fromCardanoValue :: C.Value -> Value
fromCardanoValue (C.valueToList -> list) = foldMap toValue list
    where
        toValue (C.AdaAssetId, C.Quantity q) = Ada.lovelaceValueOf q
        toValue (C.AssetId policyId assetName, C.Quantity q)
            = singleton (mpsSymbol $ fromCardanoPolicyId policyId) (fromCardanoAssetName assetName) q

toCardanoValue :: Value -> Either ToCardanoError C.Value
toCardanoValue = fmap C.valueFromList . traverse fromValue . flattenValue
    where
        fromValue (currencySymbol, tokenName, amount)
            | currencySymbol == Ada.adaSymbol && tokenName == Ada.adaToken =
                pure (C.AdaAssetId, C.Quantity amount)
            | otherwise =
                (,) <$> (C.AssetId <$> toCardanoPolicyId (currencyMPSHash currencySymbol) <*> pure (toCardanoAssetName tokenName)) <*> pure (C.Quantity amount)

fromCardanoPolicyId :: C.PolicyId -> P.MintingPolicyHash
fromCardanoPolicyId (C.PolicyId sh) = P.MintingPolicyHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes sh)

toCardanoPolicyId :: P.MintingPolicyHash -> Either ToCardanoError C.PolicyId
toCardanoPolicyId (P.MintingPolicyHash bs) = C.PolicyId <$> tag "toCardanoPolicyId" (tag (show (BS.length (PlutusTx.fromBuiltin bs)) <> " bytes") (deserialiseFromRawBytes C.AsScriptHash (PlutusTx.fromBuiltin bs)))

fromCardanoAssetName :: C.AssetName -> TokenName
fromCardanoAssetName (C.AssetName bs) = TokenName $ PlutusTx.toBuiltin bs

toCardanoAssetName :: TokenName -> C.AssetName
toCardanoAssetName (TokenName bs) = C.AssetName $ PlutusTx.fromBuiltin bs

fromCardanoFee :: C.TxFee era -> Value
fromCardanoFee (C.TxFeeImplicit _)          = mempty
fromCardanoFee (C.TxFeeExplicit _ lovelace) = fromCardanoLovelace lovelace

toCardanoFee :: Value -> Either ToCardanoError (C.TxFee C.BabbageEra)
toCardanoFee value = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra <$> toCardanoLovelace value

fromCardanoLovelace :: C.Lovelace -> Value
fromCardanoLovelace (C.lovelaceToQuantity -> C.Quantity lovelace) = Ada.lovelaceValueOf lovelace

toCardanoLovelace :: Value -> Either ToCardanoError C.Lovelace
toCardanoLovelace value = if value == Ada.lovelaceValueOf lovelace then pure . C.quantityToLovelace . C.Quantity $ lovelace else Left ValueNotPureAda
    where
        Ada.Lovelace lovelace = Ada.fromValue value

fromCardanoValidityRange :: (C.TxValidityLowerBound era, C.TxValidityUpperBound era) -> P.SlotRange
fromCardanoValidityRange (l, u) = Interval (fromCardanoValidityLowerBound l) (fromCardanoValidityUpperBound u)

toCardanoValidityRange :: P.SlotRange -> Either ToCardanoError (C.TxValidityLowerBound C.BabbageEra, C.TxValidityUpperBound C.BabbageEra)
toCardanoValidityRange (Interval l u) = (,) <$> toCardanoValidityLowerBound l <*> toCardanoValidityUpperBound u

fromCardanoValidityLowerBound :: C.TxValidityLowerBound era -> LowerBound P.Slot
fromCardanoValidityLowerBound C.TxValidityNoLowerBound          = LowerBound NegInf True
fromCardanoValidityLowerBound (C.TxValidityLowerBound _ slotNo) = LowerBound (Finite $ fromCardanoSlotNo slotNo) True

toCardanoValidityLowerBound :: LowerBound P.Slot -> Either ToCardanoError (C.TxValidityLowerBound C.BabbageEra)
toCardanoValidityLowerBound (LowerBound NegInf _) = pure C.TxValidityNoLowerBound
toCardanoValidityLowerBound (LowerBound (Finite slotNo) closed)
    = pure . C.TxValidityLowerBound C.ValidityLowerBoundInBabbageEra . toCardanoSlotNo $ if slotNo < 0 then 0 else if closed then slotNo else slotNo + 1
toCardanoValidityLowerBound (LowerBound PosInf _) = Left InvalidValidityRange

fromCardanoValidityUpperBound :: C.TxValidityUpperBound era -> UpperBound P.Slot
fromCardanoValidityUpperBound (C.TxValidityNoUpperBound _)      = UpperBound PosInf True
fromCardanoValidityUpperBound (C.TxValidityUpperBound _ slotNo) = UpperBound (Finite $ fromCardanoSlotNo slotNo) False

toCardanoValidityUpperBound :: UpperBound P.Slot -> Either ToCardanoError (C.TxValidityUpperBound C.BabbageEra)
toCardanoValidityUpperBound (UpperBound PosInf _) = pure $ C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra
toCardanoValidityUpperBound (UpperBound (Finite slotNo) closed)
    = pure . C.TxValidityUpperBound C.ValidityUpperBoundInBabbageEra . toCardanoSlotNo $ if closed then slotNo + 1 else slotNo
toCardanoValidityUpperBound (UpperBound NegInf _) = Left InvalidValidityRange

fromCardanoSlotNo :: C.SlotNo -> P.Slot
fromCardanoSlotNo (C.SlotNo w64) = P.Slot (toInteger w64)

toCardanoSlotNo :: P.Slot -> C.SlotNo
toCardanoSlotNo (P.Slot i) = C.SlotNo (fromInteger i)

fromCardanoScriptData :: C.ScriptData -> BuiltinData
fromCardanoScriptData = dataToBuiltinData . C.toPlutusData

toCardanoScriptData :: BuiltinData -> C.ScriptData
toCardanoScriptData = C.fromPlutusData . builtinDataToData

fromCardanoScriptInEra :: C.ScriptInEra era -> Maybe P.Script
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV1InAlonzo (C.PlutusScript C.PlutusScriptV1 script)) =
    Just $ fromCardanoPlutusScript script
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV1InBabbage (C.PlutusScript C.PlutusScriptV1 script)) =
    Just $ fromCardanoPlutusScript script
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV2InBabbage (C.PlutusScript C.PlutusScriptV2 script)) =
    Just $ fromCardanoPlutusScript script
fromCardanoScriptInEra (C.ScriptInEra _ C.SimpleScript{}) = Nothing

toCardanoPlutusV1ScriptInEra :: P.Script -> Either ToCardanoError (C.ScriptInEra C.BabbageEra)
toCardanoPlutusV1ScriptInEra script = C.ScriptInEra C.PlutusScriptV1InBabbage . C.PlutusScript C.PlutusScriptV1 <$> toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) script

-- TODO: Is there a way to combine this with 'toCardanoPlutusV1ScriptInEra'.
toCardanoPlutusV2ScriptInEra :: P.Script -> Either ToCardanoError (C.ScriptInEra C.BabbageEra)
toCardanoPlutusV2ScriptInEra script = C.ScriptInEra C.PlutusScriptV2InBabbage . C.PlutusScript C.PlutusScriptV2 <$> toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script

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

fromCardanoScriptInAnyLang :: C.ScriptInAnyLang -> Maybe P.Script
fromCardanoScriptInAnyLang (C.ScriptInAnyLang _sl (C.SimpleScript _ssv _ss)) = Nothing
fromCardanoScriptInAnyLang (C.ScriptInAnyLang _sl (C.PlutusScript psv ps)) = Just $ case psv of
     C.PlutusScriptV1 -> fromCardanoPlutusScript ps
     C.PlutusScriptV2 -> fromCardanoPlutusScript ps

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
    | MissingTxInType
    | MissingMintingPolicyRedeemer
    | MissingMintingPolicy
    | ScriptPurposeNotSupported ScriptTag
    | Tag String ToCardanoError
    | ReferenceScriptNotSupported         -- FIXME this should not exist
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
    pretty MissingTxInType                    = "Missing TxInType"
    pretty MissingMintingPolicyRedeemer       = "Missing minting policy redeemer"
    pretty MissingMintingPolicy               = "Missing minting policy"
    pretty (ScriptPurposeNotSupported p)      = "Script purpose not supported:" <+> viaShow p
    pretty ReferenceScriptNotSupported        = "Reference script not supported"
    pretty (Tag t err)                        = pretty t <> colon <+> pretty err

zeroExecutionUnits :: C.ExecutionUnits
zeroExecutionUnits = C.ExecutionUnits 0 0
