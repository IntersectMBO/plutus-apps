{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}

{-# OPTIONS_GHC -Wno-orphans    #-}
{-# LANGUAGE LambdaCase         #-}

{-|

Interface to the transaction types from 'cardano-api'

-}
module Ledger.Tx.CardanoAPI(
  SomeCardanoApiTx(..)
  , txOutRefs
  , unspentOutputsTx
  , fromCardanoTxId
  , fromCardanoTxIn
  , fromCardanoTxInsCollateral
  , fromCardanoTxOut
  , fromCardanoTxOutDatum
  , fromCardanoTxOutDatumHash
  , fromCardanoAddress
  , fromCardanoMintValue
  , fromCardanoValue
  , fromCardanoPolicyId
  , fromCardanoFee
  , fromCardanoValidityRange
  , fromCardanoScriptInEra
  , fromCardanoPaymentKeyHash
  , fromCardanoScriptData
  , fromTxScriptValidity
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
  , toCardanoTxOutBabbage
  , toCardanoTxOutDatumHash
  , toCardanoTxOutDatumHashBabbage
  , toCardanoAddress
  , toCardanoMintValue
  , toCardanoValue
  , toCardanoFee
  , toCardanoValidityRange
  , toCardanoScriptInEra
  , toCardanoPaymentKeyHash
  , toCardanoScriptHash
  , toCardanoScriptDataHash
  , toCardanoTxId
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
import Data.Set qualified as Set
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Ledger.Ada qualified as Ada
import Ledger.Address qualified as L
import Ledger.Scripts qualified as L
import Ledger.Slot qualified as L
import Ledger.Tx.CardanoAPITemp (makeTransactionBody')
import Ledger.Tx.Internal qualified as L
import Ledger.Tx.Types qualified as L
import Plutus.Script.Utils.V2.Scripts qualified as PV2
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Credential qualified as Credential
import Plutus.V1.Ledger.Tx qualified as PV1
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty (pretty), colon, viaShow, (<+>))

instance (Typeable era, Typeable mode) => OpenApi.ToSchema (C.EraInMode era mode) where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "EraInMode") $ sketchSchema C.AlonzoEraInCardanoMode

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
parseBabbageEraInCardanoModeTx =
    undefined
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
    txSchema <- declareSchemaRef (Proxy :: Proxy (C.Tx C.AlonzoEra))
    eraInModeSchema <- declareSchemaRef (Proxy :: Proxy (C.EraInMode C.AlonzoEra C.CardanoMode))
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
    plutusTxOuts = mapMaybe (either (const Nothing) Just . fromCardanoTxOut) txOuts

unspentOutputsTx :: SomeCardanoApiTx -> Map PV1.TxOutRef PV1.TxOut
unspentOutputsTx tx = Map.fromList $ swap <$> txOutRefs tx

-- | Given a 'C.TxScriptValidity era', if the @era@ supports scripts, return a
-- @True@ or @False@ depending on script validity. If the @era@ does not support
-- scripts, always return @True@.
fromTxScriptValidity :: C.TxScriptValidity era -> Bool
fromTxScriptValidity C.TxScriptValidityNone                                                       = True
fromTxScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptValid)    = True
fromTxScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInAlonzoEra C.ScriptInvalid)  = False
fromTxScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptValid)   = True
fromTxScriptValidity (C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptInvalid) = False

-- | Given a 'C.TxBody from a 'C.Tx era', return the datums and redeemers along
-- with their hashes.
scriptDataFromCardanoTxBody
  :: C.TxBody era
  -> (Map PV1.DatumHash PV1.Datum, Map PV1.RedeemerHash PV1.Redeemer)
scriptDataFromCardanoTxBody C.ByronTxBody {} = (mempty, mempty)
scriptDataFromCardanoTxBody (C.ShelleyTxBody _ _ _ C.TxBodyNoScriptData _ _) =
  (mempty, mempty)
scriptDataFromCardanoTxBody
  (C.ShelleyTxBody _ _ _ (C.TxBodyScriptData _ (Alonzo.TxDats' dats) (Alonzo.Redeemers' reds)) _ _) =

  let datums = Map.fromList
             $ fmap ( (\d -> (L.datumHash d, d))
                    . PV1.Datum
                    . fromCardanoScriptData
                    . C.fromAlonzoData
                    )
             $ Map.elems dats
      redeemers = Map.fromList
                $ fmap ( (\r -> (L.redeemerHash r, r))
                       . PV1.Redeemer
                       . fromCardanoScriptData
                       . C.fromAlonzoData
                       . fst
                       )
                $ Map.elems reds
   in (datums, redeemers)

-- | Extract plutus scripts from a Cardano API tx body.
--
-- Note that Plutus scripts are only supported in Alonzo era and onwards.
plutusScriptsFromTxBody :: C.TxBody era -> Map L.ScriptHash PV1.Script
plutusScriptsFromTxBody C.ByronTxBody {} = mempty
plutusScriptsFromTxBody (C.ShelleyTxBody shelleyBasedEra _ scripts _ _ _) =
  Map.fromList $ mapMaybe (fromLedgerScript shelleyBasedEra) scripts

-- | Convert a script from a Cardano api in shelley based era to a Plutus script along with it's hash.
--
-- Note that Plutus scripts are only supported in Alonzo era and onwards.
fromLedgerScript
  :: C.ShelleyBasedEra era
  -> Ledger.Script (C.ShelleyLedgerEra era)
  -> Maybe (L.ScriptHash, PV1.Script)
fromLedgerScript C.ShelleyBasedEraShelley _      = Nothing
fromLedgerScript C.ShelleyBasedEraAllegra _      = Nothing
fromLedgerScript C.ShelleyBasedEraMary _         = Nothing
fromLedgerScript C.ShelleyBasedEraAlonzo script  = fromLedgerPlutusScript script
fromLedgerScript C.ShelleyBasedEraBabbage script = fromLedgerPlutusScript script

-- | Convert a `cardano-ledger` Plutus script from the Alonzo era and onwards to
-- a 'Script' along with it's hash.
fromLedgerPlutusScript :: Alonzo.Script a -> Maybe (L.ScriptHash, PV1.Script)
fromLedgerPlutusScript Alonzo.TimelockScript {} = Nothing
fromLedgerPlutusScript (Alonzo.PlutusScript Alonzo.PlutusV1 bs) =
  let script = fmap (\s -> (L.scriptHash s, s))
             $ deserialiseOrFail
             $ BSL.fromStrict
             $ SBS.fromShort bs
   in either (const Nothing) Just script
fromLedgerPlutusScript (Alonzo.PlutusScript Alonzo.PlutusV2 bs) =
  let script = fmap (\s -> (PV2.scriptHash s, s))
             $ deserialiseOrFail
             $ BSL.fromStrict
             $ SBS.fromShort bs
   in either (const Nothing) Just script

toCardanoTxBodyContent
    :: [L.PaymentPubKeyHash] -- ^ Required signers of the transaction
    -> Maybe C.ProtocolParameters -- ^ Protocol parameters to use. Building Plutus transactions will fail if this is 'Nothing'
    -> C.NetworkId -- ^ Network ID
    -> L.Tx
    -> Either ToCardanoError (C.TxBodyContent C.BuildTx C.AlonzoEra)
toCardanoTxBodyContent sigs protocolParams networkId
  -- TODO: translate all fields
  tx@L.Tx{txInputs, txCollateral, txOutputs, txFee, txValidRange, txWithdrawals, txScripts, txData} = do
    txIns <- traverse (toCardanoTxInBuild tx) txInputs
    txInsCollateral <- toCardanoTxInsCollateral txCollateral
    txOuts <- traverse (toCardanoTxOut networkId (lookupDatum txData)) txOutputs
    txFee' <- toCardanoFee txFee
    txValidityRange <- toCardanoValidityRange txValidRange
    txMintValue <- toCardanoMintValue tx
    txExtraKeyWits <- C.TxExtraKeyWitnesses C.ExtraKeyWitnessesInAlonzoEra <$> traverse toCardanoPaymentKeyHash sigs
    withdrawals <- toWithdrawals txScripts networkId txWithdrawals
    pure $ C.TxBodyContent
        { txIns = txIns
        , txInsCollateral = txInsCollateral
        , txInsReference = C.TxInsReferenceNone -- TODO Change when finally supporting Babbage era txs
        , txOuts = txOuts
        , txTotalCollateral = C.TxTotalCollateralNone -- TODO Change when going to Babbage era txs
        , txReturnCollateral = C.TxReturnCollateralNone -- TODO Change when going to Babbage era txs
        , txFee = txFee'
        , txValidityRange = txValidityRange
        , txMintValue = txMintValue
        , txProtocolParams = C.BuildTxWith protocolParams
        , txScriptValidity = C.TxScriptValidityNone
        , txExtraKeyWits
        -- unused:
        , txMetadata = C.TxMetadataNone
        , txAuxScripts = C.TxAuxScriptsNone
        , txWithdrawals = withdrawals
        , txCertificates = C.TxCertificatesNone
        , txUpdateProposal = C.TxUpdateProposalNone
        }

toWithdrawals :: Map L.ScriptHash PV1.Script
  -> C.NetworkId
  -> [L.Withdrawal]
  -> Either ToCardanoError (C.TxWithdrawals C.BuildTx C.AlonzoEra)
toWithdrawals txScripts networkId = \case
  [] -> pure C.TxWithdrawalsNone
  xs -> C.TxWithdrawals C.WithdrawalsInAlonzoEra <$> mapM toWithdraw xs

  where
    toWithdraw L.Withdrawal{withdrawalCredential, withdrawalAmount, withdrawalRedeemer} = do
      saddr <- toCardanoStakeAddress networkId withdrawalCredential
      witness <- toStakeWitness withdrawalRedeemer withdrawalCredential
      pure (saddr, C.Lovelace withdrawalAmount, witness)

    toStakeWitness withdrawalRedeemer cred = case cred of
      PV1.PubKeyCredential _pkh -> pure $ C.BuildTxWith $ C.KeyWitness C.KeyWitnessForStakeAddr
      PV1.ScriptCredential _vh -> case (,) <$> withdrawalRedeemer <*> L.lookupValidator txScripts _vh of
        Just (redeemer, PV1.Validator script) -> C.BuildTxWith . C.ScriptWitness C.ScriptWitnessForStakeAddr <$> toCardanoScriptWitness C.NoScriptDatumForStake redeemer script
        Nothing                    -> Left MissingStakeValidator

toCardanoStakeAddress :: C.NetworkId -> PV1.Credential -> Either ToCardanoError C.StakeAddress
toCardanoStakeAddress networkId credential =
  C.StakeAddress (C.toShelleyNetwork networkId) . C.toShelleyStakeCredential <$> toCardanoStakingCredential credential

toCardanoStakingCredential :: PV1.Credential -> Either ToCardanoError C.StakeCredential
toCardanoStakingCredential (PV1.PubKeyCredential pubKeyHash) = C.StakeCredentialByKey <$> toCardanoStakeKeyHash pubKeyHash
toCardanoStakingCredential (PV1.ScriptCredential validatorHash) = C.StakeCredentialByScript <$> toCardanoScriptHash validatorHash


toCardanoTxBody ::
    [L.PaymentPubKeyHash] -- ^ Required signers of the transaction
    -> Maybe C.ProtocolParameters -- ^ Protocol parameters to use. Building Plutus transactions will fail if this is 'Nothing'
    -> C.NetworkId -- ^ Network ID
    -> L.Tx
    -> Either ToCardanoError (C.TxBody C.AlonzoEra)
toCardanoTxBody sigs protocolParams networkId tx = do
    txBodyContent <- toCardanoTxBodyContent sigs protocolParams networkId tx
    makeTransactionBody mempty txBodyContent

makeTransactionBody
    :: Map Alonzo.RdmrPtr Alonzo.ExUnits
    -> C.TxBodyContent C.BuildTx C.AlonzoEra
    -> Either ToCardanoError (C.TxBody C.AlonzoEra)
makeTransactionBody exUnits txBodyContent =
  first (TxBodyError . C.displayError) $ makeTransactionBody' exUnits txBodyContent

fromCardanoTxIn :: C.TxIn -> PV1.TxOutRef
fromCardanoTxIn (C.TxIn txId (C.TxIx txIx)) = PV1.TxOutRef (fromCardanoTxId txId) (toInteger txIx)

toCardanoTxInBuild :: L.Tx -> L.TxInput -> Either ToCardanoError (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.AlonzoEra))
toCardanoTxInBuild tx (L.TxInput txInRef txInType) = (,) <$> toCardanoTxIn txInRef <*> (C.BuildTxWith <$> toCardanoTxInWitness tx txInType)

toCardanoTxIn :: PV1.TxOutRef -> Either ToCardanoError C.TxIn
toCardanoTxIn (PV1.TxOutRef txId txIx) = C.TxIn <$> toCardanoTxId txId <*> pure (C.TxIx (fromInteger txIx))

fromCardanoTxId :: C.TxId -> PV1.TxId
fromCardanoTxId txId = PV1.TxId $ PlutusTx.toBuiltin $ C.serialiseToRawBytes txId

toCardanoTxId :: PV1.TxId -> Either ToCardanoError C.TxId
toCardanoTxId (PV1.TxId bs) =
    tag "toCardanoTxId"
    $ deserialiseFromRawBytes C.AsTxId $ PlutusTx.fromBuiltin bs

fromCardanoTxInsCollateral :: C.TxInsCollateral era -> Set.Set PV1.TxIn
fromCardanoTxInsCollateral C.TxInsCollateralNone       = mempty
fromCardanoTxInsCollateral (C.TxInsCollateral _ txIns) = Set.fromList $ fmap (PV1.pubKeyTxIn . fromCardanoTxIn) txIns

toCardanoTxInsCollateral :: [L.TxInput] -> Either ToCardanoError (C.TxInsCollateral C.AlonzoEra)
toCardanoTxInsCollateral = fmap (C.TxInsCollateral C.CollateralInAlonzoEra) . traverse (toCardanoTxIn . L.txInputRef)

toCardanoTxInWitness :: L.Tx -> L.TxInputType -> Either ToCardanoError (C.Witness C.WitCtxTxIn C.AlonzoEra)
toCardanoTxInWitness _ L.TxConsumePublicKeyAddress = pure (C.KeyWitness C.KeyWitnessForSpending)
toCardanoTxInWitness _ L.TxConsumeSimpleScriptAddress = Left SimpleScriptsNotSupportedToCardano -- TODO: Better support for simple scripts
toCardanoTxInWitness tx
    (L.TxConsumeScriptAddress
        (L.Redeemer redeemer)
        validatorHash
        datumHash)
    = do
      (PV1.Datum datum) <- maybe (Left MissingDatum) pure $ Map.lookup datumHash (L.txData tx)
      (PV1.Validator validator) <- maybe (Left MissingInputValidator) pure $ L.lookupValidator (L.txScripts tx) validatorHash
      C.ScriptWitness C.ScriptWitnessForSpending <$>
        (C.PlutusScriptWitness C.PlutusScriptV1InAlonzo C.PlutusScriptV1
        <$> fmap C.PScript (toCardanoPlutusScript validator)
        <*> pure (C.ScriptDatumForTxIn $ toCardanoScriptData datum)
        <*> pure (toCardanoScriptData redeemer)
        <*> pure zeroExecutionUnits
        )



toCardanoMintWitness :: PV1.Redeemer -> Maybe PV1.MintingPolicy -> Either ToCardanoError (C.ScriptWitness C.WitCtxMint C.AlonzoEra)
toCardanoMintWitness _ Nothing = Left MissingMintingPolicy
toCardanoMintWitness redeemer (Just (PV1.MintingPolicy script)) = toCardanoScriptWitness C.NoScriptDatumForMint redeemer script

-- toCardanoStakeWitness :: PV1.Redeemer -> PV1.StakeValidator -> Either ToCardanoError (C.ScriptWitness C.WitCtxStake C.AlonzoEra)
toCardanoScriptWitness :: PV1.ToData a =>
  C.ScriptDatum witctx
  -> a
  -> PV1.Script
  -> Either ToCardanoError (C.ScriptWitness witctx C.AlonzoEra)
toCardanoScriptWitness datum redeemer script = do
    C.PlutusScriptWitness C.PlutusScriptV1InAlonzo C.PlutusScriptV1
        <$> fmap C.PScript (toCardanoPlutusScript script)
        <*> pure datum
        <*> pure (C.fromPlutusData $ PV1.toData redeemer)
        <*> pure zeroExecutionUnits

-- TODO Handle reference script once 'PV1.TxOut' supports it (or when we use
-- exclusively 'C.TxOut' in all the codebase).
fromCardanoTxOut :: C.TxOut C.CtxTx era -> Either FromCardanoError PV1.TxOut
fromCardanoTxOut (C.TxOut addr value datumHash _) =
    PV1.TxOut
    <$> fromCardanoAddress addr
    <*> pure (fromCardanoTxOutValue value)
    <*> pure (fromCardanoTxOutDatumHash datumHash)

toCardanoTxOutBabbage
    :: C.NetworkId
    -> (Maybe L.DatumHash -> Either ToCardanoError (C.TxOutDatum ctx C.BabbageEra))
    -> PV1.TxOut
    -> Either ToCardanoError (C.TxOut ctx C.BabbageEra)
toCardanoTxOutBabbage networkId fromHash (PV1.TxOut addr value datumHash) =
    C.TxOut <$> toCardanoAddressBabbage networkId addr
            <*> toCardanoTxOutValueBabbage value
            <*> fromHash datumHash
            <*> pure C.ReferenceScriptNone

toCardanoTxOut
    :: C.NetworkId
    -> (Maybe PV1.DatumHash -> Either ToCardanoError (C.TxOutDatum ctx C.AlonzoEra))
    -> PV1.TxOut
    -> Either ToCardanoError (C.TxOut ctx C.AlonzoEra)
toCardanoTxOut networkId fromHash (PV1.TxOut addr value datumHash) =
    C.TxOut <$> toCardanoAddress networkId addr
            <*> toCardanoTxOutValue value
            <*> fromHash datumHash
            <*> pure C.ReferenceScriptNone

toCardanoTxOutUnsafe
    :: C.NetworkId
    -> (Maybe PV1.DatumHash -> Either ToCardanoError (C.TxOutDatum ctx C.AlonzoEra))
    -> PV1.TxOut
    -> Either ToCardanoError (C.TxOut ctx C.AlonzoEra)
toCardanoTxOutUnsafe networkId fromHash (PV1.TxOut addr value datumHash) =
    C.TxOut <$> toCardanoAddress networkId addr
            <*> toCardanoTxOutValueUnsafe value
            <*> fromHash datumHash
            <*> pure C.ReferenceScriptNone

lookupDatum :: Map PV1.DatumHash PV1.Datum -> Maybe PV1.DatumHash -> Either ToCardanoError (C.TxOutDatum C.CtxTx C.AlonzoEra)
lookupDatum datums datumHash =
    case flip Map.lookup datums =<< datumHash of
        Just datum -> pure $ C.TxOutDatumInTx C.ScriptDataInAlonzoEra (toCardanoScriptData $ PV1.getDatum datum)
        Nothing    -> toCardanoTxOutDatumHash datumHash

fromCardanoAddress :: C.AddressInEra era -> Either FromCardanoError PV1.Address
fromCardanoAddress (C.AddressInEra C.ByronAddressInAnyEra (C.ByronAddress address)) =
    Right $ PV1.Address plutusCredential Nothing
    where
      plutusCredential :: Credential.Credential
      plutusCredential =
          Credential.PubKeyCredential
        $ PV1.PubKeyHash
        $ PlutusTx.toBuiltin
        $ addrToBase58 address

fromCardanoAddress (C.AddressInEra _ (C.ShelleyAddress _ paymentCredential stakeAddressReference)) =
    PV1.Address (fromCardanoPaymentCredential (C.fromShelleyPaymentCredential paymentCredential))
        <$> fromCardanoStakeAddressReference (C.fromShelleyStakeReference stakeAddressReference)

toCardanoAddress :: C.NetworkId -> PV1.Address -> Either ToCardanoError (C.AddressInEra C.AlonzoEra)
toCardanoAddress networkId (PV1.Address addressCredential addressStakingCredential) =
    C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraAlonzo) <$>
        (C.makeShelleyAddress networkId
            <$> toCardanoPaymentCredential addressCredential
            <*> toCardanoStakeAddressReference addressStakingCredential)

toCardanoAddressBabbage :: C.NetworkId -> L.Address -> Either ToCardanoError (C.AddressInEra C.BabbageEra)
toCardanoAddressBabbage networkId (L.Address addressCredential addressStakingCredential) =
    C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) <$>
        (C.makeShelleyAddress networkId
            <$> toCardanoPaymentCredential addressCredential
            <*> toCardanoStakeAddressReference addressStakingCredential)

fromCardanoPaymentCredential :: C.PaymentCredential -> Credential.Credential
fromCardanoPaymentCredential (C.PaymentCredentialByKey paymentKeyHash) = Credential.PubKeyCredential (fromCardanoPaymentKeyHash paymentKeyHash)
fromCardanoPaymentCredential (C.PaymentCredentialByScript scriptHash) = Credential.ScriptCredential (fromCardanoScriptHash scriptHash)

toCardanoPaymentCredential :: Credential.Credential -> Either ToCardanoError C.PaymentCredential
toCardanoPaymentCredential (Credential.PubKeyCredential pubKeyHash) = C.PaymentCredentialByKey <$> toCardanoPaymentKeyHash (L.PaymentPubKeyHash pubKeyHash)
toCardanoPaymentCredential (Credential.ScriptCredential validatorHash) = C.PaymentCredentialByScript <$> toCardanoScriptHash validatorHash

fromCardanoPaymentKeyHash :: C.Hash C.PaymentKey -> PV1.PubKeyHash
fromCardanoPaymentKeyHash paymentKeyHash = PV1.PubKeyHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes paymentKeyHash

toCardanoPaymentKeyHash :: L.PaymentPubKeyHash -> Either ToCardanoError (C.Hash C.PaymentKey)
toCardanoPaymentKeyHash (L.PaymentPubKeyHash (PV1.PubKeyHash bs)) =
    let bsx = PlutusTx.fromBuiltin bs
        tg = "toCardanoPaymentKeyHash (" <> show (BS.length bsx) <> " bytes)"
    in tag tg $ deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) bsx

fromCardanoScriptHash :: C.ScriptHash -> PV1.ValidatorHash
fromCardanoScriptHash scriptHash = PV1.ValidatorHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes scriptHash

toCardanoScriptHash :: PV1.ValidatorHash -> Either ToCardanoError C.ScriptHash
toCardanoScriptHash (PV1.ValidatorHash bs) = tag "toCardanoScriptHash" $ deserialiseFromRawBytes C.AsScriptHash $ PlutusTx.fromBuiltin bs

fromCardanoStakeAddressReference :: C.StakeAddressReference -> Either FromCardanoError (Maybe Credential.StakingCredential)
fromCardanoStakeAddressReference C.NoStakeAddress = pure Nothing
fromCardanoStakeAddressReference (C.StakeAddressByValue stakeCredential) =
    pure $ Just (Credential.StakingHash $ fromCardanoStakeCredential stakeCredential)
fromCardanoStakeAddressReference C.StakeAddressByPointer{} = pure Nothing

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

toCardanoTxOutValue :: PV1.Value -> Either ToCardanoError (C.TxOutValue C.AlonzoEra)
toCardanoTxOutValue value = do
    when (Ada.fromValue value == mempty) (Left OutputHasZeroAda)
    C.TxOutValue C.MultiAssetInAlonzoEra <$> toCardanoValue value

toCardanoTxOutValueUnsafe :: PV1.Value -> Either ToCardanoError (C.TxOutValue C.AlonzoEra)
toCardanoTxOutValueUnsafe value = C.TxOutValue C.MultiAssetInAlonzoEra <$> toCardanoValue value

toCardanoTxOutValueBabbage :: PV1.Value -> Either ToCardanoError (C.TxOutValue C.BabbageEra)
toCardanoTxOutValueBabbage value = C.TxOutValue C.MultiAssetInBabbageEra <$> toCardanoValue value

fromCardanoTxOutDatumHash :: C.TxOutDatum C.CtxTx era -> Maybe L.DatumHash
fromCardanoTxOutDatumHash C.TxOutDatumNone       = Nothing
fromCardanoTxOutDatumHash (C.TxOutDatumHash _ h) = Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatumHash (C.TxOutDatumInTx _ d) = Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d))
fromCardanoTxOutDatumHash (C.TxOutDatumInline _ d) = Just $ PV1.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes (C.hashScriptData d))

fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> PV2.OutputDatum
fromCardanoTxOutDatum C.TxOutDatumNone       = PV2.NoOutputDatum
fromCardanoTxOutDatum (C.TxOutDatumHash _ h) = PV2.OutputDatumHash $ PV2.DatumHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes h)
fromCardanoTxOutDatum (C.TxOutDatumInTx _ d) = PV2.OutputDatum $ PV2.Datum $ fromCardanoScriptData d
fromCardanoTxOutDatum (C.TxOutDatumInline _ d) = PV2.OutputDatum $ PV2.Datum $ fromCardanoScriptData d

toCardanoTxOutDatumHashBabbage :: Maybe L.DatumHash -> Either ToCardanoError (C.TxOutDatum ctx C.BabbageEra)
toCardanoTxOutDatumHashBabbage Nothing          = pure C.TxOutDatumNone
toCardanoTxOutDatumHashBabbage (Just datumHash) = C.TxOutDatumHash C.ScriptDataInBabbageEra <$> toCardanoScriptDataHash datumHash

toCardanoTxOutDatumHash :: Maybe L.DatumHash -> Either ToCardanoError (C.TxOutDatum ctx C.AlonzoEra)
toCardanoTxOutDatumHash Nothing          = pure C.TxOutDatumNone
toCardanoTxOutDatumHash (Just datumHash) = C.TxOutDatumHash C.ScriptDataInAlonzoEra <$> toCardanoScriptDataHash datumHash

toCardanoScriptDataHash :: PV1.DatumHash -> Either ToCardanoError (C.Hash C.ScriptData)
toCardanoScriptDataHash (PV1.DatumHash bs) = tag "toCardanoTxOutDatumHash" (deserialiseFromRawBytes (C.AsHash C.AsScriptData) (PlutusTx.fromBuiltin bs))

fromCardanoMintValue :: C.TxMintValue build era -> PV1.Value
fromCardanoMintValue C.TxMintNone              = mempty
fromCardanoMintValue (C.TxMintValue _ value _) = fromCardanoValue value

toCardanoMintValue :: L.Tx -> Either ToCardanoError (C.TxMintValue C.BuildTx C.AlonzoEra)
toCardanoMintValue tx@L.Tx{..} =
    let indexedMps = Map.assocs txMintingScripts
     in C.TxMintValue C.MultiAssetInAlonzoEra
        <$> toCardanoValue txMint
        <*> (C.BuildTxWith . Map.fromList <$> traverse (\(mph, rd) ->
          (,) <$> toCardanoPolicyId mph <*> toCardanoMintWitness rd (L.lookupMintingPolicy (L.txScripts tx) mph)) indexedMps)

fromCardanoValue :: C.Value -> PV1.Value
fromCardanoValue (C.valueToList -> list) = foldMap toValue list
    where
        toValue (C.AdaAssetId, C.Quantity q) = Ada.lovelaceValueOf q
        toValue (C.AssetId policyId assetName, C.Quantity q)
            = Value.singleton (Value.mpsSymbol $ fromCardanoPolicyId policyId) (fromCardanoAssetName assetName) q

toCardanoValue :: PV1.Value -> Either ToCardanoError C.Value
toCardanoValue = fmap C.valueFromList . traverse fromValue . Value.flattenValue
    where
        fromValue (currencySymbol, tokenName, amount)
            | currencySymbol == Ada.adaSymbol && tokenName == Ada.adaToken =
                pure (C.AdaAssetId, C.Quantity amount)
            | otherwise =
                (,) <$> (C.AssetId <$> toCardanoPolicyId (Value.currencyMPSHash currencySymbol) <*> pure (toCardanoAssetName tokenName)) <*> pure (C.Quantity amount)

fromCardanoPolicyId :: C.PolicyId -> PV1.MintingPolicyHash
fromCardanoPolicyId (C.PolicyId scriptHash) = PV1.MintingPolicyHash $ PlutusTx.toBuiltin (C.serialiseToRawBytes scriptHash)

toCardanoPolicyId :: PV1.MintingPolicyHash -> Either ToCardanoError C.PolicyId
toCardanoPolicyId (PV1.MintingPolicyHash bs) = C.PolicyId <$> tag "toCardanoPolicyId" (tag (show (BS.length (PlutusTx.fromBuiltin bs)) <> " bytes") (deserialiseFromRawBytes C.AsScriptHash (PlutusTx.fromBuiltin bs)))

fromCardanoAssetName :: C.AssetName -> Value.TokenName
fromCardanoAssetName (C.AssetName bs) = Value.TokenName $ PlutusTx.toBuiltin bs

toCardanoAssetName :: Value.TokenName -> C.AssetName
toCardanoAssetName (Value.TokenName bs) = C.AssetName $ PlutusTx.fromBuiltin bs

fromCardanoFee :: C.TxFee era -> PV1.Value
fromCardanoFee (C.TxFeeImplicit _)          = mempty
fromCardanoFee (C.TxFeeExplicit _ lovelace) = fromCardanoLovelace lovelace

toCardanoFee :: PV1.Value -> Either ToCardanoError (C.TxFee C.AlonzoEra)
toCardanoFee value = C.TxFeeExplicit C.TxFeesExplicitInAlonzoEra <$> toCardanoLovelace value

fromCardanoLovelace :: C.Lovelace -> PV1.Value
fromCardanoLovelace (C.lovelaceToQuantity -> C.Quantity lovelace) = Ada.lovelaceValueOf lovelace

toCardanoLovelace :: PV1.Value -> Either ToCardanoError C.Lovelace
toCardanoLovelace value = if value == Ada.lovelaceValueOf lovelace then pure . C.quantityToLovelace . C.Quantity $ lovelace else Left ValueNotPureAda
    where
        Ada.Lovelace lovelace = Ada.fromValue value

fromCardanoValidityRange :: (C.TxValidityLowerBound era, C.TxValidityUpperBound era) -> L.SlotRange
fromCardanoValidityRange (l, u) = PV1.Interval (fromCardanoValidityLowerBound l) (fromCardanoValidityUpperBound u)

toCardanoValidityRange :: L.SlotRange -> Either ToCardanoError (C.TxValidityLowerBound C.AlonzoEra, C.TxValidityUpperBound C.AlonzoEra)
toCardanoValidityRange (PV1.Interval l u) = (,) <$> toCardanoValidityLowerBound l <*> toCardanoValidityUpperBound u

fromCardanoValidityLowerBound :: C.TxValidityLowerBound era -> PV1.LowerBound L.Slot
fromCardanoValidityLowerBound C.TxValidityNoLowerBound = PV1.LowerBound PV1.NegInf True
fromCardanoValidityLowerBound (C.TxValidityLowerBound _ slotNo) = PV1.LowerBound (PV1.Finite $ fromCardanoSlotNo slotNo) True

toCardanoValidityLowerBound :: PV1.LowerBound L.Slot -> Either ToCardanoError (C.TxValidityLowerBound C.AlonzoEra)
toCardanoValidityLowerBound (PV1.LowerBound PV1.NegInf _) = pure C.TxValidityNoLowerBound
toCardanoValidityLowerBound (PV1.LowerBound (PV1.Finite slotNo) closed)
    = pure . C.TxValidityLowerBound C.ValidityLowerBoundInAlonzoEra . toCardanoSlotNo $ if slotNo < 0 then 0 else if closed then slotNo else slotNo + 1
toCardanoValidityLowerBound (PV1.LowerBound PV1.PosInf _) = Left InvalidValidityRange

fromCardanoValidityUpperBound :: C.TxValidityUpperBound era -> PV1.UpperBound L.Slot
fromCardanoValidityUpperBound (C.TxValidityNoUpperBound _) = PV1.UpperBound PV1.PosInf True
fromCardanoValidityUpperBound (C.TxValidityUpperBound _ slotNo) = PV1.UpperBound (PV1.Finite $ fromCardanoSlotNo slotNo) False

toCardanoValidityUpperBound :: PV1.UpperBound L.Slot -> Either ToCardanoError (C.TxValidityUpperBound C.AlonzoEra)
toCardanoValidityUpperBound (PV1.UpperBound PV1.PosInf _) = pure $ C.TxValidityNoUpperBound C.ValidityNoUpperBoundInAlonzoEra
toCardanoValidityUpperBound (PV1.UpperBound (PV1.Finite slotNo) closed)
    = pure . C.TxValidityUpperBound C.ValidityUpperBoundInAlonzoEra . toCardanoSlotNo $ if closed then slotNo + 1 else slotNo
toCardanoValidityUpperBound (PV1.UpperBound PV1.NegInf _) = Left InvalidValidityRange

fromCardanoSlotNo :: C.SlotNo -> L.Slot
fromCardanoSlotNo (C.SlotNo w64) = L.Slot (toInteger w64)

toCardanoSlotNo :: L.Slot -> C.SlotNo
toCardanoSlotNo (L.Slot i) = C.SlotNo (fromInteger i)

fromCardanoScriptData :: C.ScriptData -> PV1.BuiltinData
fromCardanoScriptData = PV1.dataToBuiltinData . C.toPlutusData

toCardanoScriptData :: PV1.BuiltinData -> C.ScriptData
toCardanoScriptData = C.fromPlutusData . PV1.builtinDataToData

fromCardanoScriptInEra :: C.ScriptInEra era -> Maybe PV1.Script
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV1InAlonzo (C.PlutusScript C.PlutusScriptV1 script)) =
    Just $ fromCardanoPlutusScript script
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV1InBabbage (C.PlutusScript C.PlutusScriptV1 script)) =
    Just $ fromCardanoPlutusScript script
fromCardanoScriptInEra (C.ScriptInEra C.PlutusScriptV2InBabbage (C.PlutusScript C.PlutusScriptV2 script)) =
    Just $ fromCardanoPlutusScript script
fromCardanoScriptInEra (C.ScriptInEra _ C.SimpleScript{}) = Nothing

toCardanoScriptInEra :: PV1.Script -> Either ToCardanoError (C.ScriptInEra C.AlonzoEra)
toCardanoScriptInEra script = C.ScriptInEra C.PlutusScriptV1InAlonzo . C.PlutusScript C.PlutusScriptV1 <$> toCardanoPlutusScript script

fromCardanoPlutusScript :: C.HasTypeProxy lang => C.PlutusScript lang -> PV1.Script
fromCardanoPlutusScript = Codec.deserialise . BSL.fromStrict . C.serialiseToRawBytes

toCardanoPlutusScript :: PV1.Script -> Either ToCardanoError (C.PlutusScript C.PlutusScriptV1)
toCardanoPlutusScript =
    tag "toCardanoPlutusScript"
    . deserialiseFromRawBytes (C.AsPlutusScript C.AsPlutusScriptV1) . BSL.toStrict . Codec.serialise

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
    | MissingStakeValidator
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
    pretty MissingInputValidator              = "Missing input validator."
    pretty MissingDatum                       = "Missing required datum."
    pretty MissingMintingPolicy               = "Missing minting policy."
    pretty MissingStakeValidator              = "Missing stake validator."
    pretty (Tag t err)                        = pretty t <> colon <+> pretty err

zeroExecutionUnits :: C.ExecutionUnits
zeroExecutionUnits = C.ExecutionUnits 0 0
