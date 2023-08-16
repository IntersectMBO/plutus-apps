{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}


module PlutusExample.ScriptContextChecker where

import Prelude hiding (($))

import Cardano.Api
import Cardano.Api.Byron
import Cardano.Api.Shelley
import Cardano.Api.Shelley qualified as Api

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except.Extra
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LB
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Records (HasField (..))

import Cardano.CLI.Shelley.Run.Query
import Cardano.Ledger.Alonzo qualified as Alonzo
import Cardano.Ledger.Alonzo.PParams qualified as Alonzo
import Cardano.Ledger.Alonzo.PlutusScriptApi qualified as Alonzo
import Cardano.Ledger.Alonzo.Tx qualified as Alonzo
import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo
import Cardano.Ledger.Alonzo.TxWitness qualified as Alonzo
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.Coin qualified as Ledger
import Cardano.Ledger.Shelley.API qualified as Shelley
import Cardano.Ledger.Shelley.Tx ()
import Cardano.Ledger.TxIn qualified as Ledger

import Cardano.Ledger.Babbage.TxInfo qualified as Babbage
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Slotting.EpochInfo (EpochInfo, hoistEpochInfo)
import Control.Monad.Trans.Except
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras qualified as Consensus
import Ouroboros.Consensus.HardFork.History qualified as Consensus

import Plutus.V1.Ledger.Api qualified as V1
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.Prelude as PPrelude hiding (Eq, Semigroup (..), unless, (.))

import Data.Maybe (catMaybes)
import Data.Text (Text)
import PlutusExample.PlutusVersion1.RedeemerContextScripts
import PlutusExample.PlutusVersion2.RedeemerContextEquivalence

data AnyCustomRedeemer
  = AnyPV1CustomRedeemer PV1CustomRedeemer
  | AnyPV2CustomRedeemer PV2CustomRedeemer
  deriving (Show, Eq)

-- We convert our custom redeemer to ScriptData so we can include it
-- in our transaction.
customRedeemerToScriptData :: AnyCustomRedeemer -> ScriptData
customRedeemerToScriptData (AnyPV1CustomRedeemer cRedeem) =
  fromPlutusData $ V1.toData cRedeem
customRedeemerToScriptData (AnyPV2CustomRedeemer cRedeem) =
  fromPlutusData $ V2.toData cRedeem


data ScriptContextError = NoScriptsInByronEra
                        | NoScriptsInEra
                        | ReadTxBodyError (FileError TextEnvelopeCddlError)
                        | IntervalConvError Text
                        | AcquireFail AcquiringFailure
                        | NoTipLocalStateError
                        | NoSystemStartTimeError
                        | EnvVarSocketErr EnvSocketError
                        | ScriptContextErrorByronEra
                        | QueryError ShelleyQueryCmdError
                        | ConsensusModeMismatch AnyConsensusMode AnyCardanoEra
                        | EraMismatch !Consensus.EraMismatch
                        | PlutusV2TranslationError (Alonzo.TranslationError StandardCrypto)
                        | MoreThanOneTxInput
                        deriving Show

createAnyCustomRedeemer
  :: forall era lang. PlutusScriptVersion lang
  -> ShelleyBasedEra era
  -> ProtocolParameters
  -> UTxO era
  -> EpochInfo (Either Text)
  -> SystemStart
  -> Api.Tx era
  -> Either ScriptContextError AnyCustomRedeemer
createAnyCustomRedeemer _ _ _ _ _ _ (ByronTx _) = Left NoScriptsInByronEra
createAnyCustomRedeemer _ sbe pparams utxo eInfo sStart (ShelleyTx ShelleyBasedEraAlonzo ledgerTx) = do
  let txBody = Alonzo.body ledgerTx
      witness = Alonzo.wits ledgerTx
      Alonzo.TxWitness _ _ _ _ _rdmrs = witness
      _redeemerPtrs = Map.toList $ Alonzo.unRedeemers _rdmrs
      ledgerUTxO = toLedgerUTxO ShelleyBasedEraAlonzo utxo
      scriptsNeeded = Alonzo.scriptsNeeded ledgerUTxO ledgerTx
      sPurpose = case scriptsNeeded of
                   [(p ,_)] -> Alonzo.transScriptPurpose p
                   needed   -> Prelude.error $ "More than one redeemer ptr: " <> show needed
      eTxIns = Prelude.map (getTxInInfoFromTxIn ledgerUTxO) . Set.toList $ Alonzo.inputs txBody
      eTouts = Prelude.map Alonzo.txInfoOut $ seqToList $ Alonzo.outputs txBody
      minted = Alonzo.transValue $ Alonzo.mint txBody
      txfee = Alonzo.transValue . toMaryValue . lovelaceToValue . fromShelleyLovelace $ Alonzo.txfee txBody
      Alonzo.TxDats datumHashMap = Alonzo.txdats witness
      datumHashes = Prelude.map Alonzo.transDataPair $ Map.toList datumHashMap
      txcerts = Prelude.map Alonzo.transDCert . seqToList $ Alonzo.txcerts txBody
      txsignatories = Prelude.map Alonzo.transKeyHash . Set.toList $ Alonzo.reqSignerHashes txBody
  valRange <-
    first IntervalConvError
      $ Alonzo.transVITime (toLedgerPParams sbe pparams) eInfo sStart $ Alonzo.txvldt txBody

  tOuts <- if Prelude.all isJust eTouts
           then return $ catMaybes eTouts
           else Prelude.error "Tx Outs not all Just"
  txins <- if Prelude.all isJust eTxIns
           then return $ catMaybes eTxIns
           else Prelude.error "Tx Ins not all Just"
  Right . AnyPV1CustomRedeemer $ PV1CustomRedeemer tOuts txins minted valRange txfee datumHashes txcerts txsignatories (Just sPurpose)

createAnyCustomRedeemer pScriptVer sbe pparams utxo eInfo sStart (ShelleyTx ShelleyBasedEraBabbage ledgerTx) = do
  let txBody = getField @"body" ledgerTx
      mint = getField @"mint" txBody
      txins = Set.toList $ getField @"inputs" txBody
      refTxins = Set.toList $ getField @"referenceInputs" txBody
      outputs = seqToList $ getField @"outputs" txBody
      fee = Ledger.unCoin $ getField @"txfee" txBody
      certs = seqToList $ getField @"certs" txBody
      vldt = getField @"vldt" txBody
      reqSigners = Set.toList $ getField @"reqSignerHashes" txBody
      Alonzo.TxDats datumHashMap = getField @"txdats" $ getField @"wits" ledgerTx
      wdrwls = getField @"wdrls" txBody
      txwit = getField @"wits" ledgerTx
      Alonzo.Redeemers rdmrs = getField @"txrdmrs" txwit
      rdmrList = Map.toList rdmrs
      bUtxo = toLedgerUTxO ShelleyBasedEraBabbage utxo
      scriptsNeeded = Alonzo.scriptsNeeded bUtxo ledgerTx
      sPurpose = case scriptsNeeded of
                   [(p ,_)] -> Alonzo.transScriptPurpose p
                   needed   -> Prelude.error $ "More than one redeemer ptr: " <> show needed


      -- Plutus script context types
  case pScriptVer of
    PlutusScriptV1 -> Prelude.error "createAnyCustomRedeemer: PlutusScriptV1 custom redeemer not wired up yet"
    PlutusScriptV2 -> do
      bV2Ins <- first PlutusV2TranslationError $ Prelude.mapM (Babbage.txInfoInV2 bUtxo) txins
      bV2RefIns <- first PlutusV2TranslationError $ Prelude.mapM (Babbage.txInfoInV2 bUtxo) refTxins
      bV2Outputs <- first PlutusV2TranslationError $ zipWithM Babbage.txInfoOutV2 (Prelude.map Alonzo.TxOutFromInput txins) outputs

      let _bV2fee = V2.singleton V2.adaSymbol V2.adaToken fee -- Impossible to test
          _withdrawals = PMap.fromList . Map.toList $ Alonzo.transWdrl wdrwls -- untested
          bvtMint =  Alonzo.transValue mint
      valRange <-
        first IntervalConvError
          $ Alonzo.transVITime (toLedgerPParams sbe pparams) eInfo sStart vldt
      redeemrs <- first PlutusV2TranslationError $ Prelude.mapM (Babbage.transRedeemerPtr txBody) rdmrList
      Right . AnyPV2CustomRedeemer
        $ PV2CustomRedeemer
            { pv2Inputs = bV2Ins
            , pv2RefInputs = bV2RefIns
            , pv2Outputs = bV2Outputs
            , pv2Fee = PPrelude.mempty -- Impossible to test
            , pv2Mint = bvtMint
            , pv2DCert = Prelude.map Alonzo.transDCert certs
            , pv2Wdrl = PMap.empty -- TODO: Not tested
            , pv2ValidRange = valRange -- TODO: Fails when using (/=)
            , pv2Signatories = Prelude.map Alonzo.transKeyHash reqSigners
            , pv2Redeemers = PMap.fromList redeemrs
            , pv2Data = PMap.fromList . Prelude.map Alonzo.transDataPair $ Map.toList datumHashMap
            , pv2ScriptPurpose = Just sPurpose
            }

createAnyCustomRedeemer _ _ _ _ _ _ _ = Left NoScriptsInByronEra

seqToList :: Seq.StrictSeq a -> [a]
seqToList (x Seq.:<| rest) = x : seqToList rest
seqToList Seq.Empty        = []

newtype CddlTx = CddlTx { unCddlTx :: InAnyCardanoEra Tx }
    deriving (Show, Eq)

createAnyCustomRedeemerFromTxFp
  :: PlutusScriptVersion lang
  -> FilePath
  -> AnyConsensusModeParams
  -> NetworkId
  -> ExceptT ScriptContextError IO AnyCustomRedeemer
createAnyCustomRedeemerFromTxFp pScriptVer fp (AnyConsensusModeParams cModeParams) network = do
  -- TODO: Expose readFileTx from cardano-cli
  CddlTx (InAnyCardanoEra cEra alonzoTx)
    <- firstExceptT ReadTxBodyError
         . newExceptT
         $ readFileTextEnvelopeCddlAnyOf
             [ FromCDDLTx "Witnessed Tx AlonzoEra" CddlTx
             , FromCDDLTx "Unwitnessed Tx AlonzoEra" CddlTx
             , FromCDDLTx "Witnessed Tx BabbageEra" CddlTx
             , FromCDDLTx "Unwitnessed Tx BabbageEra" CddlTx
             ]
             fp

  sbe <- getSbe $ cardanoEraStyle cEra
  SocketPath sockPath <- firstExceptT EnvVarSocketErr . newExceptT $ readEnvSocketPath
  case consensusModeOnly cModeParams of
    CardanoMode -> do
      let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath
      eInMode <- hoistMaybe
                   (ConsensusModeMismatch (AnyConsensusMode CardanoMode) (AnyCardanoEra cEra))
                   $ toEraInMode cEra CardanoMode

      eResult <-
        liftIO $ executeLocalStateQueryExpr localNodeConnInfo Nothing
          $ \ntcVersion -> do
              (EraHistory _ interpreter) <- queryExpr $ QueryEraHistory CardanoModeIsMultiEra
              mSystemStart <-
                if ntcVersion Prelude.>= NodeToClientV_9
                then Just Prelude.<$> queryExpr QuerySystemStart
                else return Nothing
              let eInfo = hoistEpochInfo (first (T.pack . displayError . TransactionValidityIntervalError) . runExcept)
                            $ Consensus.interpreterToEpochInfo interpreter
              ppResult <- queryExpr $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters
              return (eInfo, mSystemStart, ppResult)

      (eInfo, mSystemStart, ePParams) <- firstExceptT AcquireFail $ hoistEither eResult
      pparams <- firstExceptT EraMismatch $ hoistEither ePParams
      sStart <- hoistMaybe NoSystemStartTimeError mSystemStart

      -- Query UTxO
      let utxoQ = QueryInShelleyBasedEra sbe (QueryUTxO QueryUTxOWhole)
          utxoQinMode = case toEraInMode cEra CardanoMode of
                          Just eInMode' -> QueryInEra eInMode' utxoQ
                          Nothing       -> Prelude.error "Cannot determine era in mode"
      utxo <- firstExceptT QueryError
                $ executeQuery
                    cEra
                    cModeParams
                    localNodeConnInfo
                    utxoQinMode
      hoistEither $ createAnyCustomRedeemer pScriptVer sbe pparams
                                       utxo eInfo sStart alonzoTx
    _ -> Prelude.error "Please specify --cardano-mode on cli."


createAnyCustomRedeemerBsFromTxFp
  :: PlutusScriptVersion lang
  -> FilePath
  -> AnyConsensusModeParams
  -> NetworkId
  -> ExceptT ScriptContextError IO LB.ByteString
createAnyCustomRedeemerBsFromTxFp pScriptVer txFp anyCmodeParams nid = do
  anyCustomRedeemer <- createAnyCustomRedeemerFromTxFp pScriptVer txFp anyCmodeParams nid
  return . Aeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema
         $ customRedeemerToScriptData anyCustomRedeemer


getSbe :: CardanoEraStyle era -> ExceptT ScriptContextError IO (ShelleyBasedEra era)
getSbe LegacyByronEra        = left ScriptContextErrorByronEra
getSbe (ShelleyBasedEra sbe) = return sbe


-- Used in roundtrip testing

fromPlutusTxId :: V1.TxId -> Ledger.TxId StandardCrypto
fromPlutusTxId (V1.TxId builtInBs) =
  case deserialiseFromRawBytes AsTxId $ fromBuiltin builtInBs of
    Just txidHash -> toShelleyTxId txidHash
    Nothing       -> Prelude.error "Could not derserialize txid"


sampleTestV1ScriptContextDataJSON :: LB.ByteString
sampleTestV1ScriptContextDataJSON =
  Aeson.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . customRedeemerToScriptData
    . AnyPV1CustomRedeemer
    $ PV1CustomRedeemer
        dummyTxOuts
        dummyTxIns
        dummyLedgerVal
        dummyPOSIXTimeRange
        dummyLedgerVal
        dummyDatumHashes
        dummyCerts
        dummySignatories
        dummyScriptPurpose


dummyCerts :: [V1.DCert]
dummyCerts = []

dummyTxIns :: [V1.TxInInfo]
dummyTxIns = []

dummySignatories :: [V1.PubKeyHash]
dummySignatories = []

dummyDatumHashes :: [(V1.DatumHash, V1.Datum)]
dummyDatumHashes = []

dummyLedgerVal :: V1.Value
dummyLedgerVal = Alonzo.transValue $ toMaryValue Prelude.mempty

dummyTxOuts :: [V1.TxOut]
dummyTxOuts = []

dummyPOSIXTimeRange :: V1.POSIXTimeRange
dummyPOSIXTimeRange = V1.from $ V1.POSIXTime 42

dummyScriptPurpose :: Maybe V1.ScriptPurpose
dummyScriptPurpose = Nothing

getTxInInfoFromTxIn
  :: Shelley.UTxO (Alonzo.AlonzoEra StandardCrypto)
  -> Ledger.TxIn StandardCrypto
  -> Maybe V1.TxInInfo
getTxInInfoFromTxIn (Shelley.UTxO utxoMap) txIn = do
  txOut <- Map.lookup txIn utxoMap
  Alonzo.txInfoIn txIn txOut

sampleTestV2ScriptContextDataJSON :: LB.ByteString
sampleTestV2ScriptContextDataJSON =
  Aeson.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . customRedeemerToScriptData
    . AnyPV2CustomRedeemer
    $ PV2CustomRedeemer
       { pv2Inputs = []
       , pv2RefInputs = []
       , pv2Outputs = []
       , pv2Fee = PPrelude.mempty
       , pv2Mint = PPrelude.mempty
       , pv2DCert = []
       , pv2Wdrl = PMap.empty
       , pv2ValidRange = V2.always
       , pv2Signatories = []
       , pv2Redeemers = PMap.empty
       , pv2Data = PMap.empty
       , pv2ScriptPurpose = Nothing
       }
