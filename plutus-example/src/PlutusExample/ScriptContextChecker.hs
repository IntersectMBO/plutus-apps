{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module PlutusExample.ScriptContextChecker where

import Prelude hiding (($))

import Cardano.Api
import Cardano.Api.Byron
import Cardano.Api.Shelley
import Cardano.Api.Shelley qualified as Api

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except.Extra
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LB
import Data.Either as E
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set

import Cardano.CLI.Environment
import Cardano.CLI.Shelley.Run.Query
import Cardano.CLI.Types (SocketPath (..))
import Cardano.Ledger.Alonzo.PParams qualified as Alonzo
import Cardano.Ledger.Alonzo.PlutusScriptApi qualified as Alonzo
import Cardano.Ledger.Alonzo.Tx qualified as Alonzo
import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo
import Cardano.Ledger.Alonzo.TxWitness qualified as Alonzo
import Cardano.Ledger.TxIn qualified as Ledger

import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Slotting.EpochInfo (EpochInfo, hoistEpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.Monad.Trans.Except
import Ledger qualified as Plutus
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras qualified as Consensus
import Ouroboros.Consensus.HardFork.History qualified as Consensus
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Plutus.Script.Utils.V1.Scripts qualified as Scripts
import Plutus.V1.Ledger.Api qualified as Plutus
import PlutusTx qualified
import PlutusTx.IsData.Class
import PlutusTx.Prelude hiding (Eq, Semigroup (..), unless, (.))

import PlutusExample.PlutusVersion1.RedeemerContextScripts


data AnyCustomRedeemer
  = AnyPV1CustomRedeemer PV1CustomRedeemer
  deriving (Show, Eq)

-- We convert our custom redeemer to ScriptData so we can include it
-- in our transaction.
customRedeemerToScriptData :: AnyCustomRedeemer -> ScriptData
customRedeemerToScriptData (AnyPV1CustomRedeemer cRedeem) =
  fromPlutusData $ PlutusTx.builtinDataToData $ toBuiltinData cRedeem

data ScriptContextError = NoScriptsInByronEra
                        | NoScriptsInEra
                        | ReadTxBodyError (FileError TextEnvelopeError)
                        | IntervalConvError TransactionValidityError
                        | AcquireFail AcquireFailure
                        | NoTipLocalStateError
                        | NoSystemStartTimeError
                        | EnvVarSocketErr EnvSocketError
                        | ScriptContextErrorByronEra
                        | QueryError ShelleyQueryCmdError
                        | ConsensusModeMismatch AnyConsensusMode AnyCardanoEra
                        | EraMismatch !Consensus.EraMismatch
                        deriving Show

createAnyCustomRedeemer
  :: ShelleyBasedEra era
  -> ProtocolParameters
  -> UTxO era
  -> EpochInfo (Either TransactionValidityError)
  -> SystemStart
  -> Api.Tx era
  -> Either ScriptContextError AnyCustomRedeemer
createAnyCustomRedeemer _ _ _ _ _ (ByronTx _) = Left NoScriptsInByronEra
createAnyCustomRedeemer sbe pparams utxo eInfo sStart (ShelleyTx ShelleyBasedEraAlonzo ledgerTx) = do
  let txBody = Alonzo.body ledgerTx
      witness = Alonzo.wits ledgerTx
      Alonzo.TxWitness _ _ _ _ _rdmrs = witness
      _redeemerPtrs = Map.toList $ Alonzo.unRedeemers _rdmrs
      ledgerUTxO = toLedgerUTxO ShelleyBasedEraAlonzo utxo
      scriptsNeeded = Alonzo.scriptsNeeded ledgerUTxO ledgerTx
      sPurpose = case scriptsNeeded of
                   [(p ,_)] -> Alonzo.transScriptPurpose p
                   needed   -> Prelude.error $ "More than one redeemer ptr: " <> show needed
      eTxIns = Prelude.map (Alonzo.txInfoIn ledgerUTxO) . Set.toList $ Alonzo.inputs txBody
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

  tOuts <- if Prelude.all E.isRight eTouts
           then return $ E.rights eTouts
           else Prelude.error "Tx Outs not all Right"
  txins <- if Prelude.all E.isRight eTxIns
           then return $ E.rights eTxIns
           else Prelude.error "Tx Ins not all Right"
  Right . AnyPV1CustomRedeemer $ PV1CustomRedeemer tOuts txins minted valRange txfee datumHashes txcerts txsignatories (Just sPurpose)
 where
  seqToList (x Seq.:<| rest) = x : seqToList rest
  seqToList Seq.Empty        = []

createAnyCustomRedeemer _ _ _ _ _ (ShelleyTx _ _) = Left NoScriptsInEra

createAnyCustomRedeemerFromTxFp
  :: FilePath
  -> AnyConsensusModeParams
  -> NetworkId
  -> ExceptT ScriptContextError IO AnyCustomRedeemer
createAnyCustomRedeemerFromTxFp fp (AnyConsensusModeParams cModeParams) network = do
  InAnyCardanoEra cEra alonzoTx
    <- firstExceptT ReadTxBodyError
         . newExceptT
         $ readFileTextEnvelopeAnyOf
             [ FromSomeType (AsTx AsAlonzoEra) (InAnyCardanoEra AlonzoEra)
             -- TODO: Babbage Era -- Update with Babbage
             ]
             fp

  sbe <- getSbe $ cardanoEraStyle cEra
  SocketPath sockPath <- firstExceptT EnvVarSocketErr readEnvSocketPath
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
              let eInfo = hoistEpochInfo (first TransactionValidityIntervalError . runExcept)
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
      hoistEither $ createAnyCustomRedeemer sbe pparams
                                       utxo eInfo sStart alonzoTx
    _ -> Prelude.error "Please specify --cardano-mode on cli."

createAnyCustomRedeemerBsFromTxFp
  :: FilePath
  -> AnyConsensusModeParams
  -> NetworkId
  -> ExceptT ScriptContextError IO LB.ByteString
createAnyCustomRedeemerBsFromTxFp txFp anyCmodeParams nid = do
  AnyPV1CustomRedeemer testScrContext <- createAnyCustomRedeemerFromTxFp txFp anyCmodeParams nid

  return . Aeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema
                        $ testScriptContextToScriptData testScrContext

getSbe :: CardanoEraStyle era -> ExceptT ScriptContextError IO (ShelleyBasedEra era)
getSbe LegacyByronEra        = left ScriptContextErrorByronEra
getSbe (ShelleyBasedEra sbe) = return sbe


-- Used in roundtrip testing

fromPlutusTxId :: Plutus.TxId -> Ledger.TxId StandardCrypto
fromPlutusTxId (Plutus.TxId builtInBs) =
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


dummyCerts :: [Plutus.DCert]
dummyCerts = []

dummyTxIns :: [Plutus.TxInInfo]
dummyTxIns = []

dummySignatories :: [Plutus.PubKeyHash]
dummySignatories = []

dummyDatumHashes :: [(Plutus.DatumHash, Plutus.Datum)]
dummyDatumHashes = []

dummyLedgerVal :: Plutus.Value
dummyLedgerVal = Alonzo.transValue $ toMaryValue Prelude.mempty

dummyTxOuts :: [Plutus.TxOut]
dummyTxOuts = []

dummyPOSIXTimeRange :: Plutus.POSIXTimeRange
dummyPOSIXTimeRange = Plutus.from $ Plutus.POSIXTime 42

dummyScriptPurpose :: Maybe Plutus.ScriptPurpose
dummyScriptPurpose = Nothing

