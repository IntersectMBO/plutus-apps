{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-| Transaction validation using 'cardano-ledger-specs'
-}
module Ledger.Validation(
  EmulatorBlock,
  EmulatedLedgerState(..),
  Coin(..),
  SlotNo(..),
  EmulatorEra,
  CardanoLedgerError,
  initialState,
  evaluateTransactionFee,
  evaluateMinLovelaceOutput,
  getRequiredSigners,
  addSignature,
  hasValidationErrors,
  -- * Modifying the state
  makeBlock,
  setSlot,
  nextSlot,
  UTxO,
  setUtxo,
  -- * Conversion from Plutus types
  fromPlutusTx,
  fromPlutusIndex,
  fromPlutusTxOut,
  fromPlutusTxOutUnsafe,
  fromPlutusTxOutRef,
  -- * Lenses
  ledgerEnv,
  memPoolState,
  currentBlock,
  previousBlocks,
  -- * Etc.
  emulatorGlobals
  ) where

import Cardano.Api.Shelley (ShelleyBasedEra (ShelleyBasedEraAlonzo), makeSignedTransaction, shelleyGenesisDefaults,
                            toShelleyTxId, toShelleyTxOut)
import Cardano.Api.Shelley qualified as C.Api
import Cardano.Ledger.Alonzo (TxBody, TxOut)
import Cardano.Ledger.Alonzo.PParams (PParams' (..), retractPP)
import Cardano.Ledger.Alonzo.Rules.Utxos (constructValidated)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (ExUnits))
import Cardano.Ledger.Alonzo.Tools qualified as C.Ledger
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody (TxBody, reqSignerHashes))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr, txwitsVKey)
import Cardano.Ledger.BaseTypes (Globals (..))
import Cardano.Ledger.Core (PParams, Tx)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API (Coin (..), LedgerEnv (..), MempoolEnv, MempoolState, TxId, TxIn (TxIn), UTxO (UTxO),
                                   Validated, epochInfo, mkShelleyGlobals)
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Cardano.Ledger.Shelley.LedgerState (smartUTxOState)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (mkSlotLength)
import Control.Lens (_1, makeLenses, over, (&), (.~), (^.))
import Data.Array (array)
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bitraverse)
import Data.Default (def)
import Data.Functor.Identity (runIdentity)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Records (HasField (..))
import Ledger.Address qualified as P
import Ledger.Crypto qualified as P
import Ledger.Index.Internal (EmulatorEra)
import Ledger.Index.Internal qualified as P
import Ledger.Params qualified as P
import Ledger.TimeSlot qualified as P
import Ledger.Tx qualified as P
import Ledger.Tx.CardanoAPI qualified as P
import Ledger.Value qualified as P
import Plutus.V1.Ledger.Ada qualified as P
import Plutus.V1.Ledger.Api qualified as P
import Plutus.V1.Ledger.Scripts qualified as P
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.ErrorCodes (checkHasFailedError)

type CardanoLedgerError = Either P.ValidationErrorInPhase P.ToCardanoError

type EmulatorBlock = [Validated (Tx EmulatorEra)]

{- Note [Emulated ledger]

In the real cardano node, there two types of validation: Transaction validation
(performed when a transaction is first added to the mempool) and block
validation (performed when a block is created by the local node or received
from a peer).

Transaction validation runs the Plutus scripts, checks cryptographic
signatures, balances, existence of transaction inputs and so forth. This is
where the ledger state is updated. Block validation performs other checks
related to the consensus algorithm.

Networking and consensus issues are not part of the emulator's scope. We only
care about transaction validation here, so we don't have to worry about block
validation.

The decision to leave out block validation and consensus-related concerns has
the following implications:

1. We can represent blocks as simple lists-of-transactions
2. We can modify time (the slot number) and ledger parameters as we wish,
   without having to post transactions that modify them.

There are also some limitations of the emulator's functionality that could be
addressed by extending the emulator, without having to bring in the full block
validating machinery.

* We cannot represent different eras - everything is 'AlonzoEra'.
* There is no handling of epoch boundaries, rewards, etc.
* The block size is unlimited - we simply take all transactions from the
  mempool when we make a block. There is however a limit on the size of
  individual transactions.
* We use the standard ledger cryptography everywhere ('StandardCrypto').
  This could be replaced by "NoCrypto" for faster validation.

-}

{-| State of the ledger with configuration, mempool, and the blockchain.
-}
data EmulatedLedgerState =
  EmulatedLedgerState
    { _ledgerEnv      :: MempoolEnv EmulatorEra
    , _memPoolState   :: MempoolState EmulatorEra
    , _currentBlock   :: EmulatorBlock
    , _previousBlocks :: [EmulatorBlock]
    }
    deriving Show

makeLenses ''EmulatedLedgerState

{-| Increase the slot number by one
-}
nextSlot :: EmulatedLedgerState -> EmulatedLedgerState
nextSlot = over ledgerEnv f where
  f l@LedgerEnv{ledgerSlotNo=oldSlot} = l{ledgerSlotNo = succ oldSlot}

{-| Set the slot number
-}
setSlot :: SlotNo -> EmulatedLedgerState -> EmulatedLedgerState
setSlot sl = over ledgerEnv (\l -> l{ledgerSlotNo=sl})

{-| Set the utxo
-}
setUtxo :: UTxO EmulatorEra -> EmulatedLedgerState -> EmulatedLedgerState
setUtxo utxo = memPoolState . _1 .~ smartUTxOState utxo (Coin 0) (Coin 0) def

{-| Make a block with all transactions that have been validated in the
current block, add the block to the blockchain, and empty the current block.
-}
makeBlock :: EmulatedLedgerState -> EmulatedLedgerState
makeBlock state =
  state
    & currentBlock .~ []
    & over previousBlocks ((:) (reverse $ state ^. currentBlock))

{-| Initial ledger state for a distribution
-}
initialState :: P.Params -> EmulatedLedgerState
initialState params = EmulatedLedgerState
  { _ledgerEnv = C.Ledger.LedgerEnv
      { C.Ledger.ledgerSlotNo = 0
      , C.Ledger.ledgerIx = 0
      , C.Ledger.ledgerPp = C.Api.toLedgerPParams ShelleyBasedEraAlonzo $ P.pProtocolParams params
      , C.Ledger.ledgerAccount = C.Ledger.AccountState (Coin 0) (Coin 0)
      }
  , _memPoolState = (smartUTxOState (UTxO mempty) (Coin 0) (Coin 0) def, C.Ledger.DPState def def)
  , _currentBlock = []
  , _previousBlocks = []
  }


utxoEnv :: P.Params -> SlotNo -> C.Ledger.UtxoEnv EmulatorEra
utxoEnv params slotNo = C.Ledger.UtxoEnv slotNo (emulatorPParams params) mempty (C.Ledger.GenDelegs mempty)

applyTx ::
  P.Params ->
  EmulatedLedgerState ->
  Tx EmulatorEra ->
  Either P.ValidationError (EmulatedLedgerState, Validated (Tx EmulatorEra))
applyTx params oldState@EmulatedLedgerState{_ledgerEnv, _memPoolState} tx = do
  (newMempool, vtx) <- first (P.CardanoLedgerValidationError . show) (C.Ledger.applyTx (emulatorGlobals params) _ledgerEnv _memPoolState tx)
  return (oldState & memPoolState .~ newMempool & over currentBlock ((:) vtx), vtx)


genesisDefaultsFromParams :: P.Params -> C.Ledger.ShelleyGenesis EmulatorEra
genesisDefaultsFromParams P.Params { P.pSlotConfig, P.pProtocolParams, P.pNetworkId } = shelleyGenesisDefaults
  { C.Ledger.sgSystemStart = P.posixTimeToUTCTime $ P.scSlotZeroTime pSlotConfig
  , C.Ledger.sgNetworkMagic = case pNetworkId of C.Api.Testnet (C.Api.NetworkMagic nm) -> nm; _ -> 0
  , C.Ledger.sgNetworkId = case pNetworkId of C.Api.Testnet _ -> C.Ledger.Testnet; C.Api.Mainnet -> C.Ledger.Mainnet
  , C.Ledger.sgProtocolParams = retractPP (Coin 0) $ C.Api.toLedgerPParams ShelleyBasedEraAlonzo pProtocolParams
  }

{-| A sensible default 'Globals' value for the emulator
-}
emulatorGlobals :: P.Params -> Globals
emulatorGlobals params@P.Params { P.pSlotConfig, P.pProtocolParams } = mkShelleyGlobals
  (genesisDefaultsFromParams params)
  (fixedEpochInfo (EpochSize 432000) (mkSlotLength $ P.posixTimeToNominalDiffTime $ P.POSIXTime $ P.scSlotLength pSlotConfig))
  (fst $ C.Api.protocolParamProtocolVersion pProtocolParams)

emulatorPParams :: P.Params -> PParams EmulatorEra
emulatorPParams P.Params { P.pProtocolParams } = C.Api.toLedgerPParams ShelleyBasedEraAlonzo pProtocolParams

hasValidationErrors :: P.Params -> SlotNo -> UTxO EmulatorEra -> C.Api.Tx C.Api.AlonzoEra -> Maybe P.ValidationErrorInPhase
hasValidationErrors params slotNo utxo (C.Api.ShelleyTx _ tx) =
  case res of
    Left e  -> Just (P.Phase1, e)
    Right _ -> Nothing
  where
    state = setSlot slotNo $ setUtxo utxo $ initialState params
    res = do
      vtx <- first (P.CardanoLedgerValidationError . show) (constructValidated (emulatorGlobals params) (utxoEnv params slotNo) (fst (_memPoolState state)) tx)
      applyTx params state vtx


getTxExUnits :: P.Params -> UTxO EmulatorEra -> C.Api.Tx C.Api.AlonzoEra -> Either CardanoLedgerError (Map.Map RdmrPtr ExUnits)
getTxExUnits params utxo (C.Api.ShelleyTx _ tx) =
  case runIdentity $ C.Ledger.evaluateTransactionExecutionUnits (emulatorPParams params) tx utxo ei ss costmdls of
    Left e      -> Left . Left . (P.Phase1,) . P.CardanoLedgerValidationError . show $ e
    Right rdmrs -> traverse (either toCardanoLedgerError Right) rdmrs
  where
    eg = emulatorGlobals params
    ss = systemStart eg
    ei = epochInfo eg
    costmdls = array (minBound, maxBound) . Map.toList $ getField @"_costmdls" $ emulatorPParams params
    -- Failing transactions throw a checkHasFailedError error, but we don't want to deal with those yet.
    -- We might be able to do that in the future.
    -- But for now just return a zero execution cost so it will run later where we do handle failing transactions.
    toCardanoLedgerError (C.Ledger.ValidationFailedV1 (P.CekError _) logs@(_:_)) | last logs == Builtins.fromBuiltin checkHasFailedError =
      Right $ ExUnits 0 0
    toCardanoLedgerError (C.Ledger.ValidationFailedV1 (P.CekError _) logs) =
      Left $ Left (P.Phase2, P.ScriptFailure (P.EvaluationError logs "CekEvaluationFailure"))
    toCardanoLedgerError e = Left $ Left (P.Phase2, P.CardanoLedgerValidationError (show e))

makeTransactionBody
  :: P.Params
  -> UTxO EmulatorEra
  -> C.Api.TxBodyContent C.Api.BuildTx C.Api.AlonzoEra
  -> Either CardanoLedgerError (C.Api.TxBody C.Api.AlonzoEra)
makeTransactionBody params utxo txBodyContent = do
  txTmp <- first Right $ makeSignedTransaction [] <$> P.makeTransactionBody mempty txBodyContent
  exUnits <- getTxExUnits params utxo txTmp
  first Right $ P.makeTransactionBody exUnits txBodyContent

evaluateTransactionFee
  :: P.Params
  -> UTxO EmulatorEra
  -> [P.PaymentPubKeyHash]
  -> P.Tx
  -> Either CardanoLedgerError P.Value
evaluateTransactionFee params utxo requiredSigners tx = do
  txBodyContent <- first Right $ plutusTxToTxBodyContent params requiredSigners tx
  let nkeys = C.Api.estimateTransactionKeyWitnessCount txBodyContent
  txBody <- makeTransactionBody params utxo txBodyContent
  case C.Api.evaluateTransactionFee (P.pProtocolParams params) txBody nkeys 0 of
    C.Api.Lovelace fee -> pure $ P.lovelaceValueOf fee

evaluateMinLovelaceOutput :: P.Params -> TxOut EmulatorEra -> P.Ada
evaluateMinLovelaceOutput params = toPlutusValue . C.Ledger.evaluateMinLovelaceOutput (emulatorPParams params)
  where
    toPlutusValue :: Coin -> P.Ada
    toPlutusValue (Coin c) = P.lovelaceOf c

fromPlutusTx
  :: P.Params
  -> UTxO EmulatorEra
  -> [P.PaymentPubKeyHash]
  -> P.Tx
  -> Either CardanoLedgerError (C.Api.Tx C.Api.AlonzoEra)
fromPlutusTx params utxo requiredSigners tx = do
  txBodyContent <- first Right $ plutusTxToTxBodyContent params requiredSigners tx
  makeSignedTransaction [] <$> makeTransactionBody params utxo txBodyContent

plutusTxToTxBodyContent
  :: P.Params
  -> [P.PaymentPubKeyHash]
  -> P.Tx
  -> Either P.ToCardanoError (C.Api.TxBodyContent C.Api.BuildTx C.Api.AlonzoEra)
plutusTxToTxBodyContent params requiredSigners =
  P.toCardanoTxBodyContent requiredSigners (Just $ P.pProtocolParams params) (P.pNetworkId params)

getRequiredSigners :: C.Api.Tx C.Api.AlonzoEra -> [P.PaymentPubKeyHash]
getRequiredSigners (C.Api.ShelleyTx _ (ValidatedTx TxBody { reqSignerHashes = rsq } _ _ _)) =
  foldMap (pure . P.PaymentPubKeyHash . P.fromCardanoPaymentKeyHash . C.Api.PaymentKeyHash . C.Ledger.coerceKeyRole) rsq

addSignature
  :: P.PrivateKey
  -> C.Api.Tx C.Api.AlonzoEra
  -> C.Api.Tx C.Api.AlonzoEra
addSignature privKey (C.Api.ShelleyTx shelleyBasedEra (ValidatedTx body wits isValid aux))
    = C.Api.ShelleyTx shelleyBasedEra (ValidatedTx body wits' isValid aux)
  where
    wits' = wits <> mempty { txwitsVKey = newWits }
    newWits = case fromPaymentPrivateKey privKey body of
      C.Api.ShelleyKeyWitness _ wit -> Set.singleton wit
      _                             -> Set.empty

fromPlutusIndex :: P.Params -> P.UtxoIndex -> Either CardanoLedgerError (UTxO EmulatorEra)
fromPlutusIndex params (P.UtxoIndex m) = first Right $
  UTxO . Map.fromList <$> traverse (bitraverse fromPlutusTxOutRef (fromPlutusTxOutUnsafe params)) (Map.toList m)

fromPlutusTxOutRef :: P.TxOutRef -> Either P.ToCardanoError (TxIn StandardCrypto)
fromPlutusTxOutRef (P.TxOutRef txId i) = TxIn <$> fromPlutusTxId txId <*> pure (fromInteger i)

fromPlutusTxId :: P.TxId -> Either P.ToCardanoError (TxId StandardCrypto)
fromPlutusTxId = fmap toShelleyTxId . P.toCardanoTxId

fromPlutusTxOut :: P.Params -> P.TxOut -> Either P.ToCardanoError (TxOut EmulatorEra)
fromPlutusTxOut params = fmap (toShelleyTxOut ShelleyBasedEraAlonzo) . P.toCardanoTxOut (P.pNetworkId params) P.toCardanoTxOutDatumHash


-- | Like 'fromPlutusTxOut', but ignores the check for zeros in txOuts.
fromPlutusTxOutUnsafe :: P.Params -> P.TxOut -> Either P.ToCardanoError (TxOut EmulatorEra)
fromPlutusTxOutUnsafe params = fmap (toShelleyTxOut ShelleyBasedEraAlonzo) . P.toCardanoTxOutUnsafe (P.pNetworkId params) P.toCardanoTxOutDatumHash

fromPaymentPrivateKey :: P.PrivateKey -> TxBody EmulatorEra -> C.Api.KeyWitness C.Api.AlonzoEra
fromPaymentPrivateKey xprv txBody
  = C.Api.makeShelleyKeyWitness
      (C.Api.ShelleyTxBody C.Api.ShelleyBasedEraAlonzo txBody notUsed notUsed notUsed notUsed)
      (C.Api.WitnessPaymentExtendedKey (C.Api.PaymentExtendedSigningKey xprv))
  where
    notUsed = undefined -- hack so we can reuse code from cardano-api
