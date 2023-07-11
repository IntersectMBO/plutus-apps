{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}

-- | Transaction validation using 'cardano-ledger-specs'
module Cardano.Node.Emulator.Internal.Node.Validation (
  EmulatorBlock,
  EmulatedLedgerState(..),
  Coin(..),
  SlotNo(..),
  EmulatorEra,
  CardanoLedgerError,
  initialState,
  hasValidationErrors,
  makeTransactionBody,
  validateCardanoTx,
  unsafeMakeValid,
  -- * Modifying the state
  makeBlock,
  setSlot,
  nextSlot,
  UTxO(..),
  setUtxo,
  -- * Lenses
  ledgerEnv,
  memPoolState,
  currentBlock,
  previousBlocks,
  -- * Etc.
  emulatorGlobals,
  ) where

import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.PlutusScriptApi (collectTwoPhaseScriptInputs, evalScripts)
import Cardano.Ledger.Alonzo.Rules.Utxos (UtxosPredicateFailure (CollectErrors))
import Cardano.Ledger.Alonzo.Scripts (CostModels, Script, unCostModels)
import Cardano.Ledger.Alonzo.Tools qualified as C.Ledger
import Cardano.Ledger.Alonzo.Tx (IsValid (IsValid), ValidatedTx (ValidatedTx))
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO, ScriptResult (Fails, Passes))
import Cardano.Ledger.Alonzo.TxWitness qualified as Alonzo
import Cardano.Ledger.Babbage.PParams (PParams' (_costmdls, _maxTxExUnits, _protocolVersion))
import Cardano.Ledger.BaseTypes (Globals (systemStart), ProtVer, epochInfo)
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Era (Crypto, ValidateScript)
import Cardano.Ledger.Shelley.API (Coin (Coin), LedgerEnv (LedgerEnv, ledgerSlotNo),
                                   LedgerState (lsDPState, lsUTxOState), MempoolEnv, MempoolState, TxIn, UTxO (UTxO),
                                   Validated, unsafeMakeValidated)
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Cardano.Ledger.Shelley.LedgerState (LedgerState (LedgerState), UTxOState (_utxo), smartUTxOState)
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (UtxoEnv))
import Cardano.Ledger.Shelley.TxBody (DCert, Wdrl)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval)
import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Control.Lens (makeLenses, over, (&), (.~), (^.))
import Control.Monad.Except (MonadError (throwError))
import Data.Array (array)
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Default (def)
import Data.Map qualified as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Text qualified as Text
import GHC.Records (HasField (getField))
import Ledger.Index (genesisTxIn, getCollateral)
import Ledger.Index.Internal qualified as P
import Ledger.Slot (Slot)
import Ledger.Tx (CardanoTx (CardanoEmulatorEraTx))
import Ledger.Tx.CardanoAPI qualified as P
import Plutus.V1.Ledger.Api qualified as V1 hiding (TxOut (..))
import Plutus.V1.Ledger.Scripts qualified as P

import Cardano.Node.Emulator.Internal.Node.Params (EmulatorEra, Params (emulatorPParams), emulatorGlobals,
                                                   emulatorPParams)
import Ledger.Blockchain (OnChainTx (OnChainTx))

type CardanoLedgerError = Either P.ValidationErrorInPhase P.ToCardanoError

type EmulatorBlock = [Validated (Core.Tx EmulatorEra)]

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

* We cannot represent different eras - everything is 'BabbageEra'.
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
    { _ledgerEnv      :: !(MempoolEnv EmulatorEra)
    , _memPoolState   :: !(MempoolState EmulatorEra)
    , _currentBlock   :: !EmulatorBlock
    , _previousBlocks :: ![EmulatorBlock]
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
setUtxo utxo els@EmulatedLedgerState{_memPoolState} = els { _memPoolState = newPoolState }
  where
    newPoolState = _memPoolState { lsUTxOState = smartUTxOState utxo (Coin 0) (Coin 0) def }

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
initialState :: Params -> EmulatedLedgerState
initialState params = EmulatedLedgerState
  { _ledgerEnv = C.Ledger.LedgerEnv
      { C.Ledger.ledgerSlotNo = 0
      , C.Ledger.ledgerIx = minBound
      , C.Ledger.ledgerPp = emulatorPParams params
      , C.Ledger.ledgerAccount = C.Ledger.AccountState (Coin 0) (Coin 0)
      }
  , _memPoolState = LedgerState
    { lsUTxOState = smartUTxOState (UTxO mempty) (Coin 0) (Coin 0) def
    , lsDPState = C.Ledger.DPState def def
    }
  , _currentBlock = []
  , _previousBlocks = []
  }


utxoEnv :: Params -> SlotNo -> C.Ledger.UtxoEnv EmulatorEra
utxoEnv params slotNo = C.Ledger.UtxoEnv slotNo (emulatorPParams params) mempty (C.Ledger.GenDelegs mempty)

applyTx ::
  Params ->
  EmulatedLedgerState ->
  Core.Tx EmulatorEra ->
  Either P.ValidationError (EmulatedLedgerState, Validated (Core.Tx EmulatorEra))
applyTx params oldState@EmulatedLedgerState{_ledgerEnv, _memPoolState} tx = do
  (newMempool, vtx) <- first (P.CardanoLedgerValidationError . Text.pack . show) (C.Ledger.applyTx (emulatorGlobals params) _ledgerEnv _memPoolState tx)
  return (oldState & memPoolState .~ newMempool & over currentBlock ((:) vtx), vtx)


hasValidationErrors :: Params -> SlotNo -> P.UtxoIndex -> C.Tx C.BabbageEra -> P.ValidationResult
hasValidationErrors params slotNo utxoIndex tx'@(C.ShelleyTx _ tx) =
  case res of
    Left err -> P.FailPhase1 (CardanoEmulatorEraTx tx') err
    Right (_, vtx) -> case getTxExUnitsWithLogs params utxo tx' of
      Left (P.Phase1, err) -> P.FailPhase1 (CardanoEmulatorEraTx tx') err
      Left (P.Phase2, err) -> P.FailPhase2 vtx err $ getCollateral utxoIndex (CardanoEmulatorEraTx tx')
      Right report         -> P.Success vtx report
  where
    utxo = P.fromPlutusIndex utxoIndex
    state = setSlot slotNo $ setUtxo utxo $ initialState params
    res = do
      vtx <- first (P.CardanoLedgerValidationError . Text.pack . show) (constructValidated (emulatorGlobals params) (utxoEnv params slotNo) (lsUTxOState (_memPoolState state)) tx)
      fmap OnChainTx <$> applyTx params state vtx

-- | Construct a 'ValidatedTx' from a 'Core.Tx' by setting the `IsValid`
-- flag.
--
-- Note that this simply constructs the transaction; it does not validate
-- anything other than the scripts. Thus the resulting transaction may be
-- completely invalid.
--
-- Copied from cardano-ledger as it was removed there
-- in https://github.com/input-output-hk/cardano-ledger/commit/721adb55b39885847562437a6fe7e998f8e48c03
constructValidated ::
  forall era m.
  ( MonadError [UtxosPredicateFailure era] m,
    Core.Script era ~ Script era,
    Core.Witnesses era ~ Alonzo.TxWitness era,
    ValidateScript era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "_costmdls" (Core.PParams era) CostModels,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "vldt" (Core.TxBody era) ValidityInterval,
    ExtendedUTxO era
  ) =>
  Globals ->
  UtxoEnv era ->
  UTxOState era ->
  Core.Tx era ->
  m (ValidatedTx era)
constructValidated globals (UtxoEnv _ pp _ _) st tx =
  case collectTwoPhaseScriptInputs ei sysS pp tx utxo of
    Left errs -> throwError [CollectErrors errs]
    Right sLst ->
      let scriptEvalResult = evalScripts @era (getField @"_protocolVersion" pp) tx sLst
          vTx =
            ValidatedTx
              (getField @"body" tx)
              (getField @"wits" tx)
              (IsValid (lift scriptEvalResult))
              (getField @"auxiliaryData" tx)
       in pure vTx
  where
    utxo = _utxo st
    sysS = systemStart globals
    ei = epochInfo globals
    lift (Passes _)  = True
    lift (Fails _ _) = False

unsafeMakeValid :: CardanoTx -> OnChainTx
unsafeMakeValid (CardanoEmulatorEraTx (C.Tx txBody _)) =
  let C.ShelleyTxBody _ txBody' _ _ _ _ = txBody
      vtx :: Core.Tx EmulatorEra = ValidatedTx txBody' mempty (IsValid True) C.Ledger.SNothing
  in OnChainTx $ unsafeMakeValidated vtx

validateCardanoTx
  :: Params
  -> Slot
  -> P.UtxoIndex
  -> CardanoTx
  -> P.ValidationResult
validateCardanoTx params slot utxo ctx@(CardanoEmulatorEraTx tx@(C.Tx (C.TxBody bodyContent) _)) =
  if map fst (C.txIns bodyContent) == [genesisTxIn]
    then P.Success (unsafeMakeValid ctx) Map.empty
    else hasValidationErrors params (fromIntegral slot) utxo tx

getTxExUnitsWithLogs :: Params -> UTxO EmulatorEra -> C.Tx C.BabbageEra -> Either P.ValidationErrorInPhase P.RedeemerReport
getTxExUnitsWithLogs params utxo (C.ShelleyTx _ tx) =
  case C.Ledger.evaluateTransactionExecutionUnitsWithLogs (emulatorPParams params) tx utxo ei ss costmdls of
    Left e       -> Left . (P.Phase1,) . P.CardanoLedgerValidationError . Text.pack . show $ e
    Right result -> traverse (either toCardanoLedgerError Right) result
  where
    eg = emulatorGlobals params
    ss = systemStart eg
    ei = epochInfo eg
    costmdls = array (minBound, maxBound) . Map.toList $ unCostModels $ getField @"_costmdls" $ emulatorPParams params
    toCardanoLedgerError (C.Ledger.ValidationFailedV1 (V1.CekError ce) logs) =
      Left (P.Phase2, P.ScriptFailure (P.EvaluationError logs ("CekEvaluationFailure: " ++ show ce)))
    toCardanoLedgerError (C.Ledger.ValidationFailedV2 (V1.CekError ce) logs) =
      Left (P.Phase2, P.ScriptFailure (P.EvaluationError logs ("CekEvaluationFailure: " ++ show ce)))
    toCardanoLedgerError e = Left (P.Phase2, P.CardanoLedgerValidationError $ Text.pack $ show e)

makeTransactionBody
  :: Params
  -> UTxO EmulatorEra
  -> P.CardanoBuildTx
  -> Either CardanoLedgerError (C.TxBody C.BabbageEra)
makeTransactionBody params utxo txBodyContent = do
  txTmp <- bimap Right (C.makeSignedTransaction []) $ P.makeTransactionBody (Just $ emulatorPParams params) mempty txBodyContent
  exUnits <- bimap Left (Map.map snd) $ getTxExUnitsWithLogs params utxo txTmp
  first Right $ P.makeTransactionBody (Just $ emulatorPParams params) exUnits txBodyContent
