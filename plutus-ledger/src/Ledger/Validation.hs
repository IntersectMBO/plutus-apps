{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-| Transaction validation using 'cardano-ledger-specs'
-}
module Ledger.Validation(
  EmulatorBlock,
  EmulatedLedgerState(..),
  Coin(..),
  SlotNo(..),
  EmulatorEra,
  initialState,
  evaluateTransactionFee,
  evaluateMinLovelaceOutput,
  addSignature,
  applyTx,
  hasValidationErrors,
  -- * Modifying the state
  makeBlock,
  setSlot,
  nextSlot,
  -- * Conversion from Plutus types
  UTxOState,
  fromPlutusTx,
  fromPlutusIndex,
  -- * Lenses
  ledgerEnv,
  memPoolState,
  currentBlock,
  previousBlocks,
  -- * Etc.
  emulatorGlobals
  ) where
import Cardano.Api.Shelley (ShelleyBasedEra (ShelleyBasedEraAlonzo), alonzoGenesisDefaults, makeSignedTransaction,
                            shelleyGenesisDefaults, toShelleyTxId, toShelleyTxOut)
import Cardano.Api.Shelley qualified as C.Api
import Cardano.Ledger.Alonzo (TxBody, TxOut)
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Utxos (constructValidated)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (ValidatedTx))
import Cardano.Ledger.Alonzo.TxWitness (txwitsVKey)
import Cardano.Ledger.BaseTypes (Globals (..))
import Cardano.Ledger.Core (PParams, Tx)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API (Coin (..), LedgerEnv (..), MempoolEnv, MempoolState, NewEpochState, TxId,
                                   TxIn (TxIn), UTxO (UTxO), UTxOState (..), UtxoEnv (..), Validated, esPp,
                                   mkShelleyGlobals, nesEs)
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Cardano.Ledger.Shelley.LedgerState (smartUTxOState)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (mkSlotLength)
import Control.Lens (_1, makeLenses, over, (&), (.~), (^.))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bitraverse)
import Data.Default (def)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger.Address qualified as P
import Ledger.Crypto qualified as P
import Ledger.Index (EmApplyTxFailure (..), EmulatorEra, ValidationError (..))
import Ledger.Index qualified as P
import Ledger.Tx qualified as P
import Ledger.Tx.CardanoAPI qualified as P
import Ledger.Value qualified as P
import Plutus.V1.Ledger.Ada qualified as P
import Plutus.V1.Ledger.TxId qualified as P

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
initialState :: EmulatedLedgerState
initialState = EmulatedLedgerState
  { _ledgerEnv = C.Ledger.mkMempoolEnv emulatorNes 0
  , _memPoolState = C.Ledger.mkMempoolState emulatorNes
  , _currentBlock = []
  , _previousBlocks = []
  }

-- toAddr :: (P.PubKey, Coin) -> Maybe (Addr StandardCrypto, Coin)
-- toAddr (pk, coin) =
--   case P.toCardanoAddress emulatorNetworkId (P.pubKeyHashAddress (P.paymentPubKeyHash $ P.PaymentPubKey pk) Nothing) of
--     Right (AddressInEra _ (ShelleyAddress _ paymentCredential stakeAddressReference))
--         -> Just (Addr Testnet paymentCredential stakeAddressReference, coin)
--     _ -> Nothing

utxoEnv :: SlotNo -> UtxoEnv EmulatorEra
utxoEnv slotNo = UtxoEnv slotNo emulatorPParams mempty (C.Ledger.GenDelegs mempty)

applyTx ::
  EmulatedLedgerState ->
  Tx EmulatorEra ->
  Either ValidationError (EmulatedLedgerState, Validated (Tx EmulatorEra))
applyTx oldState@EmulatedLedgerState{_ledgerEnv, _memPoolState} tx = do
  (newMempool, vtx) <- first (EmApplyTxFailure . ApplyTxFailed) (C.Ledger.applyTx emulatorGlobals _ledgerEnv _memPoolState tx)
  return (oldState & memPoolState .~ newMempool & over currentBlock ((:) vtx), vtx)


emulatorNetworkId :: C.Api.NetworkId
emulatorNetworkId = C.Api.Testnet $ C.Api.NetworkMagic 1

-- TODO: the larger maxTxSize should only be used when needed.
genesisDefaultsWithBigMaxTxSize :: C.Ledger.ShelleyGenesis EmulatorEra
genesisDefaultsWithBigMaxTxSize = shelleyGenesisDefaults {
  C.Ledger.sgProtocolParams = (C.Ledger.sgProtocolParams shelleyGenesisDefaults) {
    C.Ledger._maxTxSize = 256 * 1024
  }
}

{-| A sensible default 'Globals' value for the emulator
-}
emulatorGlobals :: Globals
emulatorGlobals = mkShelleyGlobals
  genesisDefaultsWithBigMaxTxSize
  (fixedEpochInfo (EpochSize 432000) (mkSlotLength 8)) -- ?
  2 -- maxMajorPV

emulatorNes :: NewEpochState EmulatorEra
emulatorNes = C.Ledger.initialState genesisDefaultsWithBigMaxTxSize alonzoGenesisDefaults

emulatorPParams :: PParams EmulatorEra
emulatorPParams = (esPp $ nesEs emulatorNes) { _maxTxSize = 262144 }

emulatorProtocolParameters :: C.Api.ProtocolParameters
emulatorProtocolParameters = C.Api.fromLedgerPParams ShelleyBasedEraAlonzo emulatorPParams

hasValidationErrors :: SlotNo -> UTxOState EmulatorEra -> C.Api.Tx C.Api.AlonzoEra -> Maybe ValidationError
hasValidationErrors slotNo utxoState (C.Api.ShelleyTx _ tx) =
  case res of
    Left e  -> Just e
    Right _ -> Nothing
  where
    state = setSlot slotNo (initialState & memPoolState . _1 .~ utxoState)
    res = do
      vtx <- first (EmApplyTxFailure . UtxosPredicateFailures) (constructValidated emulatorGlobals (utxoEnv slotNo) utxoState tx)
      applyTx state vtx

evaluateTransactionFee :: [P.PaymentPubKeyHash] -> P.Tx -> Either P.ToCardanoError P.Value
evaluateTransactionFee requiredSigners tx = do
  txBodyContent <- plutusTxToTxBodyContent requiredSigners tx
  let nkeys = C.Api.estimateTransactionKeyWitnessCount txBodyContent
  txBody <- P.makeTransactionBody txBodyContent
  case C.Api.evaluateTransactionFee emulatorProtocolParameters txBody nkeys 0 of
    C.Api.Lovelace fee -> pure $ P.lovelaceValueOf fee

evaluateMinLovelaceOutput :: TxOut EmulatorEra -> P.Value
evaluateMinLovelaceOutput = toPlutusValue . C.Ledger.evaluateMinLovelaceOutput emulatorPParams

toPlutusValue :: Coin -> P.Value
toPlutusValue (Coin c) = P.lovelaceValueOf c

fromPlutusTx
  :: [P.PaymentPubKeyHash]
  -> P.Tx
  -> Either P.ToCardanoError (C.Api.Tx C.Api.AlonzoEra)
fromPlutusTx requiredSigners tx = do
  txBodyContent <- plutusTxToTxBodyContent requiredSigners tx
  makeSignedTransaction [] <$> P.makeTransactionBody txBodyContent

plutusTxToTxBodyContent
  :: [P.PaymentPubKeyHash]
  -> P.Tx
  -> Either P.ToCardanoError (C.Api.TxBodyContent C.Api.BuildTx C.Api.AlonzoEra)
plutusTxToTxBodyContent requiredSigners =
  P.toCardanoTxBodyContent requiredSigners (Just emulatorProtocolParameters) emulatorNetworkId

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

fromPlutusIndex :: P.UtxoIndex -> Either P.ToCardanoError (UTxOState EmulatorEra)
fromPlutusIndex (P.UtxoIndex m) = (\utxo -> smartUTxOState utxo (Coin 0) (Coin 0) def) <$>
  (UTxO . Map.fromList <$> traverse (bitraverse fromPlutusTxOutRef fromPlutusTxOut) (Map.toList m))

fromPlutusTxOutRef :: P.TxOutRef -> Either P.ToCardanoError (TxIn StandardCrypto)
fromPlutusTxOutRef (P.TxOutRef txId i) = TxIn <$> fromPlutusTxId txId <*> pure (fromInteger i)

fromPlutusTxId :: P.TxId -> Either P.ToCardanoError (TxId StandardCrypto)
fromPlutusTxId = fmap toShelleyTxId . P.toCardanoTxId

fromPlutusTxOut :: P.TxOut -> Either P.ToCardanoError (TxOut EmulatorEra)
fromPlutusTxOut = fmap (toShelleyTxOut ShelleyBasedEraAlonzo) . P.toCardanoTxOut emulatorNetworkId P.toCardanoTxOutDatumHash

fromPaymentPrivateKey :: P.PrivateKey -> TxBody EmulatorEra -> C.Api.KeyWitness C.Api.AlonzoEra
fromPaymentPrivateKey xprv txBody
  = C.Api.makeShelleyKeyWitness
      (C.Api.ShelleyTxBody C.Api.ShelleyBasedEraAlonzo txBody notUsed notUsed notUsed notUsed)
      (C.Api.WitnessPaymentExtendedKey (C.Api.PaymentExtendedSigningKey xprv))
  where
    notUsed = undefined -- hack so we can reuse code from cardano-api
