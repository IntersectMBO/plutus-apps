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
  ApplyTxError(..),
  EmulatorEra,
  EmApplyTxFailure(..),
  initialState,
  calculateMinFee,
  applyTx,
  hasValidationErrors,
  -- * Modifying the state
  makeBlock,
  setSlot,
  nextSlot,
  -- * Lenses
  ledgerEnv,
  memPoolState,
  currentBlock,
  previousBlocks,
  -- * Etc.
  emulatorGlobals
  ) where
import Cardano.Api.Shelley (NetworkId, ProtocolParameters, ShelleyBasedEra (ShelleyBasedEraAlonzo),
                            alonzoGenesisDefaults, makeSignedTransaction, shelleyGenesisDefaults, toShelleyTxId,
                            toShelleyTxOut)
import Cardano.Api.Shelley qualified as Shelley
import Cardano.Ledger.Alonzo (AlonzoEra, TxOut)
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Utxos (UtxosPredicateFailure, constructValidated)
import Cardano.Ledger.Alonzo.Tx (minfee, wits)
import Cardano.Ledger.BaseTypes (Globals (..))
import Cardano.Ledger.Core (PParams, Tx)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API (ApplyTxError (..), Coin (..), LedgerEnv (..), MempoolEnv, MempoolState,
                                   NewEpochState, UTxOState (..), UtxoEnv (..), Validated, mkShelleyGlobals, nesEs)
import Cardano.Ledger.Shelley.API qualified as Shelley.API
import Cardano.Ledger.Shelley.LedgerState (esPp, smartUTxOState)
import Cardano.Ledger.Shelley.UTxO (UTxO (UTxO))
import Cardano.Ledger.TxIn (TxId, TxIn (TxIn))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (mkSlotLength)
import Control.Lens (_1, makeLenses, over, view, (&), (.~), (^.))
import Data.Bifunctor (Bifunctor (..))
import Data.Default (def)
import Data.Map qualified as Map
import Ledger qualified as P
import Ledger.Tx.CardanoAPI qualified as P
import Plutus.V1.Ledger.Ada qualified as P

type EmulatorEra = AlonzoEra StandardCrypto

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
  { _ledgerEnv = Shelley.API.mkMempoolEnv emulatorNes 0
  , _memPoolState = Shelley.API.mkMempoolState emulatorNes
  , _currentBlock = []
  , _previousBlocks = []
  }

-- toAddr :: (P.PubKey, Coin) -> Maybe (Addr StandardCrypto, Coin)
-- toAddr (pk, coin) =
--   case P.toCardanoAddress emulatorNetworkId (P.pubKeyHashAddress (P.paymentPubKeyHash $ P.PaymentPubKey pk) Nothing) of
--     Right (AddressInEra _ (ShelleyAddress _ paymentCredential stakeAddressReference))
--         -> Just (Addr Testnet paymentCredential stakeAddressReference, coin)
--     _ -> Nothing

{-| Reason for failing to add a transaction to the ledger
-}
data EmApplyTxFailure =
  ApplyTxFailed (ApplyTxError EmulatorEra)
  | UtxosPredicateFailures [UtxosPredicateFailure EmulatorEra]
  deriving Show

{-| Make a 'UtxoEnv' from the emulated ledger state, using an empty set of delegations.
-}
utxoEnv :: EmulatedLedgerState -> UtxoEnv EmulatorEra
utxoEnv state =
  let LedgerEnv{ledgerSlotNo, ledgerPp} = view ledgerEnv state
  in UtxoEnv ledgerSlotNo ledgerPp mempty (Shelley.API.GenDelegs mempty)

applyTx ::
  EmulatedLedgerState ->
  Tx EmulatorEra ->
  Either EmApplyTxFailure (EmulatedLedgerState, Validated (Tx EmulatorEra))
applyTx oldState@EmulatedLedgerState{_ledgerEnv, _memPoolState} tx = do
  (newMempool, vtx) <- first ApplyTxFailed (Shelley.API.applyTx emulatorGlobals _ledgerEnv _memPoolState tx)
  return (oldState & memPoolState .~ newMempool & over currentBlock ((:) vtx), vtx)


emulatorNetworkId :: NetworkId
emulatorNetworkId = Shelley.Testnet $ Shelley.NetworkMagic 1

{-| A sensible default 'Globals' value for the emulator
-}
emulatorGlobals :: Globals
emulatorGlobals = mkShelleyGlobals
  shelleyGenesisDefaults
  (fixedEpochInfo (EpochSize 432000) (mkSlotLength 8)) -- ?
  2 -- maxMajorPV

emulatorNes :: NewEpochState EmulatorEra
emulatorNes = Shelley.API.initialState shelleyGenesisDefaults alonzoGenesisDefaults

emulatorPParams :: PParams EmulatorEra
emulatorPParams = esPp $ nesEs emulatorNes

emulatorProtocolParameters :: Shelley.ProtocolParameters
emulatorProtocolParameters = Shelley.fromLedgerPParams ShelleyBasedEraAlonzo emulatorPParams

hasValidationErrors :: [P.PaymentPubKeyHash] -> P.PaymentPrivateKey -> P.UtxoIndex -> P.Tx -> Maybe String
hasValidationErrors requiredSigners privKey utxo ptx =
  case res of
    Left e  -> Just $ show e
    Right _ -> Nothing
  where
    utxoState = fromPlutusIndex utxo
    state = initialState & memPoolState . _1 .~ utxoState
    tx = fromPlutusTx requiredSigners privKey ptx
    res = do
      vtx <- first UtxosPredicateFailures (constructValidated emulatorGlobals (utxoEnv state) utxoState tx)
      applyTx state vtx

fromPlutusTx :: [P.PaymentPubKeyHash] -> P.PaymentPrivateKey -> P.Tx -> Tx EmulatorEra
fromPlutusTx requiredSigners privKey = either (error . show) id . mkPartialTx requiredSigners privKey emulatorProtocolParameters emulatorNetworkId

calculateMinFee :: [P.PaymentPubKeyHash] -> P.PaymentPrivateKey -> P.Tx -> P.Value
calculateMinFee requiredSigners privKey tx = case minfee emulatorPParams $ fromPlutusTx requiredSigners privKey tx of
  Coin fee -> P.lovelaceValueOf (fee + 2) -- TODO: why this discrepancy of 1 or 2 lovelace?

mkPartialTx
  :: [P.PaymentPubKeyHash]
  -> P.PaymentPrivateKey
  -> ProtocolParameters
  -> NetworkId
  -> P.Tx
  -> Either P.ToCardanoError (Tx EmulatorEra)
mkPartialTx requiredSigners privKey params networkId tx = do
  let txBody = P.toCardanoTxBody requiredSigners (Just params) networkId tx
      witnesses = pure . fromPaymentPrivateKey privKey <$> txBody
  stx <- makeSignedTransaction <$> witnesses <*> txBody
  case stx of
    Shelley.ShelleyTx _ tx' -> pure tx'

fromPlutusIndex :: P.UtxoIndex -> UTxOState EmulatorEra
fromPlutusIndex (P.UtxoIndex m) = smartUTxOState
  (UTxO $ Map.fromList $ bimap fromPlutusTxOutRef fromPlutusTxOut <$> Map.toList m)
  (Coin 0)
  (Coin 0)
  def

fromPlutusTxOutRef :: P.TxOutRef -> TxIn StandardCrypto
fromPlutusTxOutRef (P.TxOutRef txId i) = TxIn (fromPlutusTxId txId) (fromInteger i)

fromPlutusTxId :: P.TxId -> TxId StandardCrypto
fromPlutusTxId = either (error . show) toShelleyTxId . P.toCardanoTxId

fromPlutusTxOut :: P.TxOut -> TxOut EmulatorEra
fromPlutusTxOut = toShelleyTxOut ShelleyBasedEraAlonzo . either (error . show) id . P.toCardanoTxOut emulatorNetworkId P.toCardanoTxOutDatumHash

fromPaymentPrivateKey :: P.PaymentPrivateKey -> Shelley.TxBody Shelley.AlonzoEra -> Shelley.KeyWitness Shelley.AlonzoEra
fromPaymentPrivateKey (P.PaymentPrivateKey xprv) txBody
  = Shelley.makeShelleyKeyWitness txBody (Shelley.WitnessPaymentExtendedKey (Shelley.PaymentExtendedSigningKey xprv))
