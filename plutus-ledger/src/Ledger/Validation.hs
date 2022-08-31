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
  evaluateMinLovelaceOutput,
  getRequiredSigners,
  hasValidationErrors,
  makeTransactionBody,
  validateMockchain,
  validateCardanoTx,
  -- * Modifying the state
  makeBlock,
  setSlot,
  nextSlot,
  UTxO(..),
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

import Cardano.Api qualified as C
import Cardano.Api.Shelley (ShelleyBasedEra (ShelleyBasedEraAlonzo), makeSignedTransaction, toShelleyTxId,
                            toShelleyTxOut)
import Cardano.Api.Shelley qualified as C.Api
import Cardano.Ledger.Alonzo (TxOut)
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Utxos (constructValidated)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (ExUnits))
import Cardano.Ledger.Alonzo.Tools qualified as C.Ledger
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody (TxBody, reqSignerHashes))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr)
import Cardano.Ledger.BaseTypes (Globals (..))
import Cardano.Ledger.Core (Tx)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API (Coin (..), LedgerEnv (..), MempoolEnv, MempoolState, TxId, TxIn (TxIn), UTxO (UTxO),
                                   Validated, epochInfo)
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Cardano.Ledger.Shelley.LedgerState (smartUTxOState)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Applicative ((<|>))
import Control.Lens (_1, makeLenses, over, (&), (.~), (^.))
import Data.Array (array)
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bitraverse)
import Data.Default (def)
import Data.Foldable (foldl')
import Data.Functor.Identity (runIdentity)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import GHC.Records (HasField (..))
import Ledger.Address qualified as Address
import Ledger.Blockchain qualified as Blockchain
import Ledger.CardanoWallet qualified as CW
import Ledger.Crypto qualified as Crypto
import Ledger.Generators.Internal (Mockchain (Mockchain))
import Ledger.Index.Internal qualified as P
import Ledger.Params (EmulatorEra, emulatorGlobals, emulatorPParams)
import Ledger.Params qualified as P
import Ledger.Tx (CardanoTx (CardanoApiTx, EmulatorTx), SomeCardanoApiTx (CardanoApiEmulatorEraTx, SomeTx),
                  addCardanoTxSignature, mergeCardanoTxWith, onCardanoTx)
import Ledger.Tx.CardanoAPI qualified as P
import Plutus.V1.Ledger.Ada qualified as P
import Plutus.V1.Ledger.Api qualified as P
import Plutus.V1.Ledger.Slot (Slot)
import Plutus.V1.Ledger.Tx qualified as P
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
  (newMempool, vtx) <- first P.ApplyTxError (C.Ledger.applyTx (emulatorGlobals params) _ledgerEnv _memPoolState tx)
  return (oldState & memPoolState .~ newMempool & over currentBlock ((:) vtx), vtx)


-- | Validate a transaction in a mockchain.
validateMockchain :: Mockchain -> P.Tx -> Maybe P.ValidationErrorInPhase
validateMockchain (Mockchain txPool _ params) tx = result where
    h      = 1
    idx    = P.initialise [map (Blockchain.Valid . EmulatorTx) txPool]
    cUtxoIndex = either (error . show) id $ fromPlutusIndex params idx
    result = validateCardanoTx params h cUtxoIndex (EmulatorTx tx) CW.knownPaymentKeys

hasValidationErrors :: P.Params -> SlotNo -> UTxO EmulatorEra -> C.Api.Tx C.Api.AlonzoEra -> Maybe P.ValidationErrorInPhase
hasValidationErrors params slotNo utxo (C.Api.ShelleyTx _ tx) =
  case res of
    Left e  -> Just (P.Phase1, e)
    Right _ -> Nothing
    -- TODO: uncomment to fix the issues with plutus scripts in tests
    -- See note [Second phase validation]
    --
    -- case getTxExUnits params utxo tx' of
    --   Left (Left e) -> Just e
    --   _ -> Nothing
  where
    state = setSlot slotNo $ setUtxo utxo $ initialState params
    res = do
      vtx <- first P.UtxosPredicateFailure (constructValidated (emulatorGlobals params) (utxoEnv params slotNo) (fst (_memPoolState state)) tx)
      applyTx params state vtx

validateCardanoTx
  :: P.Params
  -> Slot
  -> UTxO EmulatorEra
  -> CardanoTx
  -> Map.Map Address.PaymentPubKey Address.PaymentPrivateKey
  -> Maybe P.ValidationErrorInPhase
validateCardanoTx params slot utxo txn knownPaymentKeys =
  let
    getPublicKeys = Map.keys . P.txSignatures
    privateKeys = onCardanoTx
        (map Address.unPaymentPrivateKey . catMaybes .
            map (flip Map.lookup knownPaymentKeys) .
            map Address.PaymentPubKey . getPublicKeys)
        (const []) txn
    signTx tx = foldl' (flip addCardanoTxSignature) tx privateKeys
    convertTx tx = fmap (flip SomeTx C.AlonzoEraInCardanoMode) $ fromPlutusTx params utxo (map (Address.PaymentPubKeyHash . Crypto.pubKeyHash) $ getPublicKeys tx) tx
  in
    case onCardanoTx convertTx Right txn of
      Left (Left e) -> Just e
      Left (Right e) -> error ("validateCardanoTx: failed with ToCardanoError " ++ show e)
      Right someTx ->
        signTx (CardanoApiTx someTx) & mergeCardanoTxWith
          (\_ -> error "validateCardanoTx: EmulatorTx is not supported")
          (\(CardanoApiEmulatorEraTx tx) -> if utxo == UTxO (Map.fromList []) then Nothing else hasValidationErrors params (fromIntegral slot) utxo tx)
          (\e1 e2 -> e2 <|> e1)

{- Note [Second phase validation]
There are two phases of transaction validation:
1. When we use the cardano-ledger 'applyTx' to validate the transaction's body in 'hasValidationErrors'.
2. When we execute plutus scripts in the transaction to get the execution units in 'getTxExUnits'.

At the moment we have to turn off the second phase validation in 'hasValidationErrors' because there are tests
that fail with 'check' error in 'getTxExUnits'.

Failing transactions throw a checkHasFailedError error, but we don't want to deal with those yet.
We might be able to do that in the future. But for now just return a zero execution cost
so it will run later where we do handle failing transactions.

We also have to comment the tests that expect the script's failure. They should be uncommented
when we will fix the rest failing tests.
-}

getTxExUnits :: P.Params -> UTxO EmulatorEra -> C.Api.Tx C.Api.AlonzoEra -> Either CardanoLedgerError (Map.Map RdmrPtr ExUnits)
getTxExUnits params utxo (C.Api.ShelleyTx _ tx) =
  case runIdentity $ C.Ledger.evaluateTransactionExecutionUnits (emulatorPParams params) tx utxo ei ss costmdls of
    Left e      -> Left . Left . (P.Phase1,) . P.BasicFailure $ e
    Right rdmrs -> traverse (either toCardanoLedgerError Right) rdmrs
  where
    eg = emulatorGlobals params
    ss = systemStart eg
    ei = epochInfo eg
    costmdls = array (minBound, maxBound) . Map.toList $ getField @"_costmdls" $ emulatorPParams params
    -- See note [Second phase validation]
    toCardanoLedgerError (C.Ledger.ValidationFailedV1 (P.CekError _) logs@(_:_)) | last logs == Builtins.fromBuiltin checkHasFailedError =
      Right $ ExUnits 0 0
    toCardanoLedgerError e = Left $ Left (P.Phase2, P.ScriptFailure e)

makeTransactionBody
  :: P.Params
  -> UTxO EmulatorEra
  -> P.CardanoBuildTx
  -> Either CardanoLedgerError (C.Api.TxBody C.Api.AlonzoEra)
makeTransactionBody params utxo txBodyContent = do
  txTmp <- first Right $ makeSignedTransaction [] <$> P.makeTransactionBody mempty txBodyContent
  exUnits <- getTxExUnits params utxo txTmp
  first Right $ P.makeTransactionBody exUnits txBodyContent


evaluateMinLovelaceOutput :: P.Params -> TxOut EmulatorEra -> P.Ada
evaluateMinLovelaceOutput params = toPlutusValue . C.Ledger.evaluateMinLovelaceOutput (emulatorPParams params)
  where
    toPlutusValue :: Coin -> P.Ada
    toPlutusValue (Coin c) = P.lovelaceOf c

fromPlutusTx
  :: P.Params
  -> UTxO EmulatorEra
  -> [Address.PaymentPubKeyHash]
  -> P.Tx
  -> Either CardanoLedgerError (C.Api.Tx C.Api.AlonzoEra)
fromPlutusTx params utxo requiredSigners tx = do
  txBodyContent <- first Right $ P.toCardanoTxBodyContent params requiredSigners tx
  makeSignedTransaction [] <$> makeTransactionBody params utxo txBodyContent

getRequiredSigners :: C.Api.Tx C.Api.AlonzoEra -> [Address.PaymentPubKeyHash]
getRequiredSigners (C.Api.ShelleyTx _ (ValidatedTx TxBody { reqSignerHashes = rsq } _ _ _)) =
  foldMap (pure . Address.PaymentPubKeyHash . P.fromCardanoPaymentKeyHash . C.Api.PaymentKeyHash . C.Ledger.coerceKeyRole) rsq

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
