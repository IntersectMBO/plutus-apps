{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
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
  applyTx,
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

import Cardano.Api.Shelley (alonzoGenesisDefaults, shelleyGenesisDefaults)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Utxos (UtxosPredicateFailure, constructValidated)
import Cardano.Ledger.BaseTypes (Globals (..), Network (..), boundRational, mkActiveSlotCoeff)
import Cardano.Ledger.Core (Tx)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (GenDelegs (..))
import Cardano.Ledger.Shelley.API (Addr, ApplyTxError (..), Coin (..), LedgerEnv (..), MempoolEnv, MempoolState,
                                   NewEpochState, ShelleyGenesis (..), UtxoEnv (..), Validated)
import Cardano.Ledger.Shelley.API qualified as Shelley.API
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.Lens (makeLenses, over, view, (&), (.~), (^.))
import Data.Bifunctor (Bifunctor (..))
import Data.Map qualified as Map
import Data.Time.Format.ISO8601 qualified as F

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
initialState :: [(Addr StandardCrypto, Coin)] -> EmulatedLedgerState
initialState initialDistribution = EmulatedLedgerState
  { _ledgerEnv = Shelley.API.mkMempoolEnv nes 0
  , _memPoolState = Shelley.API.mkMempoolState nes
  , _currentBlock = []
  , _previousBlocks = []
  }
  where
    nes :: NewEpochState EmulatorEra
    nes = Shelley.API.initialState sg alonzoGenesisDefaults
    sg :: ShelleyGenesis EmulatorEra
    sg = shelleyGenesisDefaults { sgInitialFunds = Map.fromList initialDistribution }

{-| Reason for failing to add a transaction to the ledger
-}
data EmApplyTxFailure =
  ApplyTxFailed (ApplyTxError EmulatorEra)
  | UtxosPredicateFailures [UtxosPredicateFailure EmulatorEra]
  | EmulatorGlobalsFailure

{-| Make a 'UtxoEnv' from the emulated ledger state, using an empty set of delegations.
-}
utxoEnv :: EmulatedLedgerState -> UtxoEnv EmulatorEra
utxoEnv state =
  let LedgerEnv{ledgerSlotNo, ledgerPp} = view ledgerEnv state
  in UtxoEnv ledgerSlotNo ledgerPp mempty (GenDelegs mempty)

applyTx ::
  EmulatedLedgerState ->
  Tx EmulatorEra ->
  Either EmApplyTxFailure (EmulatedLedgerState, Validated (Tx EmulatorEra))
applyTx oldState@EmulatedLedgerState{_ledgerEnv, _memPoolState} tx = do
  emulatorGlobals' <- maybe (Left EmulatorGlobalsFailure) pure emulatorGlobals
  tx' <- first UtxosPredicateFailures (constructValidated emulatorGlobals' (utxoEnv oldState) (fst _memPoolState) tx)
  (newMempool, vtx) <- first ApplyTxFailed (Shelley.API.applyTx emulatorGlobals' _ledgerEnv _memPoolState tx')
  return (oldState & memPoolState .~ newMempool & over currentBlock ((:) vtx), vtx)

{-| A sensible default 'Globals' value for the emulator
-}
emulatorGlobals :: Maybe Globals
emulatorGlobals = do
  start <- F.iso8601ParseM "2017-09-23T21:44:51Z"
  asc <- boundRational 0.05
  pure $ Globals
    { epochInfoWithErr = fixedEpochInfo (EpochSize 432000) (mkSlotLength 8) -- ?
    , slotsPerKESPeriod = 129600
    , stabilityWindow = 129600 -- 3k/f
    , randomnessStabilisationWindow = 172800 -- 4k/f
    , securityParameter = 2160 -- k
    , maxKESEvo = 62
    , quorum = 5
    , maxMajorPV = 2
    , maxLovelaceSupply = 45000000000000000
    , activeSlotCoeff = mkActiveSlotCoeff asc -- f = 0.05
    , networkId = Testnet
    , systemStart = SystemStart start
    }
