{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Wallet.Emulator.MultiAgent where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Node.Emulator.Generators (alwaysSucceedPolicy, alwaysSucceedPolicyId, signAll)
import Cardano.Node.Emulator.Internal.Node.Chain qualified as Chain
import Cardano.Node.Emulator.Internal.Node.Params (Params (..))
import Control.Lens (AReview, Getter, Lens', Prism', anon, at, folded, makeLenses, prism', reversed, review, to, view,
                     (&), (.~), (^..))
import Control.Monad.Freer (Eff, Member, Members, interpret, send, subsume, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMessage, LogMsg, LogObserve, handleObserveLog, mapLog)
import Control.Monad.Freer.Extras.Modify (handleZoomedState, raiseEnd, writeIntoState)
import Control.Monad.Freer.State (State)
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Ledger.Address (PaymentPubKeyHash, pubKeyHashAddress)
import Ledger.AddressMap qualified as AM
import Ledger.Blockchain (Blockchain)
import Ledger.Index (minAdaTxOut)
import Ledger.Index qualified as Index
import Ledger.Scripts (MintingPolicy (getMintingPolicy))
import Ledger.Slot (Slot)
import Ledger.Tx.CardanoAPI (ToCardanoError, pattern CardanoEmulatorEraTx)
import Ledger.Tx.CardanoAPI.Internal qualified as C (toCardanoPlutusScript, toCardanoValidityRange, zeroExecutionUnits)
import Ledger.Tx.CardanoAPI.Internal qualified as CardanoAPI (toCardanoAddressInEra, toCardanoTxOutValue)
import Ledger.Tx.Internal (TxOut (TxOut), emptyTxBodyContent, txOutValue)
import Ledger.Tx.Internal qualified as Tx (TxOut (getTxOut))
import Ledger.Value.CardanoAPI (lovelaceToValue)
import Ledger.Value.CardanoAPI qualified as CardanoAPI
import Plutus.ChainIndex.ChainIndexError qualified as ChainIndex (ChainIndexError)
import Plutus.ChainIndex.ChainIndexLog qualified as ChainIndex (ChainIndexLog)
import Plutus.ChainIndex.Effects qualified as ChainIndex (ChainIndexControlEffect, ChainIndexQueryEffect)
import Plutus.ChainIndex.Emulator.Handlers qualified as ChainIndex (handleControl, handleQuery)
import Plutus.Trace.Emulator.Types (ContractInstanceLog, EmulatedWalletEffects, EmulatedWalletEffects', UserThreadMsg)
import Plutus.Trace.Scheduler qualified as Scheduler
import Plutus.V1.Ledger.Scripts qualified as Script
import PlutusTx (toData)
import Prettyprinter (Pretty (pretty), colon, (<+>))
import Wallet.API qualified as WAPI
import Wallet.Emulator.LogMessages (RequestHandlerLogMsg, TxBalanceMsg)
import Wallet.Emulator.NodeClient qualified as NC
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Emulator.Wallet qualified as Wallet

-- | An event with a timestamp measured in emulator time
--   (currently: 'Slot')
data EmulatorTimeEvent e =
    EmulatorTimeEvent
        { _eteEmulatorTime :: Slot
        , _eteEvent        :: e
        }
    deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
    deriving anyclass (ToJSON, FromJSON)

makeLenses ''EmulatorTimeEvent

instance Pretty e => Pretty (EmulatorTimeEvent e) where
    pretty EmulatorTimeEvent{_eteEmulatorTime, _eteEvent} =
        pretty _eteEmulatorTime <> colon <+> pretty _eteEvent

emulatorTimeEvent :: Slot -> Prism' (EmulatorTimeEvent e) e
emulatorTimeEvent t = prism' (EmulatorTimeEvent t) (\case { EmulatorTimeEvent s e | s == t -> Just e; _ -> Nothing})

-- | Events produced by the blockchain emulator.
data EmulatorEvent' =
    ChainEvent Chain.ChainEvent
    | ClientEvent Wallet NC.NodeClientEvent
    | WalletEvent Wallet Wallet.WalletEvent
    | ChainIndexEvent Wallet ChainIndex.ChainIndexLog
    | SchedulerEvent Scheduler.SchedulerLog
    | InstanceEvent ContractInstanceLog
    | UserThreadEvent UserThreadMsg
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty EmulatorEvent' where
    pretty = \case
        ClientEvent w e     -> pretty w <> colon <+> pretty e
        ChainEvent e        -> pretty e
        WalletEvent w e     -> pretty w <> colon <+> pretty e
        ChainIndexEvent w e -> pretty w <> colon <+> pretty e
        SchedulerEvent e    -> pretty e
        InstanceEvent e     -> pretty e
        UserThreadEvent e   -> pretty e

type EmulatorEvent = EmulatorTimeEvent EmulatorEvent'

chainEvent :: Prism' EmulatorEvent' Chain.ChainEvent
chainEvent = prism' ChainEvent (\case { ChainEvent c -> Just c; _ -> Nothing })

walletClientEvent :: Wallet -> Prism' EmulatorEvent' NC.NodeClientEvent
walletClientEvent w = prism' (ClientEvent w) (\case { ClientEvent w' c | w == w' -> Just c; _ -> Nothing })

walletEvent :: Wallet -> Prism' EmulatorEvent' Wallet.WalletEvent
walletEvent w = prism' (WalletEvent w) (\case { WalletEvent w' c | w == w' -> Just c; _ -> Nothing })

walletEvent' :: Prism' EmulatorEvent' (Wallet, Wallet.WalletEvent)
walletEvent' = prism' (uncurry WalletEvent) (\case { WalletEvent w c -> Just (w, c); _ -> Nothing })

chainIndexEvent :: Wallet -> Prism' EmulatorEvent' ChainIndex.ChainIndexLog
chainIndexEvent w = prism' (ChainIndexEvent w) (\case { ChainIndexEvent w' c | w == w' -> Just c; _ -> Nothing })

schedulerEvent :: Prism' EmulatorEvent' Scheduler.SchedulerLog
schedulerEvent = prism' SchedulerEvent (\case { SchedulerEvent e -> Just e; _ -> Nothing })

instanceEvent :: Prism' EmulatorEvent' ContractInstanceLog
instanceEvent = prism' InstanceEvent (\case { InstanceEvent e -> Just e; _ -> Nothing })

userThreadEvent :: Prism' EmulatorEvent' UserThreadMsg
userThreadEvent = prism' UserThreadEvent (\case { UserThreadEvent e -> Just e ; _ -> Nothing })

type EmulatedWalletControlEffects =
        '[ NC.NodeClientControlEffect
         , ChainIndex.ChainIndexControlEffect
         , Wallet.SigningProcessControlEffect
         , LogObserve (LogMessage T.Text)
         , LogMsg T.Text
        ]

{- Note [Control effects]

Plutus contracts interact with the outside world through a number of different
effects. These effects are captured in 'EmulatedWalletEffects'. They are
supposed to be a realistic representation of the capabilities that contracts
will have in the real world, when the system is released.

In the tests we often want to simulate events that happened "outside of the
contract". For example: A new block is added to the chain, or a user takes the
transaction and emails it to another person to sign, before sending it to the
node. These kinds of events cannot be expressed in 'EmulatedWalletEffects',
because it would make the emulated wallet effects unrealistic - Plutus
contracts in the real world will not have the power to decide when a new block
gets added to the chain, or to control who adds their signature to a
transaction.

But in the emulated world of our unit tests we, the contract authors, would very
much like to have this power. That is why there is a second list of emulator
effects: 'EmulatedWalletControlEffects' are the of effects that only make sense
in the emulator, but not in the real world. With 'EmulatedWalletControlEffects'
we can control the blockchain and the lines of communication between the
emulated components.

By being clear about which of our (ie. the contract authors) actions
require the full power of 'EmulatedWalletControlEffects', we can be more
confident that our contracts will run in the real world, and not just in the
test environment. That is why there are two similar but different constructors
for 'MultiAgentEffect': 'WalletAction' is used for things that we will be able
to do in the real world, and 'WalletControlAction' is for everything else.

-}

-- | The type of actions in the emulator.
data MultiAgentEffect r where
    -- | A direct action performed by a wallet. Usually represents a "user action", as it is
    -- triggered externally.
    WalletAction :: Wallet -> Eff EmulatedWalletEffects r -> MultiAgentEffect r

data MultiAgentControlEffect r where
    -- | An action affecting the emulated parts of a wallet (only available in emulator - see note [Control effects].)
    WalletControlAction :: Wallet -> Eff EmulatedWalletControlEffects r -> MultiAgentControlEffect r

-- | Run an action in the context of a wallet (ie. agent)
walletAction
    :: (Member MultiAgentEffect effs)
    => Wallet
    -> Eff EmulatedWalletEffects r
    -> Eff effs r
walletAction wallet act = send (WalletAction wallet act)

handleMultiAgentEffects ::
    forall effs.
    Member MultiAgentEffect effs
    => Wallet
    -> Eff (EmulatedWalletEffects' effs)
    ~> Eff effs
handleMultiAgentEffects wallet =
    interpret (raiseWallet @(LogMsg T.Text) wallet)
        . interpret (raiseWallet @(LogMsg TxBalanceMsg) wallet)
        . interpret (raiseWallet @(LogMsg RequestHandlerLogMsg) wallet)
        . interpret (raiseWallet @(LogObserve (LogMessage T.Text)) wallet)
        . interpret (raiseWallet @ChainIndex.ChainIndexQueryEffect wallet)
        . interpret (raiseWallet @WAPI.NodeClientEffect wallet)
        . interpret (raiseWallet @(Error WAPI.WalletAPIError) wallet)
        . interpret (raiseWallet @WAPI.WalletEffect wallet)

raiseWallet :: forall f effs.
    ( Member f EmulatedWalletEffects
    , Member MultiAgentEffect effs
    )
    => Wallet
    -> f
    ~> Eff effs
raiseWallet wllt = walletAction wllt . send

-- | Run a control action in the context of a wallet
walletControlAction
    :: (Member MultiAgentControlEffect effs)
    => Wallet
    -> Eff EmulatedWalletControlEffects r
    -> Eff effs r
walletControlAction wallet = send . WalletControlAction wallet

-- | The state of the emulator itself.
data EmulatorState = EmulatorState {
    _chainState   :: Chain.ChainState, -- ^ Mockchain
    _walletStates :: Map Wallet Wallet.WalletState, -- ^ The state of each agent.
    _emulatorLog  :: [LogMessage EmulatorEvent] -- ^ The emulator log messages, with the newest last.
    } deriving (Show)

makeLenses ''EmulatorState

walletState :: Wallet -> Lens' EmulatorState Wallet.WalletState
walletState wallet = walletStates . at wallet . anon emptyState (const False) where
    emptyState = fromMaybe (error $ "walletState: not a known wallet: " <> show wallet) (Wallet.emptyWalletState wallet)

-- | Get the blockchain as a list of blocks, starting with the oldest (genesis)
--   block.
chainOldestFirst :: Lens' EmulatorState Blockchain
chainOldestFirst = chainState . Chain.chainNewestFirst . reversed

chainUtxo :: Getter EmulatorState AM.AddressMap
chainUtxo = chainState . Chain.chainNewestFirst . to AM.fromChain

-- | Get a map with the total value of each wallet's "own funds".
fundsDistribution :: EmulatorState -> Map Wallet C.Value
fundsDistribution st =
    let fullState = view chainUtxo st
        wallets = st ^.. walletStates . to Map.keys . folded
        walletFunds = flip fmap wallets $ \w ->
            (w, foldMap (txOutValue . snd) $ view (AM.fundsAt (Wallet.mockWalletAddress w)) fullState)
    in Map.fromList walletFunds

-- | Get the emulator log.
emLog :: EmulatorState -> [LogMessage EmulatorEvent]
emLog = view emulatorLog

emptyEmulatorState :: EmulatorState
emptyEmulatorState = EmulatorState {
    _chainState = Chain.emptyChainState,
    _walletStates = mempty,
    _emulatorLog = mempty
    }

-- | Initialise the emulator state with a blockchain.
emulatorState :: Blockchain -> EmulatorState
emulatorState bc = emptyEmulatorState
    & chainState .~ Chain.fromBlockchain bc

-- | Initialise the emulator state with a pool of pending transactions.
emulatorStatePool :: Chain.TxPool -> EmulatorState
emulatorStatePool tp = emptyEmulatorState
    & chainState . Chain.txPool .~ tp

{- Note [Creating wallets with multiple outputs]

Every transaction needs a collateral input, which is a wallet output that gets spent
when the transaction fails to validate on chain (phase 2 validation). This output is required
to be an Ada-only output. To make sure we always have an Ada-only output available during emulation,
we create 10 Ada-only outputs per wallet here.
-}

-- | Initialise the emulator state with a single pending transaction that
--   creates the initial distribution of funds to public key addresses.
emulatorStateInitialDist :: Params -> Map PaymentPubKeyHash C.Value -> Either ToCardanoError EmulatorState
emulatorStateInitialDist params mp = do
    minAdaEmptyTxOut <- mMinAdaTxOut
    outs <- traverse (mkOutputs minAdaEmptyTxOut) (Map.toList mp)
    validityRange <- C.toCardanoValidityRange WAPI.defaultSlotRange
    mintWitness <- either (error . show) pure $ C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
                           <$> (C.PScript <$> C.toCardanoPlutusScript
                                                  (C.AsPlutusScript C.AsPlutusScriptV2)
                                                  (getMintingPolicy alwaysSucceedPolicy))
                           <*> pure C.NoScriptDatumForMint
                           <*> pure (C.fromPlutusData $ toData Script.unitRedeemer)
                           <*> pure C.zeroExecutionUnits
    let
        txBodyContent = emptyTxBodyContent
           { C.txIns = [ (Index.genesisTxIn, C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending)) ]
           , C.txInsCollateral = C.TxInsCollateral C.CollateralInBabbageEra [Index.genesisTxIn]
           , C.txMintValue = C.TxMintValue C.MultiAssetInBabbageEra (fold $ Map.map CardanoAPI.noAdaValue mp)
                              (C.BuildTxWith (Map.singleton alwaysSucceedPolicyId mintWitness))
           , C.txOuts = Tx.getTxOut <$> concat outs
           , C.txValidityRange = validityRange
           }
    txBody <- either (error . ("emulatorStateInitialDist: Can't create TxBody: " <>) . show) pure $ C.makeTransactionBody txBodyContent
    let cTx = signAll $ CardanoEmulatorEraTx $ C.Tx txBody []
    pure $ emulatorStatePool [cTx]
    where
        -- we start with an empty TxOut and we adjust it to be sure that the contained Adas fit the size
        -- of the TxOut
        mMinAdaTxOut = do
          let k = fst $ head $ Map.toList mp
          emptyTxOut <- mkOutput k mempty
          pure $ minAdaTxOut (emulatorPParams params) emptyTxOut
        -- See [Creating wallets with multiple outputs]
        mkOutputs minAda (key, vl) = traverse (mkOutput key) (splitInto10 vl minAda)
        splitInto10 vl minAda = if count <= 1
            then [vl]
            else replicate (fromIntegral count) (lovelaceToValue (ada `div` count)) ++ remainder
            where
                ada = case C.valueToLovelace vl of
                    Just lovelace -> lovelace
                    Nothing       -> C.selectLovelace vl - minAda
                -- Make sure we don't make the outputs too small
                count = min 10 $ ada `div` minAda
                remainder = [ vl <> lovelaceToValue (-ada) | isNothing (C.valueToLovelace vl) ]
        mkOutput key vl = do
            addr <- CardanoAPI.toCardanoAddressInEra (pNetworkId params) (pubKeyHashAddress key Nothing)
            pure $ TxOut $ C.TxOut addr (CardanoAPI.toCardanoTxOutValue vl) C.TxOutDatumNone C.ReferenceScriptNone

type MultiAgentEffs =
    '[ State EmulatorState
     , LogMsg EmulatorEvent'
     , Error WAPI.WalletAPIError
     , Error ChainIndex.ChainIndexError
     , Chain.ChainEffect
     , Chain.ChainControlEffect
     ]

handleMultiAgentControl
    :: forall effs. Members MultiAgentEffs effs
    => Eff (MultiAgentControlEffect ': effs) ~> Eff effs
handleMultiAgentControl = interpret $ \case
    WalletControlAction wallet act -> do
        let
            p1 :: AReview EmulatorEvent' Wallet.WalletEvent
            p1 = walletEvent wallet
            p2 :: AReview EmulatorEvent' NC.NodeClientEvent
            p2 = walletClientEvent wallet
            p3 :: AReview EmulatorEvent' ChainIndex.ChainIndexLog
            p3 = chainIndexEvent wallet
            p4 :: AReview EmulatorEvent' T.Text
            p4 =  walletEvent wallet . Wallet._GenericLog
        act
            & raiseEnd
            & NC.handleNodeControl
            & interpret ChainIndex.handleControl
            & Wallet.handleSigningProcessControl
            & handleObserveLog
            & interpret (mapLog (review p4))
            & interpret (handleZoomedState (walletState wallet))
            & interpret (mapLog (review p1))
            & interpret (handleZoomedState (walletState wallet . Wallet.nodeClient))
            & interpret (mapLog (review p2))
            & interpret (handleZoomedState (walletState wallet . Wallet.chainIndexEmulatorState))
            & interpret (mapLog (review p3))
            & interpret (handleZoomedState (walletState wallet . Wallet.signingProcess))
            & interpret (writeIntoState emulatorLog)

handleMultiAgent
    :: forall effs. Members MultiAgentEffs effs
    => Eff (MultiAgentEffect ': effs) ~> Eff effs
handleMultiAgent = interpret $ \case
    -- TODO: catch, log, and rethrow wallet errors?
    WalletAction wallet act ->  do
        let
            p1 :: AReview EmulatorEvent' Wallet.WalletEvent
            p1 = walletEvent wallet
            p2 :: AReview EmulatorEvent' NC.NodeClientEvent
            p2 = walletClientEvent wallet
            p3 :: AReview EmulatorEvent' ChainIndex.ChainIndexLog
            p3 = chainIndexEvent wallet
            p4 :: AReview EmulatorEvent' T.Text
            p4 = walletEvent wallet . Wallet._GenericLog
            p5 :: AReview EmulatorEvent' RequestHandlerLogMsg
            p5 = walletEvent wallet . Wallet._RequestHandlerLog
            p6 :: AReview EmulatorEvent' TxBalanceMsg
            p6 = walletEvent wallet . Wallet._TxBalanceLog
        act
            & raiseEnd
            & interpret Wallet.handleWallet
            & subsume
            & NC.handleNodeClient
            & interpret ChainIndex.handleQuery
            & handleObserveLog
            & interpret (mapLog (review p5))
            & interpret (mapLog (review p6))
            & interpret (mapLog (review p4))
            & interpret (handleZoomedState (walletState wallet))
            & interpret (mapLog (review p1))
            & interpret (handleZoomedState (walletState wallet . Wallet.nodeClient))
            & interpret (mapLog (review p2))
            & interpret (handleZoomedState (walletState wallet . Wallet.chainIndexEmulatorState))
            & interpret (mapLog (review p3))
            & interpret (handleZoomedState (walletState wallet . Wallet.signingProcess))
            & interpret (writeIntoState emulatorLog)
