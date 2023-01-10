{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-
This module provides a list of folds over the emulator event stream. To apply
the folds in this module to a stream of events, use
'Wallet.Emulator.Stream.foldEmulatorStreamM'. See note [Emulator event stream].

-}
module Wallet.Emulator.Folds (
    EmulatorEventFold
    , EmulatorEventFoldM
    , EmulatorFoldErr(..)
    , describeError
    -- * Folds for contract instances
    , instanceState
    , instanceRequests
    , instanceResponses
    , instanceOutcome
    , instanceTransactions
    , Outcome(..)
    , instanceLog
    , instanceAccumState
    -- * Folds for transactions and the UTXO set
    , chainEvents
    , ScriptEvent(..)
    , scriptEvents
    , failedTransactions
    , validatedTransactions
    , utxoAtAddress
    , valueAtAddress
    -- * Folds for individual wallets (emulated agents)
    , walletFunds
    , walletFees
    , walletTxBalanceEvents
    , walletsAdjustedTxEvents
    -- * Folds that are used in the Playground
    , annotatedBlockchain
    , blockchain
    , emulatorLog
    , userLog
    -- * Etc.
    , renderLines
    , preMapMaybeM
    , preMapMaybe
    , postMapM
    , mkTxLogs
    ) where

import Cardano.Api.Shelley qualified as C
import Cardano.Binary (serialize')
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo.TxInfo
import Cardano.Ledger.Alonzo.TxWitness qualified as Alonzo
import Cardano.Ledger.Alonzo.Tx qualified as Alonzo.Tx
import Cardano.Ledger.Babbage qualified as Babbage
import Cardano.Ledger.Crypto qualified as Crypto
import Cardano.Ledger.Mary.Value qualified as Mary
import Cardano.Ledger.Shelley.UTxO qualified as Shelley.UTxO
import Control.Applicative ((<|>))
import Control.Foldl (Fold (Fold), FoldM (FoldM))
import Control.Foldl qualified as L
import Control.Lens hiding (Empty, Fold)
import Control.Monad ((>=>))
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (Error, throwError)
import Data.Aeson qualified as JSON
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Maybe.Strict (strictMaybeToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (Block, OnChainTx (Invalid, Valid), TxId)
import Ledger.Ada qualified as Ada
import Ledger.AddressMap (UtxoMap)
import Ledger.AddressMap qualified as AM
import Ledger.Constraints.OffChain (UnbalancedTx)
import Ledger.Index (ValidationError, ValidationPhase (Phase1, Phase2), UtxoIndex, ExBudget)
import Ledger.Tx (CardanoTx(CardanoApiTx), getCardanoTxFee, txOutValue, SomeCardanoApiTx(CardanoApiEmulatorEraTx))
import Ledger.Validation (fromPlutusIndex, EmulatorEra)
import Ledger.Value (Value)
import Plutus.Contract (Contract)
import Plutus.Contract.Effects (PABReq, PABResp, _BalanceTxReq)
import Plutus.Contract.Request (MkTxLog)
import Plutus.Contract.Resumable (Request, Response)
import Plutus.Contract.Resumable qualified as State
import Plutus.Contract.Types (ResumableResult (_finalState, _observableState, _requests))
import Plutus.Trace.Emulator.ContractInstance (ContractInstanceState, addEventInstanceState, emptyInstanceState,
                                               instContractState, instEvents, instHandlersHistory)
import Plutus.Trace.Emulator.Types (ContractInstanceLog, ContractInstanceMsg (ContractLog), ContractInstanceTag,
                                    UserThreadMsg, _HandledRequest, cilMessage, cilTag, toInstanceState)
import Plutus.V1.Ledger.Address (Address)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty, vsep)
import Prettyprinter.Render.Text (renderStrict)
import Wallet.Emulator.Chain (ChainEvent (SlotAdd, TxnValidate, TxnValidationFail), _TxnValidate, _TxnValidationFail)
import Wallet.Emulator.LogMessages (_AdjustingUnbalancedTx, _BalancingUnbalancedTx, _ValidationFailed)
import Wallet.Emulator.MultiAgent (EmulatorEvent, EmulatorTimeEvent, chainEvent, eteEvent, instanceEvent,
                                   userThreadEvent, walletClientEvent, walletEvent')
import Wallet.Emulator.NodeClient (_TxSubmit)
import Wallet.Emulator.Wallet (Wallet, _RequestHandlerLog, _TxBalanceLog, mockWalletAddress)
import Wallet.Rollup qualified as Rollup
import Wallet.Rollup.Types (AnnotatedTx)

type EmulatorEventFold a = Fold EmulatorEvent a

-- | A fold over emulator events that can fail with 'EmulatorFoldErr'
type EmulatorEventFoldM effs a = FoldM (Eff effs) EmulatorEvent a

-- | Transactions that failed to validate, in the given validation phase (if specified).
failedTransactions :: Maybe ValidationPhase -> EmulatorEventFold [(UtxoIndex, TxId, CardanoTx, ValidationError, Value)]
failedTransactions phase = preMapMaybe (f >=> filterPhase phase) L.list
    where
        f e = preview (eteEvent . chainEvent . _TxnValidationFail) e
          <|> preview (eteEvent . walletEvent' . _2 . _TxBalanceLog . _ValidationFailed) e
        filterPhase Nothing (idx, _, i, t, v, c)   = Just (idx, i, t, v, c)
        filterPhase (Just p) (idx, p', i, t, v, c) = if p == p' then Just (idx, i, t, v, c) else Nothing

-- | Transactions that were validated
validatedTransactions :: EmulatorEventFold [(UtxoIndex, TxId, CardanoTx)]
validatedTransactions = preMapMaybe (preview (eteEvent . chainEvent . _TxnValidate)) L.list

{-| Execution of a script
-}
data ScriptEvent =
    ScriptEvent
        { seScriptHash    :: C.ScriptHash
        -- ^ Hash of the scrupt
        , seScript        :: Alonzo.Scripts.Script (Babbage.BabbageEra Crypto.StandardCrypto)
        -- ^ The script itself
        -- NB: Type 'C.Script C.BabbageEra' would be much better, but the function
        -- 'Cardano.Api.Script.fromShelleyBasedScript' is not exported
        -- in cardano-api :(

        , seScriptSize    :: Int
        -- ^ Script size (unapplied) in bytes

        , seExUnits       :: ExBudget
        -- ^ Cost of the script 
        , seScriptPurpose :: String
        -- ^ Purpose of the script

        -- TODO: Add the redeemer and datum (if applicable)?
        } deriving stock Generic
          deriving anyclass JSON.ToJSON

-- | Scripts that were run
scriptEvents :: EmulatorEventFold [ScriptEvent]
scriptEvents = preMapMaybe (preview (eteEvent . chainEvent) >=> getEvent) (concat <$> L.list) where
    extractScripts :: Shelley.UTxO.UTxO EmulatorEra -> C.Tx C.BabbageEra -> [ScriptEvent]
    extractScripts utxo tx =
        let bd = C.getTxBody tx
            C.ShelleyTx C.ShelleyBasedEraBabbage ltx = tx
            C.ShelleyTxBody _era txb _scripts (C.TxBodyScriptData _ _ (Alonzo.unRedeemers -> redeemers)) _ _ = bd

            scriptHashes = Alonzo.TxInfo.txscripts utxo ltx

            getScriptHash = \case
                Alonzo.Tx.Spending txIn -> do
                    C.TxOut (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) (C.ShelleyAddress _ cred _ )) _ _ _ <- C.fromShelleyTxOut C.ShelleyBasedEraBabbage <$> Shelley.UTxO.txinLookup txIn utxo
                    case C.fromShelleyPaymentCredential cred of
                        C.PaymentCredentialByScript hsh -> pure hsh
                        _ -> Nothing
                Alonzo.Tx.Minting (Mary.PolicyID sh) -> Just (C.fromShelleyScriptHash sh)

                -- Rewarding and Certifying not supported
                _ -> Nothing
            
            getEvent' (ptr, (_dt, Alonzo.TxInfo.transExUnits -> seExUnits)) = do
                sp <- strictMaybeToMaybe (Alonzo.Tx.rdptrInv txb ptr)
                let seScriptPurpose = show (Alonzo.TxInfo.transScriptPurpose sp)
                seScriptHash <- getScriptHash sp
                seScript <- Map.lookup (C.toShelleyScriptHash seScriptHash) scriptHashes
                let seScriptSize = BS.length (serialize' seScript)
                pure ScriptEvent{seScriptPurpose, seExUnits, seScriptHash, seScript, seScriptSize}

        in mapMaybe getEvent' $ Map.toList redeemers

    getEvent :: ChainEvent -> Maybe [ScriptEvent]
    getEvent = \case
        TxnValidate txi _ (CardanoApiTx (CardanoApiEmulatorEraTx tx))             ->
            extractScripts <$> either (const Nothing) pure (fromPlutusIndex txi) <*> pure tx
        TxnValidationFail txi _ _ (CardanoApiTx (CardanoApiEmulatorEraTx tx)) _ _ ->
            extractScripts <$> either (const Nothing) pure (fromPlutusIndex txi) <*> pure tx
        _ -> Nothing

-- | Unbalanced transactions that are sent to the wallet for balancing
walletTxBalanceEvents :: EmulatorEventFold [UnbalancedTx]
walletTxBalanceEvents = preMapMaybe (preview (eteEvent . walletEvent' . _2 . _TxBalanceLog . _BalancingUnbalancedTx)) L.list

-- | Min lovelace of 'txOut's from adjusted unbalanced transactions for all wallets
walletsAdjustedTxEvents :: EmulatorEventFold [Ada.Ada]
walletsAdjustedTxEvents = (Set.toList . Set.fromList . concat) <$> preMapMaybe (preview (eteEvent . walletEvent' . _2 . _RequestHandlerLog . _AdjustingUnbalancedTx)) L.list

mkTxLogs :: EmulatorEventFold [MkTxLog]
mkTxLogs =
    let getTxLogEvent :: ContractInstanceMsg -> Maybe MkTxLog
        getTxLogEvent (ContractLog vl) = case JSON.fromJSON vl of
            JSON.Error _   -> Nothing
            JSON.Success a -> Just a
        getTxLogEvent _ = Nothing

        flt :: EmulatorEvent -> Maybe MkTxLog
        flt = fmap (view (eteEvent . cilMessage)) . traverse (preview instanceEvent) >=> getTxLogEvent
    in preMapMaybe flt L.list

-- | The state of a contract instance, recovered from the emulator log.
instanceState ::
    forall w s e a effs.
    ( Member (Error EmulatorFoldErr) effs
    , Monoid w
    )
    => Contract w s e a
    -> ContractInstanceTag
    -> EmulatorEventFoldM effs (Maybe (ContractInstanceState w s e a))
instanceState con tag =
    let flt :: EmulatorEvent -> Maybe (Response JSON.Value)
        flt = preview (eteEvent . instanceEvent . filtered ((==) tag . view cilTag) . cilMessage . _HandledRequest)
        decode :: forall effs'. Member (Error EmulatorFoldErr) effs' => EmulatorEvent -> Eff effs' (Maybe (Response PABResp))
        decode e = do
            case flt e of
                Nothing -> pure Nothing
                Just response -> case traverse (JSON.fromJSON @PABResp) response of
                    JSON.Error e'   -> throwError $ InstanceStateJSONDecodingError e' response
                    JSON.Success e' -> pure (Just e')

    in preMapMaybeM decode $ L.generalize $ Fold (\s r -> s >>= addEventInstanceState r) (Just $ emptyInstanceState con) (fmap toInstanceState)

-- | The list of open requests of the contract instance at its latest iteration
instanceRequests ::
    forall w s e a effs.
    ( Member (Error EmulatorFoldErr) effs
    , Monoid w
    )
    => Contract w s e a
    -> ContractInstanceTag
    -> EmulatorEventFoldM effs [Request PABReq]
instanceRequests con = fmap g . instanceState con where
    g = fromMaybe [] . fmap (State.unRequests . _requests . instContractState)

-- | The unbalanced transactions generated by the contract instance.
instanceTransactions ::
    forall w s e a effs.
    ( Member (Error EmulatorFoldErr) effs
    , Monoid w
    )
    => Contract w s e a
    -> ContractInstanceTag
    -> EmulatorEventFoldM effs [UnbalancedTx]
instanceTransactions con = fmap g . instanceState @w @s @e @a @effs con where
    g :: Maybe (ContractInstanceState w s e a) -> [UnbalancedTx]
    g = fromMaybe [] . fmap (mapMaybe (preview _BalanceTxReq . State.rqRequest) . concat . toList . instHandlersHistory)


-- | The reponses received by the contract instance
instanceResponses ::
    forall w s e a effs.
    ( Member (Error EmulatorFoldErr) effs
    , Monoid w
    )
    => Contract w s e a
    -> ContractInstanceTag
    -> EmulatorEventFoldM effs [Response PABResp]
instanceResponses con = fmap (fromMaybe [] . fmap (toList . instEvents)) . instanceState con

-- | Accumulated state of the contract instance
instanceAccumState ::
    forall w s e a effs.
    ( Member (Error EmulatorFoldErr) effs
    , Monoid w
    )
    => Contract w s e a
    -> ContractInstanceTag
    -> EmulatorEventFoldM effs w
instanceAccumState con = fmap (maybe mempty (_observableState . instContractState)) . instanceState con

-- | The log messages produced by the contract instance.
instanceLog :: ContractInstanceTag -> EmulatorEventFold [EmulatorTimeEvent ContractInstanceLog]
instanceLog tag =
    let flt :: EmulatorEvent -> Maybe (EmulatorTimeEvent ContractInstanceLog)
        flt = traverse (preview (instanceEvent . filtered ((==) tag . view cilTag)))
    in preMapMaybe flt L.list

-- | Log and error messages produced by the main (user) thread in the emulator
userLog :: EmulatorEventFold [EmulatorTimeEvent UserThreadMsg]
userLog =
    let flt :: EmulatorEvent -> Maybe (EmulatorTimeEvent UserThreadMsg)
        flt = traverse (preview userThreadEvent)
    in preMapMaybe flt L.list

data Outcome e a =
    Done a
    -- ^ The contract finished without errors and produced a result
    | NotDone
    -- ^ The contract is waiting for more input.
    | Failed e
    -- ^ The contract failed with an error.
    deriving (Eq, Show)

fromResumableResult :: ResumableResult w e i o a -> Outcome e a
fromResumableResult = either Failed (maybe NotDone Done) . _finalState

-- | The final state of the instance
instanceOutcome ::
    forall w s e a effs.
    ( Member (Error EmulatorFoldErr) effs
    , Monoid w
    )
    => Contract w s e a
    -> ContractInstanceTag
    -> EmulatorEventFoldM effs (Outcome e a)
instanceOutcome con =
    fmap (maybe NotDone (fromResumableResult . instContractState)) . instanceState con

-- | Unspent outputs at an address
utxoAtAddress :: Address -> EmulatorEventFold UtxoMap
utxoAtAddress addr =
    preMapMaybe (preview (eteEvent . chainEvent))
    $ Fold (flip step) (AM.addAddress addr mempty) (view (AM.fundsAt addr))
    where
        step = \case
            TxnValidate _ _ txn                  -> AM.updateAddresses (Valid txn)
            TxnValidationFail _ Phase2 _ txn _ _ -> AM.updateAddresses (Invalid txn)
            _                                    -> id

-- | The total value of unspent outputs at an address
valueAtAddress :: Address -> EmulatorEventFold Value
valueAtAddress = fmap (foldMap (txOutValue . snd)) . utxoAtAddress

-- | The funds belonging to a wallet
walletFunds :: Wallet -> EmulatorEventFold Value
walletFunds = valueAtAddress . mockWalletAddress

-- | The fees paid by a wallet
walletFees :: Wallet -> EmulatorEventFold Value
walletFees w = fees <$> walletSubmittedFees <*> validatedTransactions <*> failedTransactions (Just Phase2)
    where
        fees submitted txsV txsF =
            findFees (\(_, i, _) -> i) (\(_, _, tx) -> getCardanoTxFee tx) submitted txsV
            <>
            findFees (\(_, i, _, _, _) -> i) (\(_, _, _, _, collateral) -> collateral) submitted txsF
        findFees getId getFees submitted = foldMap (\t -> if Map.member (getId t) submitted then getFees t else mempty)
        walletSubmittedFees = L.handles (eteEvent . walletClientEvent w . _TxSubmit) L.map

-- | Annotate the transactions that were validated by the node
annotatedBlockchain :: EmulatorEventFold [[AnnotatedTx]]
annotatedBlockchain =
    preMapMaybe (preview (eteEvent . chainEvent))
    $ Fold Rollup.handleChainEvent Rollup.initialState Rollup.getAnnotatedTransactions

-- | All chain events emitted by the node
chainEvents :: EmulatorEventFold [ChainEvent]
chainEvents = preMapMaybe (preview (eteEvent . chainEvent)) L.list

-- | All transactions that happened during the simulation
blockchain :: EmulatorEventFold [Block]
blockchain =
    let step (currentBlock, otherBlocks) = \case
            SlotAdd _                            -> ([], currentBlock : otherBlocks)
            TxnValidate _ _ txn                  -> (Valid txn : currentBlock, otherBlocks)
            TxnValidationFail _ Phase1 _ _   _ _ -> (currentBlock, otherBlocks)
            TxnValidationFail _ Phase2 _ txn _ _ -> (Invalid txn : currentBlock, otherBlocks)
        initial = ([], [])
        extract (currentBlock, otherBlocks) =
            (currentBlock : otherBlocks)
    in preMapMaybe (preview (eteEvent . chainEvent))
        $ Fold step initial extract

-- | The list of all emulator events
emulatorLog :: EmulatorEventFold [EmulatorEvent]
emulatorLog = L.list

-- | Pretty-print each element into a new line.
renderLines :: forall a. Pretty a => Fold a Text
renderLines =
    let rnd = renderStrict . layoutPretty defaultLayoutOptions in
    dimap pretty (rnd . vsep) L.list

-- | An effectful 'Data.Maybe.mapMaybe' for 'FoldM'.
preMapMaybeM ::
    Monad m
    => (a -> m (Maybe b))
    -> FoldM m b r
    -> FoldM m a r
preMapMaybeM f (FoldM step begin done) = FoldM step' begin done where
    step' x a = do
        result <- f a
        case result of
            Nothing -> pure x
            Just a' -> step x a'

-- | 'Data.Maybe.mapMaybe' for 'Fold'.
preMapMaybe :: (a -> Maybe b) -> Fold b r -> Fold a r
preMapMaybe f (Fold step begin done) = Fold step' begin done where
    step' x a = case f a of
        Nothing -> x
        Just b  -> step x b

-- | Effectfully map the result of a 'FoldM'
postMapM ::
    Monad m
    => (b -> m c)
    -> FoldM m a b
    -> FoldM m a c
postMapM f (FoldM step begin done) = FoldM step begin (done >=> f)

data EmulatorFoldErr =
    InstanceStateJSONDecodingError String (Response JSON.Value)
    deriving stock (Eq, Ord, Show)

-- | A human-readable explanation of the error, to be included in the logs.
describeError :: EmulatorFoldErr -> String
describeError = \case
    InstanceStateJSONDecodingError _ _ -> unwords $
        [ "Failed to decode a 'Response JSON.Value'."
        , "The event is probably for a different 'Contract'."
        , "This is often caused by having multiple contract instances share the same 'ContractInstanceTag' (for example, when  using 'activateContractWallet' repeatedly on the same wallet)."
        , "To fix this, use 'activateContract' with a unique 'ContractInstanceTag' per instance."
        ]
