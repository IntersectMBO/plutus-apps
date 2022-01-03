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
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Wallet.Emulator.Wallet where

import Cardano.Wallet.Primitive.Types qualified as Cardano.Wallet
import Control.Lens (makeLenses, makePrisms, over, view, (&), (.~), (^.))
import Control.Monad (foldM)
import Control.Monad.Freer (Eff, Member, Members, interpret, type (~>))
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logInfo, logWarn)
import Control.Monad.Freer.State (State, get, gets, put)
import Control.Monad.Freer.TH (makeEffect)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), ToJSONKey)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (bimap, first, second)
import Data.Default (Default (def))
import Data.Foldable (Foldable (fold), find, foldl', for_)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe)
import Data.OpenApi.Schema qualified as OpenApi
import Data.Semigroup (Sum (Sum, getSum))
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Text.Class (fromText, toText)
import GHC.Generics (Generic)
import Ledger (Address (addressCredential), CardanoTx, ChainIndexTxOut,
               PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey),
               PaymentPubKey (PaymentPubKey, unPaymentPubKey),
               PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash), PubKeyHash,
               ScriptValidationEvent (sveScript), StakePubKey, Tx (txFee, txMint), TxIn (TxIn, txInRef), TxOut,
               TxOutRef, UtxoIndex (UtxoIndex, getIndex), ValidationCtx (ValidationCtx), ValidatorHash, Value)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet (MockWallet, WalletNumber)
import Ledger.CardanoWallet qualified as CW
import Ledger.Constraints.OffChain (UnbalancedTx (UnbalancedTx, unBalancedTxTx, unBalancedTxUtxoIndex))
import Ledger.Constraints.OffChain qualified as U
import Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))
import Ledger.Fee (FeeConfig, calcFees)
import Ledger.TimeSlot (SlotConfig, posixTimeRangeToContainedSlotRange)
import Ledger.Tx qualified as Tx
import Ledger.Value qualified as Value
import Plutus.ChainIndex (PageQuery)
import Plutus.ChainIndex qualified as ChainIndex
import Plutus.ChainIndex.Api (UtxosResponse (page))
import Plutus.ChainIndex.Emulator (ChainIndexEmulatorState, ChainIndexQueryEffect)
import Plutus.Contract (WalletAPIError)
import Plutus.Contract.Checkpoint (CheckpointLogMsg)
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty (pretty))
import Servant.API (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))
import Wallet.API qualified as WAPI
import Wallet.Effects (NodeClientEffect,
                       WalletEffect (BalanceTx, OwnPaymentPubKeyHash, SubmitTxn, TotalFunds, WalletAddSignature, YieldUnbalancedTx),
                       publishTx)
import Wallet.Emulator.Chain (ChainState (_index))
import Wallet.Emulator.LogMessages (RequestHandlerLogMsg,
                                    TxBalanceMsg (AddingCollateralInputsFor, AddingInputsFor, AddingPublicKeyOutputFor, BalancingUnbalancedTx, FinishedBalancing, NoCollateralInputsAdded, NoInputsAdded, NoOutputsAdded, SubmittingTx, ValidationFailed))
import Wallet.Emulator.NodeClient (NodeClientState, emptyNodeClientState)

newtype SigningProcess = SigningProcess {
    unSigningProcess :: forall effs. (Member (Error WAPI.WalletAPIError) effs) => [PaymentPubKeyHash] -> Tx -> Eff effs Tx
}

instance Show SigningProcess where
    show = const "SigningProcess <...>"

-- | A wallet identifier
newtype Wallet = Wallet { getWalletId :: WalletId }
    deriving (Eq, Ord, Generic)
    deriving newtype (ToHttpApiData, FromHttpApiData)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey)

toMockWallet :: MockWallet -> Wallet
toMockWallet = Wallet . WalletId . CW.mwWalletId

knownWallets :: [Wallet]
knownWallets = toMockWallet <$> CW.knownMockWallets

knownWallet :: Integer -> Wallet
knownWallet = fromWalletNumber . CW.WalletNumber

fromWalletNumber :: WalletNumber -> Wallet
fromWalletNumber = toMockWallet . CW.fromWalletNumber

instance Show Wallet where
    showsPrec p (Wallet i) = showParen (p > 9) $ showString "Wallet " . shows i

instance Pretty Wallet where
    pretty (Wallet i) = "W" <> pretty (T.take 7 $ toBase16 i)

deriving anyclass instance OpenApi.ToSchema Wallet
deriving anyclass instance OpenApi.ToSchema Cardano.Wallet.WalletId

newtype WalletId = WalletId { unWalletId :: Cardano.Wallet.WalletId }
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSONKey)

instance Show WalletId where
    show = T.unpack . toBase16
instance ToJSON WalletId where
    toJSON = Aeson.String . toBase16
instance FromJSON WalletId where
    parseJSON = Aeson.withText "WalletId" (either fail pure . fromBase16)
instance ToHttpApiData WalletId where
    toUrlPiece = toBase16
instance FromHttpApiData WalletId where
    parseUrlPiece = first T.pack . fromBase16
deriving anyclass instance OpenApi.ToSchema WalletId

toBase16 :: WalletId -> T.Text
toBase16 = toText . unWalletId

fromBase16 :: T.Text -> Either String WalletId
fromBase16 s = bimap show WalletId (fromText s)

-- | The 'MockWallet' whose ID is the given wallet ID (if it exists)
walletToMockWallet :: Wallet -> Maybe MockWallet
walletToMockWallet (Wallet wid) = find ((==) wid . WalletId . CW.mwWalletId) CW.knownMockWallets

-- | The public key of a mock wallet.  (Fails if the wallet is not a mock wallet).
mockWalletPaymentPubKey :: Wallet -> PaymentPubKey
mockWalletPaymentPubKey w =
    CW.paymentPubKey
        $ fromMaybe (error $ "Wallet.Emulator.Wallet.walletPubKey: Wallet "
                          <> show w
                          <> " is not a mock wallet")
        $ walletToMockWallet w

-- | The payment public key hash of a mock wallet.  (Fails if the wallet is not a mock wallet).
mockWalletPaymentPubKeyHash :: Wallet -> PaymentPubKeyHash
mockWalletPaymentPubKeyHash =
    PaymentPubKeyHash
  . Ledger.pubKeyHash
  . unPaymentPubKey
  . mockWalletPaymentPubKey

-- | Get the address of a mock wallet. (Fails if the wallet is not a mock wallet).
mockWalletAddress :: Wallet -> Address
mockWalletAddress w = Ledger.pubKeyHashAddress (mockWalletPaymentPubKeyHash w) Nothing

data WalletEvent =
    GenericLog T.Text
    | CheckpointLog CheckpointLogMsg
    | RequestHandlerLog RequestHandlerLogMsg
    | TxBalanceLog TxBalanceMsg
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty WalletEvent where
    pretty = \case
        GenericLog msg        -> pretty msg
        CheckpointLog msg     -> pretty msg
        RequestHandlerLog msg -> pretty msg
        TxBalanceLog msg      -> pretty msg

makePrisms ''WalletEvent

-- | The state used by the mock wallet environment.
data WalletState = WalletState {
    _mockWallet              :: MockWallet, -- ^ mock wallet with the user's private key
    _nodeClient              :: NodeClientState,
    _chainIndexEmulatorState :: ChainIndexEmulatorState,
    _signingProcess          :: SigningProcess
    } deriving Show

makeLenses ''WalletState

ownPaymentPrivateKey :: WalletState -> PaymentPrivateKey
ownPaymentPrivateKey = CW.paymentPrivateKey . _mockWallet

ownPaymentPublicKey :: WalletState -> PaymentPubKey
ownPaymentPublicKey = CW.paymentPubKey . _mockWallet

-- | Get the user's own payment public-key address.
ownAddress :: WalletState -> Address
ownAddress = flip Ledger.pubKeyAddress Nothing
           . PaymentPubKey
           . Ledger.toPublicKey
           . unPaymentPrivateKey
           . ownPaymentPrivateKey

-- | An empty wallet using the given private key.
-- for that wallet as the sole watched address.
fromMockWallet :: MockWallet -> WalletState
fromMockWallet mw = WalletState mw emptyNodeClientState mempty sp where
    sp = signWithPrivateKey (CW.paymentPrivateKey mw)

-- | Empty wallet state for an emulator 'Wallet'. Returns 'Nothing' if the wallet
--   is not known in the emulator.
emptyWalletState :: Wallet -> Maybe WalletState
emptyWalletState = fmap fromMockWallet . walletToMockWallet

handleWallet ::
    ( Member (Error WalletAPIError) effs
    , Member NodeClientEffect effs
    , Member ChainIndexQueryEffect effs
    , Member (State WalletState) effs
    , Member (LogMsg TxBalanceMsg) effs
    )
    => FeeConfig
    -> WalletEffect ~> Eff effs
handleWallet feeCfg = \case
    SubmitTxn tx          -> submitTxnH tx
    OwnPaymentPubKeyHash  -> ownPaymentPubKeyHashH
    BalanceTx utx         -> balanceTxH utx
    WalletAddSignature tx -> walletAddSignatureH tx
    TotalFunds            -> totalFundsH
    YieldUnbalancedTx utx -> yieldUnbalancedTxH utx

  where
    submitTxnH :: (Member NodeClientEffect effs, Member (LogMsg TxBalanceMsg) effs) => CardanoTx -> Eff effs ()
    submitTxnH (Left _) = error "Wallet.Emulator.Wallet.handleWallet: Expecting a mock tx, not an Alonzo tx when submitting it."
    submitTxnH (Right tx) = do
        logInfo $ SubmittingTx tx
        publishTx tx

    ownPaymentPubKeyHashH :: (Member (State WalletState) effs) => Eff effs PaymentPubKeyHash
    ownPaymentPubKeyHashH = gets (CW.paymentPubKeyHash . _mockWallet)

    balanceTxH ::
        ( Member NodeClientEffect effs
        , Member ChainIndexQueryEffect effs
        , Member (State WalletState) effs
        , Member (LogMsg TxBalanceMsg) effs
        )
        => UnbalancedTx
        -> Eff effs (Either WalletAPIError CardanoTx)
    balanceTxH utx' = runError $ do
        logInfo $ BalancingUnbalancedTx utx'
        utxo <- get >>= ownOutputs
        slotConfig <- WAPI.getClientSlotConfig
        let validitySlotRange = posixTimeRangeToContainedSlotRange slotConfig (utx' ^. U.validityTimeRange)
        let utx = utx' & U.tx . Ledger.validRange .~ validitySlotRange
        utxWithFees <- validateTxAndAddFees feeCfg slotConfig utxo utx
        -- balance to add fees
        tx' <- handleBalanceTx utxo (utx & U.tx . Ledger.fee .~ (utxWithFees ^. U.tx . Ledger.fee))
        tx'' <- handleAddSignature tx'
        logInfo $ FinishedBalancing tx''
        pure $ Right tx''

    walletAddSignatureH :: (Member (State WalletState) effs) => CardanoTx -> Eff effs CardanoTx
    walletAddSignatureH (Left _) = error "Wallet.Emulator.Wallet.handleWallet: Expecting a mock tx, not an Alonzo tx when adding a signature."
    walletAddSignatureH (Right tx) = Right <$> handleAddSignature tx

    totalFundsH :: (Member (State WalletState) effs, Member ChainIndexQueryEffect effs) => Eff effs Value
    totalFundsH = foldMap (view Ledger.ciTxOutValue) <$> (get >>= ownOutputs)

    yieldUnbalancedTxH ::
        ( Member (Error WalletAPIError) effs
        , Member NodeClientEffect effs
        , Member ChainIndexQueryEffect effs
        , Member (State WalletState) effs
        , Member (LogMsg TxBalanceMsg) effs
        )
        => UnbalancedTx
        -> Eff effs ()
    yieldUnbalancedTxH utx = do
        balancedTxM <- balanceTxH utx
        case balancedTxM of
            Left err         -> throwError err
            Right balancedTx -> walletAddSignatureH balancedTx >>= submitTxnH

handleAddSignature ::
    Member (State WalletState) effs
    => Tx
    -> Eff effs Tx
handleAddSignature tx = do
    (PaymentPrivateKey privKey) <- gets ownPaymentPrivateKey
    pure (Ledger.addSignature' privKey tx)

ownOutputs :: forall effs.
    ( Member ChainIndexQueryEffect effs
    )
    => WalletState
    -> Eff effs (Map.Map TxOutRef ChainIndexTxOut)
ownOutputs WalletState{_mockWallet} = do
    refs <- allUtxoSet (Just def)
    Map.fromList . catMaybes <$> traverse txOutRefTxOutFromRef refs
  where
    cred :: Credential
    cred = PubKeyCredential (unPaymentPubKeyHash $ CW.paymentPubKeyHash _mockWallet)

    -- Accumulate all unspent 'TxOutRef's from the resulting pages.
    allUtxoSet :: Maybe (PageQuery TxOutRef) -> Eff effs [TxOutRef]
    allUtxoSet Nothing = pure []
    allUtxoSet (Just pq) = do
      refPage <- page <$> ChainIndex.utxoSetAtAddress pq cred
      nextItems <- allUtxoSet (ChainIndex.nextPageQuery refPage)
      pure $ ChainIndex.pageItems refPage ++ nextItems

    txOutRefTxOutFromRef :: TxOutRef -> Eff effs (Maybe (TxOutRef, ChainIndexTxOut))
    txOutRefTxOutFromRef ref = fmap (ref,) <$> ChainIndex.txOutFromRef ref

validateTxAndAddFees ::
    ( Member (Error WAPI.WalletAPIError) effs
    , Member ChainIndexQueryEffect effs
    , Member (LogMsg TxBalanceMsg) effs
    , Member (State WalletState) effs
    )
    => FeeConfig
    -> SlotConfig
    -> Map.Map TxOutRef ChainIndexTxOut -- ^ The current wallet's unspent transaction outputs.
    -> UnbalancedTx
    -> Eff effs UnbalancedTx
validateTxAndAddFees feeCfg slotCfg ownTxOuts utx = do
    -- Balance and sign just for validation
    tx <- handleBalanceTx ownTxOuts utx
    signedTx <- handleAddSignature tx
    let utxoIndex        = Ledger.UtxoIndex $ fmap Ledger.toTxOut $ (U.fromScriptOutput <$> unBalancedTxUtxoIndex utx) <> ownTxOuts
        ((e, _), events) = Ledger.runValidation (Ledger.validateTransactionOffChain signedTx) (Ledger.ValidationCtx utxoIndex slotCfg)
    for_ e $ \(phase, ve) -> do
        logWarn $ ValidationFailed phase (Ledger.txId tx) tx ve events
        throwError $ WAPI.ValidationError ve
    let scriptsSize = getSum $ foldMap (Sum . Ledger.scriptSize . Ledger.sveScript) events
        theFee = Ada.toValue $ calcFees feeCfg scriptsSize -- TODO: use protocol parameters
    pure $ utx{ unBalancedTxTx = (unBalancedTxTx utx){ txFee = theFee }}

lookupValue ::
    ( Member (Error WAPI.WalletAPIError) effs
    , Member ChainIndexQueryEffect effs
    )
    => Tx.TxIn
    -> Eff effs Value
lookupValue outputRef@TxIn {txInRef} = do
    txoutMaybe <- ChainIndex.txOutFromRef txInRef
    case txoutMaybe of
        Just txout -> pure $ view Ledger.ciTxOutValue txout
        Nothing ->
            WAPI.throwOtherError $ "Unable to find TxOut for " <> fromString (show outputRef)

-- | Balance an unbalanced transaction by adding missing inputs and outputs
handleBalanceTx ::
    forall effs.
    ( Member (State WalletState) effs
    , Member ChainIndexQueryEffect effs
    , Member (Error WAPI.WalletAPIError) effs
    , Member (LogMsg TxBalanceMsg) effs
    )
    => Map.Map TxOutRef ChainIndexTxOut -- ^ The current wallet's unspent transaction outputs.
    -> UnbalancedTx
    -> Eff effs Tx
handleBalanceTx utxo UnbalancedTx{unBalancedTxTx} = do
    let filteredUnbalancedTxTx = removeEmptyOutputs unBalancedTxTx
    let txInputs = Set.toList $ Tx.txInputs filteredUnbalancedTxTx
    ownPaymentPubKey <- gets ownPaymentPublicKey
    let ownStakePubKey = Nothing
    inputValues <- traverse lookupValue (Set.toList $ Tx.txInputs filteredUnbalancedTxTx)
    collateral  <- traverse lookupValue (Set.toList $ Tx.txCollateral filteredUnbalancedTxTx)
    let fees = txFee filteredUnbalancedTxTx
        left = txMint filteredUnbalancedTxTx <> fold inputValues
        right = fees <> foldMap (view Tx.outValue) (filteredUnbalancedTxTx ^. Tx.outputs)
        remainingFees = fees PlutusTx.- fold collateral -- TODO: add collateralPercent
        balance = left PlutusTx.- right

    (neg, pos) <- adjustBalanceWithMissingLovelace utxo ownPaymentPubKey filteredUnbalancedTxTx $ Value.split balance

    tx' <- if Value.isZero pos
           then do
               logDebug NoOutputsAdded
               pure filteredUnbalancedTxTx
           else do
                logDebug $ AddingPublicKeyOutputFor pos
                pure $ addOutputs ownPaymentPubKey ownStakePubKey pos filteredUnbalancedTxTx

    tx'' <- if Value.isZero neg
            then do
                logDebug NoInputsAdded
                pure tx'
            else do
                logDebug $ AddingInputsFor neg
                -- filter out inputs from utxo that are already in unBalancedTx
                let inputsOutRefs = map Tx.txInRef txInputs
                    filteredUtxo = flip Map.filterWithKey utxo $ \txOutRef _ ->
                        txOutRef `notElem` inputsOutRefs
                addInputs filteredUtxo ownPaymentPubKey ownStakePubKey neg tx'

    if remainingFees `Value.leq` PlutusTx.zero
    then do
        logDebug NoCollateralInputsAdded
        pure tx''
    else do
        logDebug $ AddingCollateralInputsFor remainingFees
        addCollateral utxo remainingFees tx''

-- | Adjust the left and right balance of an unbalanced 'Tx' with the missing
-- lovelace considering the minimum lovelace per transaction output constraint
-- from the Cardano blockchain.
adjustBalanceWithMissingLovelace ::
    forall effs.
    ( Member ChainIndexQueryEffect effs
    , Member (Error WAPI.WalletAPIError) effs
    )
    => Map.Map TxOutRef ChainIndexTxOut -- ^ The current wallet's unspent transaction outputs.
    -> PaymentPubKey -- ^ Wallet's public key
    -> Tx -- ^ An unbalanced tx
    -> (Value, Value) -- ^ The unbalanced tx's left and right balance.
    -> Eff effs (Value, Value) -- ^ New left and right balance.
adjustBalanceWithMissingLovelace utxo ownPaymentPubKey unBalancedTx (neg, pos) = do
    -- Find the tx's input value which refer to the current wallet's address.
    let ownPkh = Ledger.pubKeyHash $ unPaymentPubKey ownPaymentPubKey
    let pkhOfUnspentTxIn TxIn { txInRef } =
            (Ledger.toPubKeyHash . view Ledger.ciTxOutAddress) =<< Map.lookup txInRef utxo
    let ownTxInputs = filter (\txIn -> Just ownPkh == pkhOfUnspentTxIn txIn)
                             (Set.toList $ Tx.txInputs unBalancedTx)
    ownInputValues <- traverse lookupValue ownTxInputs

    -- When minting a token, there will be eventually a transaction output
    -- with that token. However, it is important to make sure that we can add
    -- the minimum lovelace alongside that token value in order to satisfy the
    -- Cardano blockchain constraint. Therefore, if there is a minted token, we
    -- add the missing lovelace on the positive balance.
    let txMintMaybe = if Value.isZero (txMint unBalancedTx) then Nothing else Just (txMint unBalancedTx)
        -- If the tx mints a token, then we find the missing lovelace considering the wallet's inputs.
        -- Ex. If we mint token A, with no wallet inputs, then the missing lovelace is 'Ledger.minAdaTxOut'.
        missingLovelaceForMintValue =
          maybe 0
                (\mintValue -> max 0 (Ledger.minAdaTxOut - Ada.fromValue (mintValue <> fold ownInputValues)))
                txMintMaybe
        -- We add to the missing lovelace from the minting to the positive balance.
        posWithMintAda = pos <> Ada.toValue missingLovelaceForMintValue
        -- Now, we find the missing lovelace from the new positive balance. If
        -- a token was minted, this should always be 0. But if no token was
        -- minted, and if the positive balance is > 0 and < 'Ledger.minAdaTxOut',
        -- then we adjust it to the minimum Ada.
        missingLovelaceFromPosValue =
          if Ada.isZero (Ada.fromValue posWithMintAda) || Ada.fromValue posWithMintAda >= Ledger.minAdaTxOut
            then 0
            else max 0 (Ledger.minAdaTxOut - Ada.fromValue posWithMintAda)
        -- We calculate the final negative and positive balances
        newPos = pos <> Ada.toValue missingLovelaceForMintValue <> Ada.toValue missingLovelaceFromPosValue
        newNeg = neg <> Ada.toValue missingLovelaceForMintValue <> Ada.toValue missingLovelaceFromPosValue

    pure (newNeg, newPos)

addOutputs :: PaymentPubKey -> Maybe StakePubKey -> Value -> Tx -> Tx
addOutputs pk sk vl tx = tx & over Tx.outputs (pko :) where
    pko = Tx.pubKeyTxOut vl pk sk

addCollateral
    :: ( Member (Error WAPI.WalletAPIError) effs
       )
    => Map.Map TxOutRef ChainIndexTxOut -- ^ The current wallet's unspent transaction outputs.
    -> Value
    -> Tx
    -> Eff effs Tx
addCollateral mp vl tx = do
    (spend, _) <- selectCoin (second (view Ledger.ciTxOutValue) <$> Map.toList mp) vl
    let addTxCollateral =
            let ins = Set.fromList (Tx.pubKeyTxIn . fst <$> spend)
            in over Tx.collateralInputs (Set.union ins)
    pure $ tx & addTxCollateral

-- | @addInputs mp pk vl tx@ selects transaction outputs worth at least
--   @vl@ from the UTXO map @mp@ and adds them as inputs to @tx@. A public
--   key output for @pk@ is added containing any leftover change.
addInputs
    :: ( Member (Error WAPI.WalletAPIError) effs
       )
    => Map.Map TxOutRef ChainIndexTxOut -- ^ The current wallet's unspent transaction outputs.
    -> PaymentPubKey
    -> Maybe StakePubKey
    -> Value
    -> Tx
    -> Eff effs Tx
addInputs mp pk sk vl tx = do
    (spend, change) <- selectCoin (second (view Ledger.ciTxOutValue) <$> Map.toList mp) vl
    let

        addTxIns  =
            let ins = Set.fromList (Tx.pubKeyTxIn . fst <$> spend)
            in over Tx.inputs (Set.union ins)

        addTxOuts = if Value.isZero change
                    then id
                    else addOutputs pk sk change

    pure $ tx & addTxOuts & addTxIns

-- Make a transaction output from a positive value.
mkChangeOutput :: PaymentPubKey -> Maybe StakePubKey -> Value -> Maybe TxOut
mkChangeOutput pubK sk v =
    if Value.isZero v then Nothing else Just (Ledger.pubKeyTxOut v pubK sk)

-- | Given a set of @a@s with coin values, and a target value, select a number
-- of @a@ such that their total value is greater than or equal to the target.
selectCoin ::
    ( Member (Error WAPI.WalletAPIError) effs
    )
    => [(a, Value)]
    -> Value
    -> Eff effs ([(a, Value)], Value)
selectCoin fnds vl =
    let
        total = foldMap snd fnds
        err   = throwError
                $ WAPI.InsufficientFunds
                $ T.unwords
                    [ "Total:", T.pack $ show total
                    , "expected:", T.pack $ show vl ]
    -- Values are in a partial order: what we want to check is that the
    -- total available funds are bigger than (or equal to) the required value.
    -- It is *not* correct to replace this condition with 'total `Value.lt` vl' -
    -- consider what happens if the amounts are incomparable.
    in  if not (total `Value.geq` vl)
        then err
        else
            let
                -- Given the funds of a wallet, we take enough just enough such
                -- that it's geq the target value, and if the resulting change
                -- is not between 0 and the minimum Ada per tx output.
                isTotalValueEnough totalVal =
                  let adaChange = Ada.fromValue totalVal PlutusTx.- Ada.fromValue vl
                   in vl `Value.leq` totalVal && (adaChange == 0 || adaChange >= Ledger.minAdaTxOut)
                fundsWithTotal = zip fnds (drop 1 $ scanl (<>) mempty $ fmap snd fnds)
                fundsToSpend   = takeUntil (isTotalValueEnough . snd) fundsWithTotal
                totalSpent     = maybe PlutusTx.zero snd $ listToMaybe $ reverse fundsToSpend
                change         = totalSpent PlutusTx.- vl
                changeAda      = Ada.fromValue change
             -- Make sure that the change is not less than the minimum amount
             -- of lovelace per tx output.
             in if changeAda > 0 && changeAda < Ledger.minAdaTxOut
                   then throwError $ WAPI.ChangeHasLessThanNAda change Ledger.minAdaTxOut
                   else pure (fst <$> fundsToSpend, change)

-- | Removes transaction outputs with empty datum and empty value.
removeEmptyOutputs :: Tx -> Tx
removeEmptyOutputs tx = tx & over Tx.outputs (filter (not . isEmpty')) where
    isEmpty' Tx.TxOut{Tx.txOutValue, Tx.txOutDatumHash} =
        null (Value.flattenValue txOutValue) && isNothing txOutDatumHash

-- | Take elements from a list until the predicate is satisfied.
-- 'takeUntil' @p@ includes the first element for wich @p@ is true
-- (unlike @takeWhile (not . p)@).
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []       = []
takeUntil p (x:xs)
    | p x            = [x]
    | otherwise      = x : takeUntil p xs

-- | The default signing process is 'signWallet'
defaultSigningProcess :: MockWallet -> SigningProcess
defaultSigningProcess = signWallet

signWithPrivateKey :: PaymentPrivateKey -> SigningProcess
signWithPrivateKey pk = SigningProcess $
    \pks tx -> foldM (signTxWithPrivateKey pk) tx pks

-- | Sign the transaction by calling 'WAPI.signTxnWithKey' (throwing a
--   'PrivateKeyNotFound' error if called with a key other than the
--   wallet's private key)
signWallet :: MockWallet -> SigningProcess
signWallet wllt = SigningProcess $
    \pks tx -> foldM (signTxnWithKey wllt) tx pks

-- | Sign the transaction with the private key of the mock wallet.
signTxnWithKey
    :: (Member (Error WAPI.WalletAPIError) r)
    => MockWallet
    -> Tx
    -> PaymentPubKeyHash
    -> Eff r Tx
signTxnWithKey mw = signTxWithPrivateKey (CW.paymentPrivateKey mw)

-- | Sign the transaction with the private key, if the hash is that of the
--   private key.
signTxWithPrivateKey
    :: (Member (Error WAPI.WalletAPIError) r)
    => PaymentPrivateKey
    -> Tx
    -> PaymentPubKeyHash
    -> Eff r Tx
signTxWithPrivateKey (PaymentPrivateKey pk) tx pkh@(PaymentPubKeyHash pubK) = do
    let ownPaymentPubKey = Ledger.toPublicKey pk
    if Ledger.pubKeyHash ownPaymentPubKey == pubK
    then pure (Ledger.addSignature' pk tx)
    else throwError (WAPI.PaymentPrivateKeyNotFound pkh)

-- | Sign the transaction with the given private keys,
--   ignoring the list of public keys that the 'SigningProcess' is passed.
signPrivateKeys :: [PaymentPrivateKey] -> SigningProcess
signPrivateKeys signingKeys = SigningProcess $ \_ tx ->
    pure (foldr (Ledger.addSignature' . unPaymentPrivateKey) tx signingKeys)

data SigningProcessControlEffect r where
    SetSigningProcess :: SigningProcess -> SigningProcessControlEffect ()
makeEffect ''SigningProcessControlEffect

type SigningProcessEffs = '[State SigningProcess, Error WAPI.WalletAPIError]

handleSigningProcessControl :: (Members SigningProcessEffs effs) => Eff (SigningProcessControlEffect ': effs) ~> Eff effs
handleSigningProcessControl = interpret $ \case
    SetSigningProcess proc -> put proc

-- | An Entity is a thing that can hold 'Value'. Used in the 'balances'
-- function to compute who holds for a given chain state and set of wallets.
data Entity
  = WalletEntity Wallet
  | PubKeyHashEntity PubKeyHash
  | ScriptEntity ValidatorHash
  deriving (Eq, Ord)

instance Show Entity where
  show (WalletEntity w)     = show w
  show (ScriptEntity h)     = "Script " <> show h
  show (PubKeyHashEntity h) = "PubKeyHash " <> show h

type WalletSet = Map.Map Wallet WalletState

-- | Pick out all the public keys from the set of wallets and map them back to
-- their corresponding wallets.
walletPaymentPubKeyHashes :: WalletSet -> Map.Map PaymentPubKeyHash Wallet
walletPaymentPubKeyHashes = foldl' f Map.empty . Map.toList
  where
    f m (w, ws) = Map.insert (CW.paymentPubKeyHash $ _mockWallet ws) w m

-- | For a set of wallets, convert them into a map of value: entity,
-- where entity is one of 'Entity'.
balances :: ChainState -> WalletSet -> Map.Map Entity Value
balances state wallets = foldl' f Map.empty . getIndex . _index $ state
  where
    toEntity :: Address -> Entity
    toEntity a =
        case addressCredential a of
            PubKeyCredential h ->
                case Map.lookup (PaymentPubKeyHash h) ws of
                    Nothing -> PubKeyHashEntity h
                    Just w  -> WalletEntity w
            ScriptCredential h -> ScriptEntity h

    ws :: Map.Map PaymentPubKeyHash Wallet
    ws = walletPaymentPubKeyHashes wallets

    f m o = Map.insertWith (<>) (toEntity $ Ledger.txOutAddress o) (Ledger.txOutValue o) m
