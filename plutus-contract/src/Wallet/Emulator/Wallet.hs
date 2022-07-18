{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
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
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Wallet.Emulator.Wallet where

import Cardano.Api.Shelley (makeSignedTransaction, protocolParamCollateralPercent)
import Cardano.Wallet.Primitive.Types qualified as Cardano.Wallet
import Control.Lens (makeLenses, makePrisms, over, view, (&), (.~), (^.))
import Control.Monad (foldM, (<=<))
import Control.Monad.Freer (Eff, Member, Members, interpret, type (~>))
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logInfo, logWarn)
import Control.Monad.Freer.State (State, get, gets, put)
import Control.Monad.Freer.TH (makeEffect)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), ToJSONKey)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (bimap, first, second)
import Data.Data (Data)
import Data.Default (Default (def))
import Data.Foldable (Foldable (fold), find, foldl')
import Data.List (sortOn, (\\))
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe)
import Data.OpenApi.Schema qualified as OpenApi
import Data.Ord (Down (Down))
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Text.Class (fromText, toText)
import GHC.Generics (Generic)
import Ledger (Address (addressCredential), CardanoTx, ChainIndexTxOut, Params (..),
               PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey), PaymentPubKey,
               PaymentPubKeyHash (PaymentPubKeyHash), PrivateKey, PubKeyHash, SomeCardanoApiTx, Tx (txFee, txMint),
               TxIn (TxIn, txInRef), TxOut (..), TxOutRef, UtxoIndex (..), Value)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet (MockWallet, WalletNumber)
import Ledger.CardanoWallet qualified as CW
import Ledger.Constraints.OffChain (UnbalancedTx)
import Ledger.Constraints.OffChain qualified as U
import Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))
import Ledger.Fee (estimateTransactionFee, makeAutoBalancedTransaction)
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI (makeTransactionBody)
import Ledger.Validation (addSignature, fromPlutusIndex, fromPlutusTx, getRequiredSigners)
import Ledger.Value qualified as Value
import Plutus.ChainIndex (PageQuery)
import Plutus.ChainIndex qualified as ChainIndex
import Plutus.ChainIndex.Api (UtxosResponse (page))
import Plutus.ChainIndex.Emulator (ChainIndexEmulatorState, ChainIndexQueryEffect)
import Plutus.Contract (WalletAPIError)
import Plutus.Contract.Checkpoint (CheckpointLogMsg)
import Plutus.Contract.Wallet (finalize)
import Plutus.V1.Ledger.Api (ValidatorHash)
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter (Pretty (pretty))
import Servant.API (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))
import Wallet.API qualified as WAPI

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Wallet.Effects (NodeClientEffect,
                       WalletEffect (BalanceTx, OwnAddresses, SubmitTxn, TotalFunds, WalletAddSignature, YieldUnbalancedTx),
                       publishTx)
import Wallet.Emulator.Chain (ChainState (_index))
import Wallet.Emulator.LogMessages (RequestHandlerLogMsg,
                                    TxBalanceMsg (AddingCollateralInputsFor, AddingInputsFor, AddingPublicKeyOutputFor, BalancingUnbalancedTx, FinishedBalancing, NoCollateralInputsAdded, NoInputsAdded, NoOutputsAdded, SigningTx, SubmittingTx, ValidationFailed))
import Wallet.Emulator.NodeClient (NodeClientState, emptyNodeClientState)

newtype SigningProcess = SigningProcess {
    unSigningProcess :: forall effs. (Member (Error WAPI.WalletAPIError) effs) => [PaymentPubKeyHash] -> CardanoTx -> Eff effs CardanoTx
}

instance Show SigningProcess where
    show = const "SigningProcess <...>"

-- | A wallet identifier
data Wallet = Wallet { prettyWalletName :: Maybe String , getWalletId :: WalletId }
    deriving (Generic, Data)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey)

instance Eq Wallet where
  w == w' = getWalletId w == getWalletId w'

instance Ord Wallet where
  compare w w' = compare (getWalletId w) (getWalletId w')

instance ToHttpApiData Wallet where
  toUrlPiece = toUrlPiece . getWalletId

instance FromHttpApiData Wallet where
  parseUrlPiece = pure . Wallet Nothing <=< parseUrlPiece

toMockWallet :: MockWallet -> Wallet
toMockWallet mw =
  Wallet (CW.mwPrintAs mw)
  . WalletId
  . Cardano.Wallet.WalletId
  . CW.mwWalletId $ mw

knownWallets :: [Wallet]
knownWallets = toMockWallet <$> CW.knownMockWallets

knownWallet :: Integer -> Wallet
knownWallet = fromWalletNumber . CW.WalletNumber

fromWalletNumber :: WalletNumber -> Wallet
fromWalletNumber = toMockWallet . CW.fromWalletNumber

instance Show Wallet where
    showsPrec p (Wallet Nothing i)  = showParen (p > 9) $ showString "Wallet " . shows i
    showsPrec p (Wallet (Just s) _) = showParen (p > 9) $ showString ("Wallet " ++ s)

instance Pretty Wallet where
    pretty (Wallet Nothing i)  = "W" <> pretty (T.take 7 $ toBase16 i)
    pretty (Wallet (Just s) _) = "W[" <> fromString s <> "]"

deriving anyclass instance OpenApi.ToSchema Wallet
deriving anyclass instance OpenApi.ToSchema Cardano.Wallet.WalletId
deriving instance Data Cardano.Wallet.WalletId

newtype WalletId = WalletId { unWalletId :: Cardano.Wallet.WalletId }
    deriving (Eq, Ord, Generic, Data)
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
walletToMockWallet (Wallet _ wid) =
  find ((==) wid . WalletId . Cardano.Wallet.WalletId . CW.mwWalletId) CW.knownMockWallets

-- | The same as @walletToMockWallet@ but fails with an error instead of returning @Nothing@.
walletToMockWallet' :: Wallet -> MockWallet
walletToMockWallet' w =
    fromMaybe (error $ "Wallet.Emulator.Wallet.walletToMockWallet': Wallet "
                          <> show w
                          <> " is not a mock wallet")
    $ walletToMockWallet w

-- | The public key of a mock wallet.  (Fails if the wallet is not a mock wallet).
mockWalletPaymentPubKey :: Wallet -> PaymentPubKey
mockWalletPaymentPubKey = CW.paymentPubKey . walletToMockWallet'

-- | The payment public key hash of a mock wallet.  (Fails if the wallet is not a mock wallet).
mockWalletPaymentPubKeyHash :: Wallet -> PaymentPubKeyHash
mockWalletPaymentPubKeyHash = CW.paymentPubKeyHash . walletToMockWallet'

-- | Get the address of a mock wallet. (Fails if the wallet is not a mock wallet).
mockWalletAddress :: Wallet -> Address
mockWalletAddress = CW.mockWalletAddress . walletToMockWallet'

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
    _mockWallet              :: MockWallet, -- ^ Mock wallet with the user's private key.
    _nodeClient              :: NodeClientState,
    _chainIndexEmulatorState :: ChainIndexEmulatorState,
    _signingProcess          :: Maybe SigningProcess
                                -- ^ Override the signing process.
                                -- Used for testing multi-agent use cases.
    } deriving Show

makeLenses ''WalletState

ownPaymentPrivateKey :: WalletState -> PaymentPrivateKey
ownPaymentPrivateKey = CW.paymentPrivateKey . _mockWallet

ownPaymentPublicKey :: WalletState -> PaymentPubKey
ownPaymentPublicKey = CW.paymentPubKey . _mockWallet

-- | Get the user's own payment public-key address.
ownAddress :: WalletState -> Address
ownAddress = flip Ledger.pubKeyAddress Nothing . ownPaymentPublicKey

-- | An empty wallet using the given private key.
-- for that wallet as the sole watched address.
fromMockWallet :: MockWallet -> WalletState
fromMockWallet mw = WalletState mw emptyNodeClientState mempty Nothing

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
    => WalletEffect ~> Eff effs
handleWallet = \case
    SubmitTxn tx          -> submitTxnH tx
    OwnAddresses          -> ownAddressesH
    BalanceTx utx         -> balanceTxH utx
    WalletAddSignature tx -> walletAddSignatureH tx
    TotalFunds            -> totalFundsH
    YieldUnbalancedTx utx -> yieldUnbalancedTxH utx

  where
    submitTxnH :: (Member NodeClientEffect effs, Member (LogMsg TxBalanceMsg) effs) => CardanoTx -> Eff effs ()
    submitTxnH tx = do
        logInfo $ SubmittingTx tx
        publishTx tx

    ownAddressesH :: (Member (State WalletState) effs) => Eff effs (NonEmpty Address)
    ownAddressesH = do
        mw <- gets _mockWallet
        pure $ NonEmpty.fromList [CW.mockWalletAddress mw]

    balanceTxH ::
        ( Member NodeClientEffect effs
        , Member ChainIndexQueryEffect effs
        , Member (State WalletState) effs
        , Member (LogMsg TxBalanceMsg) effs
        )
        => UnbalancedTx
        -> Eff effs (Either WalletAPIError CardanoTx)
    balanceTxH utx = runError $ do
        logInfo $ BalancingUnbalancedTx utx
        txCTx <- handleBalance utx
        logInfo $ FinishedBalancing txCTx
        pure txCTx

    walletAddSignatureH ::
        ( Member (State WalletState) effs
        , Member (LogMsg TxBalanceMsg) effs
        , Member (Error WalletAPIError) effs
        )
        => CardanoTx -> Eff effs CardanoTx
    walletAddSignatureH txCTx = do
        logInfo $ SigningTx txCTx
        handleAddSignature txCTx

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

handleBalance ::
    ( Member NodeClientEffect effs
    , Member ChainIndexQueryEffect effs
    , Member (State WalletState) effs
    , Member (LogMsg TxBalanceMsg) effs
    , Member (Error WalletAPIError) effs
    )
    => UnbalancedTx
    -> Eff effs CardanoTx
handleBalance utx' = do
    utxo <- get >>= ownOutputs
    params@Params { pSlotConfig } <- WAPI.getClientParams
    let utx = finalize pSlotConfig utx'
        requiredSigners = Set.toList (U.unBalancedTxRequiredSignatories utx)
        eitherTx = view U.cardanoTx utx
    cUtxoIndex <- handleError eitherTx $ fromPlutusIndex params $ UtxoIndex $ U.unBalancedTxUtxoIndex utx <> fmap Tx.toTxOut utxo
    case eitherTx of
        Right _ -> do
            -- Find the fixed point of fee calculation, trying maximally n times to prevent an infinite loop
            let calcFee n fee = do
                    tx <- handleBalanceTx utxo (utx & U.tx . Ledger.fee .~ fee)
                    newFee <- handleError (Right tx) $ estimateTransactionFee params cUtxoIndex requiredSigners tx
                    if newFee /= fee
                        then if n == (0 :: Int)
                            -- If we don't reach a fixed point, pick the larger fee
                            then pure (newFee PlutusTx.\/ fee)
                            else calcFee (n - 1) newFee
                        else pure newFee
            -- Start with a relatively high fee, bigger chance that we get the number of inputs right the first time.
            theFee <- calcFee 5 $ Ada.lovelaceValueOf 300000
            tx <- handleBalanceTx utxo (utx & U.tx . Ledger.fee .~ theFee)
            cTx <- handleError (Right tx) $ fromPlutusTx params cUtxoIndex requiredSigners tx
            pure $ Tx.Both tx (Tx.CardanoApiEmulatorEraTx cTx)
        Left txBodyContent -> do
            ownAddr <- gets ownAddress
            cTx <- handleError eitherTx $ makeAutoBalancedTransaction params cUtxoIndex txBodyContent ownAddr
            pure $ Tx.CardanoApiTx (Tx.CardanoApiEmulatorEraTx cTx)
    where
        handleError tx (Left (Left (ph, ve))) = do
            tx' <- either (throwError . WAPI.ToCardanoError)
                           pure
                 $ either (fmap (Tx.CardanoApiTx . Tx.CardanoApiEmulatorEraTx . makeSignedTransaction []) . makeTransactionBody mempty)
                            (pure . Tx.EmulatorTx)
                 $ tx
            let sves = case ve of
                    Ledger.ScriptFailure f -> [Ledger.ScriptValidationResultOnlyEvent (Left f)]
                    _                      -> []
            logWarn $ ValidationFailed ph (Ledger.getCardanoTxId tx') tx' ve sves mempty
            throwError $ WAPI.ValidationError ve
        handleError _ (Left (Right ce)) = throwError $ WAPI.ToCardanoError ce
        handleError _ (Right v) = pure v

handleAddSignature ::
    ( Member (State WalletState) effs
    , Member (Error WalletAPIError) effs
    )
    => CardanoTx
    -> Eff effs CardanoTx
handleAddSignature tx = do
    msp <- gets _signingProcess
    case msp of
        Nothing -> do
            PaymentPrivateKey privKey <- gets ownPaymentPrivateKey
            pure $ addSignature' privKey tx
        Just (SigningProcess sp) -> do
            let ctx = case tx of
                    Tx.CardanoApiTx (Tx.CardanoApiEmulatorEraTx ctx') -> ctx'
                    Tx.Both _ (Tx.CardanoApiEmulatorEraTx ctx') -> ctx'
                    _ -> error "handleAddSignature: Need a Cardano API Tx from the Alonzo era to get the required signers"
                reqSigners = getRequiredSigners ctx
            sp reqSigners tx

addSignature' :: PrivateKey -> CardanoTx -> CardanoTx
addSignature' privKey = Tx.cardanoTxMap (Ledger.addSignature' privKey) addSignatureCardano
    where
        addSignatureCardano :: SomeCardanoApiTx -> SomeCardanoApiTx
        addSignatureCardano (Tx.CardanoApiEmulatorEraTx ctx)
            = Tx.CardanoApiEmulatorEraTx (addSignature privKey ctx)

ownOutputs :: forall effs.
    ( Member ChainIndexQueryEffect effs
    )
    => WalletState
    -> Eff effs (Map.Map TxOutRef ChainIndexTxOut)
ownOutputs WalletState{_mockWallet} = do
    refs <- allUtxoSet (Just def)
    Map.fromList . catMaybes <$> traverse txOutRefTxOutFromRef refs
  where
    addr :: Address
    addr = CW.mockWalletAddress _mockWallet

    -- Accumulate all unspent 'TxOutRef's from the resulting pages.
    allUtxoSet :: Maybe (PageQuery TxOutRef) -> Eff effs [TxOutRef]
    allUtxoSet Nothing = pure []
    allUtxoSet (Just pq) = do
      refPage <- page <$> ChainIndex.utxoSetAtAddress pq (addressCredential addr)
      nextItems <- allUtxoSet (ChainIndex.nextPageQuery refPage)
      pure $ ChainIndex.pageItems refPage ++ nextItems

    txOutRefTxOutFromRef :: TxOutRef -> Eff effs (Maybe (TxOutRef, ChainIndexTxOut))
    txOutRefTxOutFromRef ref = fmap (ref,) <$> ChainIndex.unspentTxOutFromRef ref

lookupValue ::
    ( Member (Error WAPI.WalletAPIError) effs
    , Member ChainIndexQueryEffect effs
    )
    => Tx.TxIn
    -> Eff effs Value
lookupValue outputRef@TxIn {txInRef} = do
    txoutMaybe <- ChainIndex.unspentTxOutFromRef txInRef
    case txoutMaybe of
        Just txout -> pure $ view Ledger.ciTxOutValue txout
        Nothing ->
            WAPI.throwOtherError $ "Unable to find TxOut for " <> fromString (show outputRef)

-- | Balance an unbalanced transaction by adding missing inputs and outputs
handleBalanceTx ::
    forall effs.
    ( Member NodeClientEffect effs
    , Member (State WalletState) effs
    , Member ChainIndexQueryEffect effs
    , Member (Error WAPI.WalletAPIError) effs
    , Member (LogMsg TxBalanceMsg) effs
    )
    => Map.Map TxOutRef ChainIndexTxOut -- ^ The current wallet's unspent transaction outputs.
    -> UnbalancedTx
    -> Eff effs Tx
handleBalanceTx utxo utx = do
    params@Params { pProtocolParams } <- WAPI.getClientParams
    let filteredUnbalancedTxTx = removeEmptyOutputs (view U.tx utx)
    let txInputs = Set.toList $ Tx.txInputs filteredUnbalancedTxTx
    ownAddr <- gets ownAddress
    inputValues <- traverse lookupValue (Set.toList $ Tx.txInputs filteredUnbalancedTxTx)
    collateral  <- traverse lookupValue (Set.toList $ Tx.txCollateral filteredUnbalancedTxTx)
    let fees = txFee filteredUnbalancedTxTx
        left = txMint filteredUnbalancedTxTx <> fold inputValues
        right = fees <> foldMap (view Tx.outValue) (filteredUnbalancedTxTx ^. Tx.outputs)
        collFees = Ada.toValue $ (Ada.fromValue fees * maybe 100 fromIntegral (protocolParamCollateralPercent pProtocolParams)) `Ada.divide` 100
        remainingCollFees = collFees PlutusTx.- fold collateral
        balance = left PlutusTx.- right
        -- filter out inputs from utxo that are already in unBalancedTx
        inputsOutRefs = map Tx.txInRef txInputs
        filteredUtxo = flip Map.filterWithKey utxo $ \txOutRef _ ->
            txOutRef `notElem` inputsOutRefs

    ((neg, newTxIns), (pos, newTxOuts)) <- calculateTxChanges params ownAddr filteredUtxo $ Value.split balance

    tx' <- if Value.isZero pos
           then do
               logDebug NoOutputsAdded
               pure filteredUnbalancedTxTx
           else do
                logDebug $ AddingPublicKeyOutputFor pos
                pure $ filteredUnbalancedTxTx & over Tx.outputs (++ newTxOuts)

    tx'' <- if Value.isZero neg
            then do
                logDebug NoInputsAdded
                pure tx'
            else do
                logDebug $ AddingInputsFor neg
                pure $ tx' & over Tx.inputs (Set.union $ Set.fromList newTxIns)

    if remainingCollFees `Value.leq` PlutusTx.zero
    then do
        logDebug NoCollateralInputsAdded
        pure tx''
    else do
        logDebug $ AddingCollateralInputsFor remainingCollFees
        addCollateral utxo remainingCollFees tx''

calculateTxChanges
    :: ( Member (Error WAPI.WalletAPIError) effs
       )
    => Params
    -> Address -- ^ The address for the change output
    -> Map.Map TxOutRef ChainIndexTxOut -- ^ The current wallet's unspent transaction outputs.
    -> (Value, Value) -- ^ The unbalanced tx's negative and positive balance.
    -> Eff effs ((Value, [TxIn]), (Value, [TxOut]))
calculateTxChanges params addr utxos (neg, pos) = do

    -- Calculate the change output with minimal ada
    (newNeg, newPos, extraTxOuts) <- if Value.isZero pos
        then pure (neg, pos, [])
        else do
            (missing, extraTxOut) <-
                either (throwError . WAPI.ToCardanoError) pure
                $ U.adjustTxOut params (TxOut addr pos Nothing)
            let missingValue = Ada.toValue (fold missing)
            -- Add the missing ada to both sides to keep the balance.
            pure (neg <> missingValue, pos <> missingValue, [extraTxOut])

    -- Calculate the extra inputs needed
    (spend, change) <- if Value.isZero newNeg
        then pure ([], mempty)
        else selectCoin (second (view Ledger.ciTxOutValue) <$> Map.toList utxos) newNeg

    if Value.isZero change
        then do
            -- No change, so the new inputs and outputs have balanced the transaction
            pure ((newNeg, Tx.pubKeyTxIn . fst <$> spend), (newPos, extraTxOuts))
        else if null extraTxOuts
            -- We have change so we need an extra output, if we didn't have that yet,
            -- first make one with an estimated minimal amount of ada
            -- which then will calculate a more exact set of inputs
            then calculateTxChanges params addr utxos (neg <> Ada.toValue Ledger.minAdaTxOut, Ada.toValue Ledger.minAdaTxOut)
            -- Else recalculate with the change added to both sides
            -- Ideally this creates the same inputs and outputs and then the change will be zero
            -- But possibly the minimal Ada increases and then we also want to compute a new set of inputs
            else calculateTxChanges params addr utxos (newNeg <> change, newPos <> change)

addCollateral
    :: ( Member (Error WAPI.WalletAPIError) effs
       )
    => Map.Map TxOutRef ChainIndexTxOut -- ^ The current wallet's unspent transaction outputs.
    -> Value
    -> Tx
    -> Eff effs Tx
addCollateral mp vl tx = do
    (spend, _) <- selectCoin (filter (Value.isAdaOnlyValue . snd) (second (view Ledger.ciTxOutValue) <$> Map.toList mp)) vl
    let addTxCollateral =
            let ins = Set.fromList (Tx.pubKeyTxIn . fst <$> spend)
            in over Tx.collateralInputs (Set.union ins)
    pure $ tx & addTxCollateral

-- | Given a set of @a@s with coin values, and a target value, select a number
-- of @a@ such that their total value is greater than or equal to the target.
selectCoin ::
    ( Member (Error WAPI.WalletAPIError) effs
    , Eq a
    )
    => [(a, Value)] -- ^ Possible inputs to choose from
    -> Value -- ^ The target value
    -> Eff effs ([(a, Value)], Value) -- ^ The chosen inputs and the change
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
            -- Select inputs per asset class, sorting so we do Ada last.
            -- We want to do the non-Ada asset classes first, because utxo's often contain
            -- extra Ada because of fees or minAda constraints. So when we are done with the
            -- non-Ada asset classes we probably already have picked some Ada too.
            let (usedFinal, remainderFinal) = foldl' step ([], vl) (sortOn Down $ Value.flattenValue vl)
                step (used, remainder) (cur, tok, _) =
                    let (used', remainder') = selectCoinSingle cur tok (fnds \\ used) remainder
                    in (used <> used', remainder')
            in pure (usedFinal, PlutusTx.negate remainderFinal)

selectCoinSingle
    :: Value.CurrencySymbol
    -> Value.TokenName
    -> [(a, Value)] -- ^ Possible inputs to choose from
    -> Value -- ^ The target value
    -> ([(a, Value)], Value) -- ^ The chosen inputs and the remainder
selectCoinSingle cur tok fnds' vl =
    let
        -- We only want the values that contain the given asset class,
        -- and want the single currency values first,
        -- so that we're picking inputs that contain *only* the given asset class when possible.
        fnds = sortOn (length . Value.symbols . snd) $ filter (\(_, v) -> Value.valueOf v cur tok > 0) fnds'
        -- Given the funds of a wallet, we take enough just enough from
        -- the target value such that the asset class value of the remainder is <= 0.
        fundsWithRemainder = zip fnds (drop 1 $ scanl (PlutusTx.-) vl $ fmap snd fnds)
        fundsToSpend       = takeUntil (\(_, v) -> Value.valueOf v cur tok <= 0) fundsWithRemainder
        remainder          = maybe vl snd $ listToMaybe $ reverse fundsToSpend
    in (fst <$> fundsToSpend, remainder)


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
    -> CardanoTx
    -> PaymentPubKeyHash
    -> Eff r CardanoTx
signTxnWithKey mw = signTxWithPrivateKey (CW.paymentPrivateKey mw)

-- | Sign the transaction with the private key, if the hash is that of the
--   private key.
signTxWithPrivateKey
    :: (Member (Error WAPI.WalletAPIError) r)
    => PaymentPrivateKey
    -> CardanoTx
    -> PaymentPubKeyHash
    -> Eff r CardanoTx
signTxWithPrivateKey (PaymentPrivateKey pk) tx pkh@(PaymentPubKeyHash pubK) = do
    let ownPaymentPubKey = Ledger.toPublicKey pk
    if Ledger.pubKeyHash ownPaymentPubKey == pubK
    then pure (addSignature' pk tx)
    else throwError (WAPI.PaymentPrivateKeyNotFound pkh)

-- | Sign the transaction with the given private keys,
--   ignoring the list of public keys that the 'SigningProcess' is passed.
signPrivateKeys :: [PaymentPrivateKey] -> SigningProcess
signPrivateKeys signingKeys = SigningProcess $ \_ tx ->
    pure (foldr (addSignature' . unPaymentPrivateKey) tx signingKeys)

data SigningProcessControlEffect r where
    SetSigningProcess :: Maybe SigningProcess -> SigningProcessControlEffect ()
makeEffect ''SigningProcessControlEffect

type SigningProcessEffs = '[State (Maybe SigningProcess), Error WAPI.WalletAPIError]

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
