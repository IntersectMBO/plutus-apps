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
{-# OPTIONS_GHC -Wno-deprecations #-} -- TODO Remove once TotalFunds gets removed

module Wallet.Emulator.Wallet where

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Chain (ChainState (_index))
import Cardano.Node.Emulator.Fee qualified as Fee
import Cardano.Node.Emulator.Params (Params (..))
import Control.Lens (makeLenses, makePrisms, view)
import Control.Monad (foldM, (<=<))
import Control.Monad.Freer (Eff, Member, Members, interpret, type (~>))
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Extras.Log (LogMsg, logInfo, logWarn)
import Control.Monad.Freer.State (State, get, gets, put)
import Control.Monad.Freer.TH (makeEffect)
import Crypto.Hash qualified as Crypto
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), ToJSONKey)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Default (Default (def))
import Data.Foldable (find, foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import Ledger (CardanoTx, DecoratedTxOut, PubKeyHash, TxOutRef, UtxoIndex (..))
import Ledger qualified
import Ledger.Address (CardanoAddress, PaymentPrivateKey (..), PaymentPubKey, PaymentPubKeyHash (PaymentPubKeyHash),
                       cardanoAddressCredential)
import Ledger.CardanoWallet (MockWallet, WalletNumber)
import Ledger.CardanoWallet qualified as CW
import Ledger.Credential (Credential (PubKeyCredential, ScriptCredential))
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI (fromCardanoValue, getRequiredSigners)
import Ledger.Tx.CardanoAPI qualified as CardanoAPI
import Ledger.Tx.Constraints.OffChain (UnbalancedTx)
import Ledger.Tx.Constraints.OffChain qualified as U
import Plutus.ChainIndex (PageQuery)
import Plutus.ChainIndex qualified as ChainIndex
import Plutus.ChainIndex.Api (UtxosResponse (page))
import Plutus.ChainIndex.Emulator (ChainIndexEmulatorState, ChainIndexQueryEffect)
import Plutus.Contract.Checkpoint (CheckpointLogMsg)
import Plutus.V1.Ledger.Api (ValidatorHash, Value)
import Prettyprinter (Pretty (pretty))
import Servant.API (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))
import Wallet.Effects (NodeClientEffect,
                       WalletEffect (BalanceTx, OwnAddresses, SubmitTxn, TotalFunds, WalletAddSignature, YieldUnbalancedTx),
                       getClientParams, publishTx)
import Wallet.Emulator.Error qualified as WAPI (WalletAPIError (InsufficientFunds, PaymentPrivateKeyNotFound, ToCardanoError, ValidationError))
import Wallet.Emulator.LogMessages (RequestHandlerLogMsg,
                                    TxBalanceMsg (BalancingUnbalancedTx, FinishedBalancing, SigningTx, SubmittingTx, ValidationFailed))
import Wallet.Emulator.NodeClient (NodeClientState, emptyNodeClientState)
import Wallet.Error (WalletAPIError)


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

newtype WalletId = WalletId { unWalletId :: Crypto.Digest Crypto.Blake2b_160 }
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

toBase16 :: WalletId -> T.Text
toBase16 = T.decodeUtf8 . convertToBase Base16 . unWalletId

fromBase16 :: T.Text -> Either String WalletId
fromBase16 s = maybe (Left $ show s) (Right . WalletId)
    (decodeHex s >>= Crypto.digestFromByteString @_ @ByteString)
    where
    decodeHex = either (const Nothing) Just . convertFromBase Base16 . T.encodeUtf8

-- | The 'MockWallet' whose ID is the given wallet ID (if it exists)
walletToMockWallet :: Wallet -> Maybe MockWallet
walletToMockWallet (Wallet _ wid) =
  find ((==) wid . WalletId . CW.mwWalletId) CW.knownMockWallets

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

-- | Get the cardano address of a mock wallet. (Fails if the wallet is not a mock wallet).
mockWalletAddress :: Wallet -> CardanoAddress
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
    _nodeClient              :: NodeClientState, -- ^ The representation of the node, as known by the wallet
    _chainIndexEmulatorState :: ChainIndexEmulatorState, -- ^ the chain index info known by the wallet
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
ownAddress :: WalletState -> CardanoAddress
ownAddress = CW.mockWalletAddress . _mockWallet

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

    ownAddressesH :: (Member (State WalletState) effs) => Eff effs (NonEmpty CardanoAddress)
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
    totalFundsH = fromCardanoValue . foldMap (view Ledger.decoratedTxOutValue) <$> (get >>= ownOutputs)

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
handleBalance utx = do
    params@Params { pNetworkId } <- getClientParams
    utxo <- get >>= ownOutputs
    mappedUtxo <- either (throwError . WAPI.ToCardanoError) pure $ traverse (Tx.toTxOut pNetworkId) utxo
    let unbalancedBodyContent = U.unBalancedCardanoBuildTx utx
    ownAddr <- gets ownAddress
    -- filter out inputs from utxo that are already in unBalancedTx
    let inputsOutRefs = map Tx.txInRef $ Tx.getTxBodyContentInputs $ CardanoAPI.getCardanoBuildTx unbalancedBodyContent
        filteredUtxo = flip Map.filterWithKey mappedUtxo $ \txOutRef _ ->
            txOutRef `notElem` inputsOutRefs
    cTx <- Fee.makeAutoBalancedTransactionWithUtxoProvider
        params
        (UtxoIndex $ U.unBalancedTxUtxoIndex utx)
        ownAddr
        (handleBalancingError utx . Fee.utxoProviderFromWalletOutputs filteredUtxo)
        (handleError utx . Left)
        unbalancedBodyContent
    pure $ Tx.CardanoEmulatorEraTx cTx
    where
        handleError utx' (Left (Left (ph, ve))) = do
            tx' <- either (throwError . WAPI.ToCardanoError)
                           pure
                 $ fmap (Tx.CardanoEmulatorEraTx . C.makeSignedTransaction [])
                          . CardanoAPI.makeTransactionBody Nothing mempty
                 $ U.unBalancedCardanoBuildTx utx'
            logWarn $ ValidationFailed ph (Ledger.getCardanoTxId tx') tx' ve mempty []
            throwError $ WAPI.ValidationError ve
        handleError _ (Left (Right ce)) = throwError $ WAPI.ToCardanoError ce
        handleError _ (Right v) = pure v
        handleBalancingError _ (Left (Fee.InsufficientFunds total expected)) = throwError $ WAPI.InsufficientFunds
            $ T.unwords
                [ "Total:", T.pack $ show total
                , "expected:", T.pack $ show expected ]
        handleBalancingError utx' (Left (Fee.CardanoLedgerError e)) = handleError utx' (Left e)
        handleBalancingError _ (Right v) = pure v

handleAddSignature ::
    ( Member (State WalletState) effs
    , Member (Error WalletAPIError) effs
    )
    => CardanoTx
    -> Eff effs CardanoTx
handleAddSignature tx@(Tx.CardanoEmulatorEraTx ctx) = do
    msp <- gets _signingProcess
    case msp of
        Nothing -> do
            PaymentPrivateKey privKey <- gets ownPaymentPrivateKey
            pure $ Tx.addCardanoTxSignature privKey tx
        Just (SigningProcess sp) -> do
            let reqSigners = getRequiredSigners ctx
            sp reqSigners tx

ownOutputs :: forall effs.
    ( Member ChainIndexQueryEffect effs
    )
    => WalletState
    -> Eff effs (Map.Map TxOutRef DecoratedTxOut)
ownOutputs WalletState{_mockWallet} = do
    refs <- allUtxoSet (Just def)
    Map.fromList . catMaybes <$> traverse txOutRefTxOutFromRef refs
  where
    addr :: CardanoAddress
    addr = CW.mockWalletAddress _mockWallet

    -- Accumulate all unspent 'TxOutRef's from the resulting pages.
    allUtxoSet :: Maybe (PageQuery TxOutRef) -> Eff effs [TxOutRef]
    allUtxoSet Nothing = pure []
    allUtxoSet (Just pq) = do
      refPage <- page <$> ChainIndex.utxoSetAtAddress pq (cardanoAddressCredential addr)
      nextItems <- allUtxoSet (ChainIndex.nextPageQuery refPage)
      pure $ ChainIndex.pageItems refPage ++ nextItems

    txOutRefTxOutFromRef :: TxOutRef -> Eff effs (Maybe (TxOutRef, DecoratedTxOut))
    txOutRefTxOutFromRef ref = fmap (ref,) <$> ChainIndex.unspentTxOutFromRef ref

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
    then pure (Tx.addCardanoTxSignature pk tx)
    else throwError (WAPI.PaymentPrivateKeyNotFound pkh)

-- | Sign the transaction with the given private keys,
--   ignoring the list of public keys that the 'SigningProcess' is passed.
signPrivateKeys :: [PaymentPrivateKey] -> SigningProcess
signPrivateKeys signingKeys = SigningProcess $ \_ tx ->
    pure (foldr (Tx.addCardanoTxSignature . unPaymentPrivateKey) tx signingKeys)

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
balances :: ChainState -> WalletSet -> Map.Map Entity C.Value
balances state wallets = foldl' f Map.empty . getIndex . _index $ state
  where
    toEntity :: CardanoAddress -> Entity
    toEntity a =
        case cardanoAddressCredential a of
            PubKeyCredential h ->
                case Map.lookup (PaymentPubKeyHash h) ws of
                    Nothing -> PubKeyHashEntity h
                    Just w  -> WalletEntity w
            ScriptCredential h -> ScriptEntity h

    ws :: Map.Map PaymentPubKeyHash Wallet
    ws = walletPaymentPubKeyHashes wallets

    f m o = Map.insertWith (<>) (toEntity $ Ledger.txOutAddress o) (Ledger.txOutValue o) m
