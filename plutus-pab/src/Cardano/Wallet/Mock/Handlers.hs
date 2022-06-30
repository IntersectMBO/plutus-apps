{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Cardano.Wallet.Mock.Handlers
    ( processWalletEffects
    , integer2ByteString32
    , byteString2Integer
    , newWallet
    , distributeNewWalletFunds
    ) where

import Cardano.BM.Data.Trace (Trace)
import Cardano.Node.Client qualified as NodeClient
import Cardano.Node.Types (ChainSyncHandle)
import Cardano.Protocol.Socket.Mock.Client qualified as MockClient
import Cardano.Wallet.Mock.Types (MultiWalletEffect (..), WalletEffects, WalletInfo (..), WalletMsg (..), Wallets,
                                  fromWalletState)
import Cardano.Wallet.Primitive.Types as CWP
import Control.Concurrent (MVar)
import Control.Concurrent.MVar (putMVar, takeMVar)
import Control.Lens (at, (?~))
import Control.Monad.Error (MonadError)
import Control.Monad.Except qualified as MonadError
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Extras hiding (Error)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.Freer.State (State, evalState, get, put, runState)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Random (getRandomBytes)
import Data.Bits (shiftL, shiftR)
import Data.ByteArray (ScrubbedBytes, unpack)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.ByteString.Lazy.Char8 qualified as Char8
import Data.Function ((&))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Ledger.Ada qualified as Ada
import Ledger.Address (PaymentPubKeyHash)
import Ledger.CardanoWallet (MockWallet)
import Ledger.CardanoWallet qualified as CW
import Ledger.Params (Params)
import Ledger.Tx (CardanoTx)
import Plutus.ChainIndex (ChainIndexQueryEffect)
import Plutus.ChainIndex.Client qualified as ChainIndex
import Plutus.PAB.Arbitrary ()
import Plutus.PAB.Monitoring.Monitoring qualified as LM
import Plutus.PAB.Types (PABError)
import Prettyprinter (pretty)
import Servant (ServerError (..), err400, err401, err404)
import Servant.Client (ClientEnv)
import Servant.Server (err500)
import Wallet.API (WalletAPIError (..))
import Wallet.API qualified as WAPI
import Wallet.Effects (NodeClientEffect)
import Wallet.Emulator.LogMessages (RequestHandlerLogMsg, TxBalanceMsg)
import Wallet.Emulator.Wallet qualified as Wallet

newtype Seed = Seed ScrubbedBytes

generateSeed :: (LastMember m effs, MonadIO m) => Eff effs Seed
generateSeed = do
    (bytes :: ScrubbedBytes) <- sendM $ liftIO $ getRandomBytes 32
    pure $ Seed bytes

{-# INLINE byteString2Integer #-}
-- |Helper function to convert bytestrings to integers
byteString2Integer :: BS.ByteString -> Integer
byteString2Integer = BS.foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0

{-# INLINE integer2ByteString32 #-}
-- |@i2bs bitLen i@ converts @i@ to a 'ByteString' of @bitLen@ bits (must be a multiple of 8).
integer2ByteString32 :: Integer -> BS.ByteString
integer2ByteString32 i = BS.unfoldr (\l' -> if l' < 0 then Nothing else Just (fromIntegral (i `shiftR` l'), l' - 8)) (31*8)

distributeNewWalletFunds :: forall effs.
    ( Member WAPI.WalletEffect effs
    , Member (Error WalletAPIError) effs
    , Member (LogMsg Text) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    )
    => Params
    -> Maybe Ada.Ada
    -> PaymentPubKeyHash
    -> Eff effs CardanoTx
distributeNewWalletFunds params funds = WAPI.payToPaymentPublicKeyHash params WAPI.defaultSlotRange
    (maybe (Ada.adaValueOf 10_000) Ada.toValue funds)

newWallet :: forall m effs. (LastMember m effs, MonadIO m) => Eff effs MockWallet
newWallet = do
    Seed seed <- generateSeed
    let secretKeyBytes = BS.pack . unpack $ seed
    return $ CW.fromSeed' secretKeyBytes

-- | Handle multiple wallets using existing @Wallet.handleWallet@ handler
handleMultiWallet :: forall m effs.
    ( Member NodeClientEffect effs
    , Member ChainIndexQueryEffect effs
    , Member (State Wallets) effs
    , Member (Error WAPI.WalletAPIError) effs
    , Member (LogMsg WalletMsg) effs
    , Member (LogMsg Text) effs
    , LastMember m effs
    , MonadIO m
    )
    => Params -> MultiWalletEffect ~> Eff effs
handleMultiWallet params = \case
    MultiWallet (Wallet.Wallet _ walletId) action -> do
        wallets <- get @Wallets
        case Map.lookup walletId wallets of
            Just walletState -> do
                (x, newState) <- runState walletState
                    $ action
                        & raiseEnd
                        & interpret Wallet.handleWallet
                        & interpret (mapLog @TxBalanceMsg @WalletMsg Balancing)
                put @Wallets (wallets & at walletId ?~ newState)
                pure x
            Nothing -> throwError $ WAPI.OtherError "Wallet not found"
    CreateWallet funds -> do
        wallets <- get @Wallets
        mockWallet <- newWallet
        let walletId = Wallet.WalletId . CWP.WalletId $ CW.mwWalletId mockWallet
            wallets' = Map.insert walletId (Wallet.fromMockWallet mockWallet) wallets
            pkh = CW.paymentPubKeyHash mockWallet
            addr = CW.mockWalletAddress mockWallet
        put wallets'
        -- For some reason this doesn't work with (Wallet 1)/privateKey1,
        -- works just fine with (Wallet 2)/privateKey2
        -- ¯\_(ツ)_/¯
        let sourceWallet = Wallet.fromMockWallet (CW.knownMockWallet 2)
        _ <- evalState sourceWallet $
            interpret (mapLog @TxBalanceMsg @WalletMsg Balancing)
            $ interpret (mapLog @RequestHandlerLogMsg @WalletMsg RequestHandling)
            $ interpret Wallet.handleWallet
            $ distributeNewWalletFunds params funds pkh
        return $ WalletInfo
            { wiWallet = Wallet.toMockWallet mockWallet
            , wiPaymentPubKeyHash = pkh
            , wiAddresses = NonEmpty.fromList [addr]
            }
    GetWalletInfo wllt -> do
        wallets <- get @Wallets
        return $ fmap fromWalletState $ Map.lookup wllt wallets

-- | Process wallet effects. Retain state and yield HTTP400 on error
--   or set new state on success.
processWalletEffects ::
    (MonadIO m, MonadError ServerError m)
    => Trace IO WalletMsg -- ^ trace for logging
    -> MockClient.TxSendHandle -- ^ node client
    -> ChainSyncHandle -- ^ node client
    -> ClientEnv          -- ^ chain index client
    -> MVar Wallets   -- ^ wallets state
    -> Params
    -> Eff (WalletEffects IO) a -- ^ wallet effect
    -> m a
processWalletEffects trace txSendHandle chainSyncHandle chainIndexEnv mVarState params action = do
    oldState <- liftIO $ takeMVar mVarState
    result <- liftIO $ runWalletEffects trace
                                        txSendHandle
                                        chainSyncHandle
                                        chainIndexEnv
                                        oldState
                                        params
                                        action
    case result of
        Left e -> do
            liftIO $ putMVar mVarState oldState
            MonadError.throwError $ err400 { errBody = Char8.pack (show e) }
        Right (result_, newState) -> do
            liftIO $ putMVar mVarState newState
            pure result_

-- | Interpret wallet effects
runWalletEffects ::
    Trace IO WalletMsg -- ^ trace for logging
    -> MockClient.TxSendHandle -- ^ node client
    -> ChainSyncHandle -- ^ node client
    -> ClientEnv -- ^ chain index client
    -> Wallets -- ^ current state
    -> Params
    -> Eff (WalletEffects IO) a -- ^ wallet effect
    -> IO (Either ServerError (a, Wallets))
runWalletEffects trace txSendHandle chainSyncHandle chainIndexEnv wallets params action =
    reinterpret (handleMultiWallet params) action
    & interpret (LM.handleLogMsgTrace trace)
    & reinterpret2 (NodeClient.handleNodeClientClient params)
    & runReader chainSyncHandle
    & runReader (Just txSendHandle)
    & reinterpret ChainIndex.handleChainIndexClient
    & runReader chainIndexEnv
    & runState wallets
    & flip catchError logPabErrorAndRethrow
    & handlePrettyErrors
    & interpret (LM.handleLogMsgTrace (toWalletMsg trace))
    & handleWalletApiErrors
    & handleClientErrors
    & runError
    & runM
        where
            handleWalletApiErrors = flip handleError (throwError . fromWalletAPIError)
            handleClientErrors = flip handleError (\e -> throwError $ err500 { errBody = Char8.pack (show e) })
            handlePrettyErrors = flip handleError (\e -> throwError $ err500 { errBody = Char8.pack (show $ pretty e) })
            toWalletMsg = LM.convertLog ChainClientMsg
            logPabErrorAndRethrow (e :: PABError) = do
                logError (pack $ show $ pretty e)
                throwError e

-- | Convert Wallet errors to Servant error responses
fromWalletAPIError :: WalletAPIError -> ServerError
fromWalletAPIError (InsufficientFunds text) =
    err401 {errBody = BSL.fromStrict $ encodeUtf8 text}
fromWalletAPIError e@NoPaymentPubKeyHashError =
    err404 {errBody = BSL8.pack $ show e}
fromWalletAPIError e@(PaymentPrivateKeyNotFound _) =
    err404 {errBody = BSL8.pack $ show e}
fromWalletAPIError e@(ValidationError _) =
    err500 {errBody = BSL8.pack $ show $ pretty e}
fromWalletAPIError e@(ToCardanoError _) =
    err500 {errBody = BSL8.pack $ show $ pretty e}
fromWalletAPIError e@ChangeHasLessThanNAda {} =
    err500 {errBody = BSL8.pack $ show $ pretty e}
fromWalletAPIError e@PaymentMkTxError {} =
    err500 {errBody = BSL8.pack $ show $ pretty e}
fromWalletAPIError e@(RemoteClientFunctionNotYetSupported _) =
    err500 {errBody = BSL8.pack $ show $ pretty e}
fromWalletAPIError (OtherError text) =
    err500 {errBody = BSL.fromStrict $ encodeUtf8 text}
