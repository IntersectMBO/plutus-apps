{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Wno-deprecations #-} -- TODO Remove once TotalFunds gets removed

module Cardano.Wallet.LocalClient where

import Cardano.Api (shelleyAddressInEra)
import Cardano.Api qualified
import Cardano.Node.Types (PABServerConfig (pscPassphrase))
import Cardano.Wallet.Api qualified as C
import Cardano.Wallet.Api.Client qualified as C
import Cardano.Wallet.Api.Types (ApiWallet (assets, balance))
import Cardano.Wallet.Api.Types qualified as C
import Cardano.Wallet.LocalClient.ExportTx (export)
import Cardano.Wallet.Primitive.AddressDerivation qualified as C
import Cardano.Wallet.Primitive.Types qualified as C
import Cardano.Wallet.Primitive.Types.Hash qualified as C
import Cardano.Wallet.Primitive.Types.TokenMap qualified as C
import Cardano.Wallet.Primitive.Types.TokenPolicy qualified as C
import Cardano.Wallet.Primitive.Types.TokenQuantity qualified as C
import Cardano.Wallet.Primitive.Types.Tx qualified as C
import Cardano.Wallet.Shelley.Compatibility ()
import Control.Monad ((>=>))
import Control.Monad.Freer (Eff, LastMember, Member, sendM, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Extras.Log (LogMsg, logWarn)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (toJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseMaybe, (.:))
import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Quantity (Quantity (Quantity))
import Data.Text (Text, pack)
import Data.Text.Class (fromText)
import Ledger (CardanoAddress, CardanoTx (..))
import Ledger.Tx.CardanoAPI (SomeCardanoApiTx (SomeTx), ToCardanoError)
import Ledger.Tx.Constraints.OffChain (UnbalancedTx)
import Plutus.PAB.Monitoring.PABLogMsg (WalletClientMsg (BalanceTxError, WalletClientError))
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value (Value (Value), currencySymbol, tokenName)
import PlutusTx.AssocMap qualified as Map
import Prettyprinter (Pretty (pretty))
import Servant ((:<|>) ((:<|>)), (:>))
import Servant.Client (ClientEnv, ClientError, ClientM, client, runClientM)
import Wallet.API qualified as WAPI
import Wallet.Effects (WalletEffect (BalanceTx, OwnAddresses, SubmitTxn, TotalFunds, WalletAddSignature, YieldUnbalancedTx))
import Wallet.Emulator.Error (WalletAPIError (OtherError, ToCardanoError))
import Wallet.Emulator.Wallet (Wallet (Wallet), WalletId (WalletId))

getWalletKey :: C.ApiT C.WalletId -> C.ApiT C.Role -> C.ApiT C.DerivationIndex -> Maybe Bool -> ClientM C.ApiVerificationKeyShelley
getWalletKey :<|> _ :<|> _ :<|> _ = client (Proxy @("v2" :> C.WalletKeys))

handleWalletClient
    :: forall m effs.
    ( LastMember m effs
    , MonadIO m
    , Member WAPI.NodeClientEffect effs
    , Member (Error ClientError) effs
    , Member (Error WalletAPIError) effs
    , Member (Reader ClientEnv) effs
    , Member (LogMsg WalletClientMsg) effs
    )
    => PABServerConfig -- TODO: Rename. Not mock
    -> Wallet
    -> WalletEffect
    ~> Eff effs
handleWalletClient config (Wallet _ (WalletId wId)) event = do
    clientEnv <- ask @ClientEnv
    params <- WAPI.getClientParams
    let walletId = C.WalletId wId
        mpassphrase = pscPassphrase config

        runClient :: ClientM a -> Eff effs a
        runClient a = do
            result <- runClient' a
            case result of
                Left err -> do
                    logWarn (WalletClientError $ show err)
                    throwError err
                Right e -> pure e
        runClient' :: ClientM a -> Eff effs (Either ClientError a)
        runClient' a = do
            result <- sendM $ liftIO $ runClientM a clientEnv
            case result of
                Left err -> do
                    logWarn (WalletClientError $ show err)
                    pure . Left $ err
                Right _ -> pure result

        submitTxnH :: CardanoTx -> Eff effs ()
        submitTxnH tx = do
            sealedTx <- either (throwError . ToCardanoError) pure $ toSealedTx tx
            void . runClient $ C.postExternalTransaction C.transactionClient (C.ApiBytesT (C.SerialisedTx $ C.serialisedTx sealedTx))

        ownAddressesH :: Eff effs (NonEmpty CardanoAddress)
        ownAddressesH = do
            addressValues <- runClient $ C.listAddresses  C.addressClient (C.ApiT walletId) Nothing
            pure $ NonEmpty.fromList $ mapMaybe (decodeApiAddress >=> fromApiAddress) addressValues
         where
             decodeApiAddress :: Aeson.Value -> Maybe Text
             decodeApiAddress v = parseMaybe (Aeson.withObject "ApiAddress" (\o -> o .: "id")) v

             fromApiAddress :: Text -> Maybe CardanoAddress
             fromApiAddress addrBech32 = do
                 case Cardano.Api.deserialiseFromBech32 (Cardano.Api.AsAddress Cardano.Api.AsShelleyAddr) addrBech32 of
                   Left _         -> Nothing
                   Right addrCApi -> Just $ shelleyAddressInEra addrCApi

        balanceTxH :: UnbalancedTx -> Eff effs (Either WalletAPIError CardanoTx)
        balanceTxH utx = do
            case export params utx of
                Left err -> do
                    logWarn $ BalanceTxError $ show $ pretty err
                    throwOtherError $ pretty err
                Right ex -> do
                    res <- runClient' $ C.balanceTransaction C.transactionClient (C.ApiT walletId) (toJSON ex)
                    case res of
                        -- TODO: use the right error case based on http error code
                        Left err -> pure $ Left $ OtherError $ pack $ show err
                        Right r  -> do
                            pure (Right $ fromApiSerialisedTransaction r)

        walletAddSignatureH :: CardanoTx -> Eff effs CardanoTx
        walletAddSignatureH tx = do
            sealedTx <- either (throwError . ToCardanoError) pure $ toSealedTx tx
            passphrase <- maybe (throwError $ OtherError "Wallet passphrase required") pure mpassphrase
            lenientPP <- either throwOtherError pure $ fromText passphrase
            let postData = C.ApiSignTransactionPostData (C.ApiT sealedTx) (C.ApiT lenientPP)
            fmap fromApiSerialisedTransaction . runClient $ C.signTransaction C.transactionClient (C.ApiT walletId) postData

        totalFundsH :: Eff effs Value
        totalFundsH = do
            C.ApiWallet{balance, assets} <- runClient $ C.getWallet C.walletClient (C.ApiT walletId)
            let C.ApiWalletBalance (Quantity avAda) _ _ = balance
                C.ApiWalletAssetsBalance (C.ApiT avAssets) _ = assets
            pure $ Ada.lovelaceValueOf (fromIntegral avAda) <> tokenMapToValue avAssets

        yieldUnbalancedTxH :: UnbalancedTx -> Eff effs ()
        yieldUnbalancedTxH utx = do
            balancedTxM <- balanceTxH utx
            case balancedTxM of
                Left err         -> throwError err
                Right balancedTx -> walletAddSignatureH balancedTx >>= submitTxnH

    case event of
        SubmitTxn tx          -> submitTxnH tx
        OwnAddresses          -> ownAddressesH
        BalanceTx utx         -> balanceTxH utx
        WalletAddSignature tx -> walletAddSignatureH tx
        TotalFunds            -> totalFundsH
        YieldUnbalancedTx utx -> yieldUnbalancedTxH utx

tokenMapToValue :: C.TokenMap -> Value
tokenMapToValue = Value . Map.fromList . fmap (bimap (currencySymbol . C.getHash . C.unTokenPolicyId) (Map.fromList . fmap (bimap (tokenName . C.unTokenName) (fromIntegral . C.unTokenQuantity)) . toList)) . C.toNestedList

fromApiSerialisedTransaction :: C.ApiSerialisedTransaction -> CardanoTx
fromApiSerialisedTransaction (C.ApiSerialisedTransaction (C.ApiT sealedTx)) = CardanoTx $ case C.cardanoTxIdeallyNoLaterThan (Cardano.Api.anyCardanoEra Cardano.Api.BabbageEra) sealedTx of
    Cardano.Api.InAnyCardanoEra Cardano.Api.ByronEra tx   -> SomeTx tx Cardano.Api.ByronEraInCardanoMode
    Cardano.Api.InAnyCardanoEra Cardano.Api.ShelleyEra tx -> SomeTx tx Cardano.Api.ShelleyEraInCardanoMode
    Cardano.Api.InAnyCardanoEra Cardano.Api.AllegraEra tx -> SomeTx tx Cardano.Api.AllegraEraInCardanoMode
    Cardano.Api.InAnyCardanoEra Cardano.Api.MaryEra tx    -> SomeTx tx Cardano.Api.MaryEraInCardanoMode
    Cardano.Api.InAnyCardanoEra Cardano.Api.AlonzoEra tx  -> SomeTx tx Cardano.Api.AlonzoEraInCardanoMode
    Cardano.Api.InAnyCardanoEra Cardano.Api.BabbageEra tx -> SomeTx tx Cardano.Api.BabbageEraInCardanoMode

toSealedTx :: CardanoTx -> Either ToCardanoError C.SealedTx
toSealedTx (CardanoTx (SomeTx tx Cardano.Api.ByronEraInCardanoMode)) = Right $ C.sealedTxFromCardano $ Cardano.Api.InAnyCardanoEra Cardano.Api.ByronEra tx
toSealedTx (CardanoTx (SomeTx tx Cardano.Api.ShelleyEraInCardanoMode)) = Right $ C.sealedTxFromCardano $ Cardano.Api.InAnyCardanoEra Cardano.Api.ShelleyEra tx
toSealedTx (CardanoTx (SomeTx tx Cardano.Api.AllegraEraInCardanoMode)) = Right $ C.sealedTxFromCardano $ Cardano.Api.InAnyCardanoEra Cardano.Api.AllegraEra tx
toSealedTx (CardanoTx (SomeTx tx Cardano.Api.MaryEraInCardanoMode)) = Right $ C.sealedTxFromCardano $ Cardano.Api.InAnyCardanoEra Cardano.Api.MaryEra tx
toSealedTx (CardanoTx (SomeTx tx Cardano.Api.AlonzoEraInCardanoMode)) = Right $ C.sealedTxFromCardano $ Cardano.Api.InAnyCardanoEra Cardano.Api.AlonzoEra tx
toSealedTx (CardanoTx (SomeTx tx Cardano.Api.BabbageEraInCardanoMode)) = Right $ C.sealedTxFromCardano $ Cardano.Api.InAnyCardanoEra Cardano.Api.BabbageEra tx

throwOtherError :: (Member (Error WalletAPIError) effs, Show err) => err -> Eff effs a
throwOtherError = throwError . OtherError . pack . show
