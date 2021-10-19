{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module Cardano.Wallet.Client where

import qualified Cardano.Wallet.Api                           as C
import qualified Cardano.Wallet.Api.Client                    as C
import           Cardano.Wallet.Api.Types                     (ApiVerificationKeyShelley (..), ApiWallet (..))
import qualified Cardano.Wallet.Api.Types                     as C
import qualified Cardano.Wallet.Primitive.AddressDerivation   as C
import qualified Cardano.Wallet.Primitive.Types               as C
import qualified Cardano.Wallet.Primitive.Types.Hash          as C
import qualified Cardano.Wallet.Primitive.Types.TokenMap      as C
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy   as C
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as C
import           Control.Monad.Freer                          (Eff, LastMember, Member, sendM, type (~>))
import           Control.Monad.Freer.Error                    (Error, throwError)
import           Control.Monad.Freer.Reader                   (Reader, ask)
import           Control.Monad.IO.Class                       (MonadIO (..))
import           Data.Bifunctor                               (bimap)
import           Data.Coerce                                  (coerce)
import           Data.Foldable                                (toList)
import           Data.Proxy                                   (Proxy (Proxy))
import           Data.Quantity
import qualified Ledger.Ada                                   as Ada
import           Ledger.Value
import           Plutus.V1.Ledger.Crypto                      (PubKeyHash (..))
import qualified PlutusTx.AssocMap                            as Map
import           PlutusTx.Builtins.Internal                   (BuiltinByteString (..))
import           Servant                                      ((:<|>) (..))
import           Servant.Client                               (ClientEnv, ClientError, ClientM, client, runClientM)
import           Wallet.Effects                               (WalletEffect (..))
import           Wallet.Emulator.Error                        (WalletAPIError (..))
import           Wallet.Emulator.Wallet                       (Wallet (..), WalletId (..))

getWalletKey :: C.ApiT C.WalletId -> C.ApiT C.Role -> C.ApiT C.DerivationIndex -> Maybe Bool -> ClientM ApiVerificationKeyShelley
getWalletKey :<|> _ :<|> _ :<|> _ = client (Proxy @C.WalletKeys)

handleWalletClient
    :: forall m effs.
    ( LastMember m effs
    , MonadIO m
    , Member (Error ClientError) effs
    , Member (Error WalletAPIError) effs
    , Member (Reader ClientEnv) effs
    )
    => Wallet
    -> WalletEffect
    ~> Eff effs
handleWalletClient (Wallet (WalletId walletId)) event = do
    clientEnv <- ask @ClientEnv
    let
        runClient :: forall a. ClientM a -> Eff effs a
        runClient a = (sendM $ liftIO $ runClientM a clientEnv) >>= either throwError pure
    case event of
        SubmitTxn _t           -> throwError $ OtherError "Not implemented yet"

        OwnPubKeyHash          ->
            fmap (PubKeyHash . BuiltinByteString . fst . getApiVerificationKey) . runClient $
                getWalletKey (C.ApiT walletId) (C.ApiT C.UtxoExternal) (C.ApiT (C.DerivationIndex 0)) (Just True)

        BalanceTx _utx         -> throwError $ OtherError "Not implemented yet"
        WalletAddSignature _tx -> throwError $ OtherError "Not implemented yet"

        TotalFunds             -> do
            C.ApiWallet{balance, assets} <- runClient $ C.getWallet C.walletClient (C.ApiT walletId)
            let C.ApiWalletBalance (Quantity avAda) _ _ = balance
                C.ApiWalletAssetsBalance (C.ApiT avAssets) _ = assets
            pure $ Ada.lovelaceValueOf (fromIntegral avAda) <> tokenMapToValue avAssets

tokenMapToValue :: C.TokenMap -> Value
tokenMapToValue = Value . Map.fromList . fmap (bimap coerce (Map.fromList . fmap (bimap coerce (fromIntegral . C.unTokenQuantity)) . toList)) . C.toNestedList

