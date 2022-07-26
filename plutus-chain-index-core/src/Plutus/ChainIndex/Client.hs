{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Plutus.ChainIndex.Client(
    -- * HTTP Client handler
    handleChainIndexClient
    -- * Servant client functions
    , healthCheck
    , collectGarbage

    , getDatum
    , getValidator
    , getMintingPolicy
    , getStakeValidator
    , getRedeemer

    , getTxOut
    , getTx
    , getUnspentTxOut
    , getUnspentTxOutsAtAddress
    , getIsUtxo
    , getUtxoSetAtAddress
    , getUtxoSetWithCurrency
    , getTxs
    , getTxoSetAtAddress
    , getTip
    ) where

import Control.Monad.Freer (Eff, LastMember, Member, sendM, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Proxy (Proxy (..))
import Ledger (TxId)
import Ledger.Tx (ChainIndexTxOut, TxOutRef)
import Network.HTTP.Types.Status (Status (..))
import Plutus.ChainIndex.Api (API, IsUtxoResponse, QueryAtAddressRequest (QueryAtAddressRequest), QueryResponse,
                              TxoAtAddressRequest (TxoAtAddressRequest), TxosResponse,
                              UtxoAtAddressRequest (UtxoAtAddressRequest),
                              UtxoWithCurrencyRequest (UtxoWithCurrencyRequest), UtxosResponse)
import Plutus.ChainIndex.Effects (ChainIndexQueryEffect (..))
import Plutus.ChainIndex.Tx (ChainIndexTx)
import Plutus.ChainIndex.Types (Tip)
import Plutus.V1.Ledger.Api (Datum, DatumHash, MintingPolicy, MintingPolicyHash, Redeemer, RedeemerHash, StakeValidator,
                             StakeValidatorHash, Validator, ValidatorHash)
import Servant (NoContent, (:<|>) (..))
import Servant.Client (ClientEnv, ClientError (..), ClientM, client, runClientM)
import Servant.Client.Core.Response (ResponseF (..))

healthCheck :: ClientM NoContent
collectGarbage :: ClientM NoContent

-- TODO: Catch 404 error
getDatum :: DatumHash -> ClientM Datum
getValidator :: ValidatorHash -> ClientM Validator
getMintingPolicy :: MintingPolicyHash -> ClientM MintingPolicy
getStakeValidator :: StakeValidatorHash -> ClientM StakeValidator
getRedeemer :: RedeemerHash -> ClientM Redeemer

getTxOut :: TxOutRef -> ClientM ChainIndexTxOut
getTx :: TxId -> ClientM ChainIndexTx
getUnspentTxOut :: TxOutRef -> ClientM ChainIndexTxOut
getIsUtxo :: TxOutRef -> ClientM IsUtxoResponse
getUtxoSetAtAddress :: UtxoAtAddressRequest -> ClientM UtxosResponse
getUnspentTxOutsAtAddress :: QueryAtAddressRequest -> ClientM (QueryResponse [(TxOutRef, ChainIndexTxOut)])
getUtxoSetWithCurrency :: UtxoWithCurrencyRequest -> ClientM UtxosResponse
getTxs :: [TxId] -> ClientM [ChainIndexTx]
getTxoSetAtAddress :: TxoAtAddressRequest -> ClientM TxosResponse
getTip :: ClientM Tip

(healthCheck, (getDatum, getValidator, getMintingPolicy, getStakeValidator, getRedeemer), getTxOut, getUnspentTxOut, getTx, getIsUtxo, getUtxoSetAtAddress, getUnspentTxOutsAtAddress, getUtxoSetWithCurrency, getTxs, getTxoSetAtAddress, getTip, collectGarbage) =
    (healthCheck_, (getDatum_, getValidator_, getMintingPolicy_, getStakeValidator_, getRedeemer_), getTxOut_, getUnspentTxOut_, getTx_, getIsUtxo_, getUtxoSetAtAddress_, getUnspentTxOutsAtAddress_, getUtxoSetWithCurrency_, getTxs_, getTxoSetAtAddress_, getTip_, collectGarbage_) where
        healthCheck_
            :<|> (getDatum_ :<|> getValidator_ :<|> getMintingPolicy_ :<|> getStakeValidator_ :<|> getRedeemer_)
            :<|> getTxOut_
            :<|> getUnspentTxOut_
            :<|> getTx_
            :<|> getIsUtxo_
            :<|> getUtxoSetAtAddress_
            :<|> getUnspentTxOutsAtAddress_
            :<|> getUtxoSetWithCurrency_
            :<|> getTxs_
            :<|> getTxoSetAtAddress_
            :<|> getTip_
            :<|> collectGarbage_
            :<|> _ = client (Proxy @API)

-- | Handle 'ChainIndexQueryEffect' by making HTTP calls to a remote
--   server.
handleChainIndexClient ::
    forall m effs.
    ( LastMember m effs
    , Member (Reader ClientEnv) effs
    , MonadIO m
    , Member (Error ClientError) effs
    )
    => ChainIndexQueryEffect
    ~> Eff effs
handleChainIndexClient event = do
    clientEnv <- ask
    let
        runClient :: forall a. ClientM a -> Eff effs a
        runClient a = (sendM $ liftIO $ runClientM a clientEnv) >>= either throwError pure
        runClientMaybe :: forall a. ClientM a -> Eff effs (Maybe a)
        runClientMaybe a = do
            response <- sendM $ liftIO $ runClientM a clientEnv
            case response of
                Right a'                                                                     -> pure (Just a')

                -- convert 404 (NOT FOUND) to 'Nothing'
                Left (FailureResponse _ Response{responseStatusCode=Status{statusCode=404}}) -> pure Nothing
                Left e                                                                       -> throwError e
    case event of
        DatumFromHash d               -> runClientMaybe (getDatum d)
        ValidatorFromHash d           -> runClientMaybe (getValidator d)
        MintingPolicyFromHash d       -> runClientMaybe (getMintingPolicy d)
        StakeValidatorFromHash d      -> runClientMaybe (getStakeValidator d)
        RedeemerFromHash d            -> runClientMaybe (getRedeemer d)
        TxFromTxId t                  -> runClientMaybe (getTx t)
        TxOutFromRef r                -> runClientMaybe (getTxOut r)
        UnspentTxOutFromRef r         -> runClientMaybe (getUnspentTxOut r)
        UtxoSetMembership r           -> runClient (getIsUtxo r)
        UtxoSetAtAddress pq a         -> runClient (getUtxoSetAtAddress $ UtxoAtAddressRequest (Just pq) a)
        UnspentTxOutSetAtAddress pq a -> runClient (getUnspentTxOutsAtAddress $ QueryAtAddressRequest (Just pq) a)
        UtxoSetWithCurrency pq a      -> runClient (getUtxoSetWithCurrency $ UtxoWithCurrencyRequest (Just pq) a)
        TxsFromTxIds t                -> runClient (getTxs t)
        TxoSetAtAddress pq a          -> runClient (getTxoSetAtAddress $ TxoAtAddressRequest (Just pq) a)
        GetTip                        -> runClient getTip
