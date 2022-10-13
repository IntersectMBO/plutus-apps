{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module Plutus.Blockfrost.Client(handleBlockfrostClient) where

import Blockfrost.Client (BlockfrostClientT, BlockfrostError (BlockfrostNotFound), projectFromFile, runBlockfrost)
import Control.Monad.Freer (Eff, LastMember, Member, type (~>))
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.IO.Class (MonadIO (..))
import Plutus.ChainIndex.Effects (ChainIndexQueryEffect (..))

import Plutus.Blockfrost.Queries
import Plutus.Blockfrost.Responses
import Plutus.Blockfrost.Types (BlockfrostEnv (..))
import Plutus.Blockfrost.Utils


-- | Handle 'ChainIndexQueryEffect' by making HTTP calls to the remote
--   blockfrost server.
handleBlockfrostClient ::
    forall m effs.
    ( LastMember m effs
    , Member (Reader BlockfrostEnv) effs
    , MonadIO m
    )
    => ChainIndexQueryEffect
    ~> Eff effs
handleBlockfrostClient event = do
    bfEnv <- ask
    liftIO $ do
        prj <- projectFromFile $ envBfTokenPath bfEnv
        let
            runClient :: forall a. BlockfrostClientT IO a -> IO a
            runClient a = runBlockfrost prj a >>= either (ioError . userError . show) return

            runClientWithDef :: forall a. BlockfrostClientT IO a -> BlockfrostClientT IO a -> IO a
            runClientWithDef defIO a = do
                response <- runBlockfrost prj a
                case response of
                    Right a'                -> return a'
                    Left BlockfrostNotFound -> runClient defIO
                    Left e                  -> ioError (userError $ show e)

            runClientMaybe :: forall a. BlockfrostClientT IO a -> IO (Maybe a)
            runClientMaybe a = do
                response <- runBlockfrost prj a
                case response of
                    Right a'                -> pure (Just a')
                    Left BlockfrostNotFound -> pure Nothing
                    Left e                  -> ioError (userError $ show e)

        case event of
            DatumFromHash d               -> (runClientMaybe . getDatumBlockfrost . toBlockfrostDatumHash) d        >>= processGetDatum
            RedeemerFromHash d            -> (runClientMaybe . getDatumBlockfrost . toBlockfrostDatumHash) d        >>= processGetDatum
            ValidatorFromHash d           -> (runClientMaybe . getValidatorBlockfrost . toBlockfrostScriptHash) d   >>= processGetValidator
            MintingPolicyFromHash d       -> (runClientMaybe . getValidatorBlockfrost . toBlockfrostScriptHash) d   >>= processGetValidator
            StakeValidatorFromHash d      -> (runClientMaybe . getValidatorBlockfrost . toBlockfrostScriptHash) d   >>= processGetValidator
            UnspentTxOutFromRef r         -> (runClientMaybe . getUnspentTxOutBlockfrost . toBlockfrostRef) r       >>= processUnspentTxOut
            TxOutFromRef r                -> (runClientMaybe . getTxOutBlockfrost . toBlockfrostRef) r              >>= processUnspentTxOut
            TxFromTxId i                  -> (runClientMaybe . getTxFromTxIdBlockfrost . toBlockfrostTxHash) i      >>= processGetTxFromTxId
            TxsFromTxIds is               -> (runClientWithDef defaultGetList . getTxsFromTxIdsBlockfrost . toBlockfrostTxHashes) is >>= processGetTxsFromTxIds
            UtxoSetMembership r           -> (runClientWithDef defaultIsUtxo  . getIsUtxoBlockfrost . toBlockfrostRef) r                >>= processIsUtxo
            UtxoSetAtAddress pq a         -> (runClientWithDef defaultGetUtxo . getUtxoAtAddressBlockfrost pq . credentialToAddress (envNetworkId bfEnv)) a  >>= processGetUtxos pq
            UtxoSetWithCurrency pq a      -> (runClientWithDef defaultGetUtxo . getUtxoSetWithCurrency pq . toBlockfrostAssetId) a      >>= processGetUtxos pq
            TxoSetAtAddress pq a          -> (runClientWithDef defaultGetList . getTxoAtAddressBlockfrost pq . credentialToAddress (envNetworkId bfEnv)) a >>= processGetTxos pq
            GetTip                        -> runClient getTipBlockfrost >>= processTip
            UnspentTxOutSetAtAddress pq a -> (runClientWithDef defaultGetList . getUnspentAtAddressBlockfrost pq . credentialToAddress (envNetworkId bfEnv)) a  >>= processUnspentTxOutSetAtAddress pq a
            DatumsAtAddress pq a          -> (runClientWithDef defaultGetList . getDatumsAtAddressBlockfrost pq . credentialToAddress (envNetworkId bfEnv)) a  >>= processDatumsAtAddress pq a
