{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Plutus.Blockfrost.Client(handleBlockfrostClient) where

import Blockfrost.Client (BlockfrostClientT, BlockfrostError (BlockfrostNotFound), projectFromFile, runBlockfrost)
import Control.Monad.Freer (Eff, LastMember, Member, sendM, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Plutus.ChainIndex.Effects (ChainIndexQueryEffect (..))

import Plutus.Blockfrost.Queries
import Plutus.Blockfrost.Responses
import Plutus.Blockfrost.Utils

-- | Handle 'ChainIndexQueryEffect' by making HTTP calls to the remote
--   blockfrost server.
handleBlockfrostClient ::
    forall m effs.
    ( LastMember m effs
    , MonadIO m
    )
    => ChainIndexQueryEffect
    ~> Eff effs
handleBlockfrostClient event = liftIO $ do
    print "------- HANDLE BLOCKFROST CLIENT -------"
    prj <- projectFromFile "/home/valentino/Documents/Plank/plutus-apps/plutus-chain-index-core/testnet-token"
    let
        runClient :: forall a. BlockfrostClientT IO a -> IO a
        runClient a = runBlockfrost prj a >>= either (ioError . userError . show) return

        runClientMaybe :: forall a. BlockfrostClientT IO a -> IO (Maybe a)
        runClientMaybe a = do
            response <- runBlockfrost prj a
            case response of
                Right a'                -> pure (Just a')
                Left BlockfrostNotFound -> pure Nothing
                Left e                  -> ioError (userError $ show e)

    case event of
        DatumFromHash d          -> (runClientMaybe . getDatumBlockfrost . toBlockfrostDatumHash) d      >>= processGetDatum
        RedeemerFromHash d       -> ioError (userError "TODO")
        ValidatorFromHash d      -> ioError (userError "TODO")
        MintingPolicyFromHash d  -> ioError (userError "TODO")
        StakeValidatorFromHash d -> ioError (userError "TODO")
        UnspentTxOutFromRef r    -> ioError (userError "TODO")
        UtxoSetMembership r      -> ioError (userError "TODO")
        UtxoSetAtAddress pq a    -> ioError (userError "TODO")
        UtxoSetWithCurrency pq a -> ioError (userError "TODO")
        TxoSetAtAddress pq a     -> ioError (userError "TODO")
        GetTip                   -> runClient getTipBlockfrost >>= processTip

