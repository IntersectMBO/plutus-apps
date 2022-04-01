{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Plutus.ChainIndex.Http.Server
    ( serveChainIndexServer
    , serveChainIndex
    ) where

import Control.Monad ((>=>))
import Control.Monad.Freer (Eff, Member, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Data.Default (Default (def))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Network.Wai.Handler.Warp qualified as Warp
import Plutus.ChainIndex.Effects (ChainIndexControlEffect, ChainIndexQueryEffect)
import Plutus.ChainIndex.Effects qualified as E
import Plutus.ChainIndex.Http.Api (API, FromHashAPI, FullAPI, TxoAtAddressRequest (TxoAtAddressRequest),
                                   UtxoAtAddressRequest (UtxoAtAddressRequest),
                                   UtxoWithCurrencyRequest (UtxoWithCurrencyRequest), swagger)
import Servant.API ((:<|>) (..))
import Servant.API.ContentTypes (NoContent (..))
import Servant.Server (Handler, ServerError, ServerT, err404, hoistServer, serve)

serveChainIndexServer ::
    Int -- ^ Port
    -> Eff '[Error ServerError, ChainIndexQueryEffect, ChainIndexControlEffect] ~> Handler
    -> IO ()
serveChainIndexServer port action = do
    let server = hoistServer (Proxy @API) action serveChainIndex
    Warp.run port (serve (Proxy @FullAPI) (server :<|> swagger))

serveChainIndex ::
    forall effs.
    ( Member (Error ServerError) effs
    , Member ChainIndexQueryEffect effs
    , Member ChainIndexControlEffect effs
    )
    => ServerT API (Eff effs)
serveChainIndex =
    pure NoContent
    :<|> serveFromHashApi
    :<|> (E.unspentTxOutFromRef >=> handleMaybe)
    :<|> E.utxoSetMembership
    :<|> (\(UtxoAtAddressRequest pq c) -> E.utxoSetAtAddress (fromMaybe def pq) c)
    :<|> (\(UtxoWithCurrencyRequest pq c) -> E.utxoSetWithCurrency (fromMaybe def pq) c)
    :<|> (\(TxoAtAddressRequest pq c) -> E.txoSetAtAddress (fromMaybe def pq) c)
    :<|> E.getTip
    :<|> E.getDiagnostics
    :<|> E.collectGarbage *> pure NoContent

serveFromHashApi ::
    forall effs.
    ( Member (Error ServerError) effs
    , Member ChainIndexQueryEffect effs
    )
    => ServerT FromHashAPI (Eff effs)
serveFromHashApi =
    (E.datumFromHash >=> handleMaybe)
    :<|> (E.validatorFromHash >=> handleMaybe)
    :<|> (E.mintingPolicyFromHash >=> handleMaybe)
    :<|> (E.stakeValidatorFromHash >=> handleMaybe)
    :<|> (E.redeemerFromHash >=> handleMaybe)

-- | Return the value of throw a 404 error
handleMaybe :: forall effs. Member (Error ServerError) effs => Maybe ~> Eff effs
handleMaybe = maybe (throwError err404) pure
