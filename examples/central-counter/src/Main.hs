{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Reader         hiding (ask)
import           Counter.WebAPI
import           Data.Aeson
import           Data.IORef
import           Data.Proxy
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text.IO                       as T
import           Data.Typeable
import           GHC.Generics                       (Generic)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API

type CounterVar = IORef Int


type HandlerConstraint m = (MonadIO m, MonadReader CounterVar m)

getCounter ::  HandlerConstraint m => m Int
getCounter = liftIO . readIORef =<< ask


putCounter :: HandlerConstraint m => CounterAction -> m Int
putCounter action = liftIO . flip atomicModifyIORef' (doAction action) =<< ask
  where
    doAction (CounterAdd val) c = (c+val, c+val)
    doAction (CounterSet val) _ = (val, val)

counterHandlers :: ServerT CounterAPI (ReaderT CounterVar Handler)
counterHandlers = getCounter :<|> putCounter

toServant' :: CounterVar -> Maybe AuthToken -> ReaderT CounterVar Handler a -> Handler a
toServant' cVar (Just (VerySecret "topsecret")) m = runReaderT m cVar
toServant' _ _ _ = throwError $ err401 { errBody = "Your have to provide a valid secret, which is topsecret!" }

toServant :: CounterVar -> Maybe AuthToken -> ReaderT CounterVar Handler :~> Handler
toServant cVar secret = Nat $ toServant' cVar secret

counterServer :: CounterVar -> Maybe AuthToken -> Server CounterAPI
counterServer cVar secret = enter (toServant cVar secret) counterHandlers

fullHandlers :: CounterVar -> Server FullAPI
fullHandlers cVar = counterServer cVar :<|> serveDirectory "frontend/dist/"

counterApplication :: CounterVar -> Application
counterApplication = serve fullAPI . fullHandlers

main :: IO ()
main = newIORef 0 >>= run 8081 . counterApplication
