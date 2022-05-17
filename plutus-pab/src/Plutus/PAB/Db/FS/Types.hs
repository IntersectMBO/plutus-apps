{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}


module Plutus.PAB.Db.FS.Types where

import Data.Aeson
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as AT
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Plutus.Contract.Effects (PABReq, PABResp)
import Plutus.Contract.State (ContractResponse)
import Plutus.Contract.Test qualified as Wallet
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Webserver.Types (ContractActivationArgs (ContractActivationArgs, caID, caWallet))
import Wallet.Emulator.Wallet (Wallet (..))
import Wallet.Types (ContractActivityStatus)

newtype ContractStoreDir t = ContractStoreDir FilePath
   deriving (Show, Eq, Generic)
   deriving anyclass (ToJSON, FromJSON)

-- | `State t` is not really serializable as it contains runtime sync pieces (`TVars` etc.).
-- | The serializable part for the `Builtin` is the below `ContractResponse`.
-- | Please check `PAB.Effects.Contract.PABContract` class for details.
type SerializableState = ContractResponse Value Value PABResp PABReq

data ContractInstance t
  = ContractInstance
    { contractActivationArgs            :: ContractActivationArgs (Contract.ContractDef t)
    , contractInstanceSerialisableState :: Maybe SerializableState
    , contractInstanceActivityStatus    :: ContractActivityStatus
    }


contractActivationArgsToJSON :: ToJSON t => ContractActivationArgs t -> Value
contractActivationArgsToJSON ContractActivationArgs {caID, caWallet} = A.object
  [ "caID" .= A.toJSON caID
  , "caWallet" .= A.toJSON (getWalletId . fromMaybe (Wallet.knownWallet 1) $ caWallet)
  ]

contractActivationArgsFromJSON :: FromJSON t => Value -> AT.Parser (ContractActivationArgs t)
contractActivationArgsFromJSON (Object o) = do
  caID <- o .: "caID" >>= A.parseJSON
  caWallet <- (o .: "caWallet" >>= parseJSON) >>= \case
    Nothing  -> pure Nothing
    Just wId -> pure $ Just $ Wallet Nothing wId
  pure $ ContractActivationArgs {..}
contractActivationArgsFromJSON _ = fail "ContractActivationArgs should be encoded as an JSON object"

instance (ToJSON (Contract.ContractDef t)) => ToJSON (ContractInstance t) where
  toJSON ContractInstance {..} = A.object
    [ "contractActivationArgs" .= contractActivationArgsToJSON contractActivationArgs
    , "contractInstanceSerialisableState" .= maybe Null toJSON contractInstanceSerialisableState
    , "contractInstanceActivityStatus" .= toJSON contractInstanceActivityStatus
    ]

instance (FromJSON (Contract.ContractDef t)) => FromJSON (ContractInstance t) where
  parseJSON (Object o) = do
    contractActivationArgs <- o .: "contractActivationArgs" >>= contractActivationArgsFromJSON
    contractInstanceSerialisableState <- o .: "contractInstanceSerialisableState" >>= \case
      Null  -> pure Nothing
      value -> Just <$> parseJSON value
    contractInstanceActivityStatus <- o.: "contractInstanceActivityStatus" >>= parseJSON
    pure $ ContractInstance {..}
  parseJSON _ = fail "ContractInstance should be encoded a an JSON object"
