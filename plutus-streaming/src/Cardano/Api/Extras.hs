{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Extras where

import Cardano.Api
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as C8
import Data.Proxy
import Data.String

-- FIXME orphan instance
-- https://github.com/input-output-hk/cardano-node/pull/3608
instance IsString (Hash BlockHeader) where
  fromString = either error id . deserialiseFromRawBytesBase16 . C8.pack
    where
      deserialiseFromRawBytesBase16 str =
        case Base16.decode str of
          Right raw -> case deserialiseFromRawBytes ttoken raw of
            Just x  -> Right x
            Nothing -> Left ("cannot deserialise " ++ show str)
          Left msg -> Left ("invalid hex " ++ show str ++ ", " ++ msg)
        where
          ttoken = proxyToAsType (Proxy :: Proxy a)

deriving instance Show BlockHeader

deriving instance Show LedgerState

deriving instance Show LedgerEvent

deriving instance Show MIRDistributionDetails

deriving instance Show PoolReapDetails
