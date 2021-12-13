{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Types
  ( Config(..)
  ) where

import Auth qualified
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

newtype Config = Config
  { _authConfig :: Auth.Config
  }

instance FromJSON Config where
  parseJSON =
    withObject "config" $ \o -> do
      _authConfig <- o .: "auth"
      pure Config {..}
