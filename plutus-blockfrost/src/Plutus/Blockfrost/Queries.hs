{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Plutus.Blockfrost.Queries (
    getTipBlockfrost
    , getDatumBlockfrost
    ) where

import Data.Aeson (Value)
import Data.Functor ((<&>))

import Blockfrost.Client

-- ENDPOINTS

getTipBlockfrost :: MonadBlockfrost m => m Block
getTipBlockfrost = getLatestBlock

getDatumBlockfrost :: MonadBlockfrost m => DatumHash -> m Value
getDatumBlockfrost dHash = getScriptDatum dHash <&> _scriptDatumJsonValue
