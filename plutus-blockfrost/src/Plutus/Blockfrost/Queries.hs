{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Plutus.Blockfrost.Queries (
    getTipBlockfrost
    ) where

import Blockfrost.Client

-- ENDPOINTS

getTipBlockfrost :: MonadBlockfrost m => m Block
getTipBlockfrost = getLatestBlock
