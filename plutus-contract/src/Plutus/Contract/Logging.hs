module Plutus.Contract.Logging(
  logDebug
  , logInfo
  , logWarn
  , logError
) where


import Data.Aeson (ToJSON (toJSON))
import Plutus.Contract.Types (Contract (..))

import Control.Monad.Freer.Extras.Log qualified as L

-- | Log a message at the 'Debug' level
logDebug :: ToJSON a => a -> Contract w s e ()
logDebug = Contract . L.logDebug . toJSON

-- | Log a message at the 'Info' level
logInfo :: ToJSON a => a -> Contract w s e ()
logInfo = Contract . L.logInfo . toJSON

-- | Log a message at the 'Warning' level
logWarn :: ToJSON a => a -> Contract w s e ()
logWarn = Contract . L.logWarn . toJSON

-- | Log a message at the 'Error' level
logError :: ToJSON a => a -> Contract w s e ()
logError = Contract . L.logError . toJSON
