module Cardano.Wallet.Nami
  ( WalletId(..)
  , Pagination(..)
  , enable
  , isEnabled
  , getWalletId
  , getNetworkId
  , getBalance
  , getUtxos
  , getCollateral
  , getUsedAddresses
  , getUnusedAddresses
  , getChangeAddress
  , getRewardAddress
  , balanceTx
  , signData
  , signTx
  , submitTx
  ) where

import Prologue
import Control.Promise (Promise, toAffE)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Undefinable (Undefinable, toUndefinable)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)

type WalletId = { unWalletId :: String }

data Pagination = Pagination
  { paginationPage :: Int
  , paginationLimit :: Int
  }

foreign import enableImpl :: Effect (Promise Boolean)

enable :: forall m. MonadAff m => m Boolean
enable = liftAff $ toAffE enableImpl

foreign import isEnabled :: Boolean

foreign import getWalletIdImpl :: Effect (Promise String)

getWalletId :: forall m. MonadAff m => m WalletId
getWalletId = liftAff $ map (\wid -> { unWalletId: wid }) $ toAffE getWalletIdImpl

foreign import getNetworkIdImpl :: Effect (Promise Int)

getNetworkId :: forall m. MonadAff m => m Int
getNetworkId = liftAff $ toAffE getNetworkIdImpl

foreign import getBalanceImpl :: Effect (Promise Uint8Array)

getBalance :: forall m. MonadAff m => m Uint8Array
getBalance = liftAff $ toAffE getBalanceImpl

foreign import getUtxosImpl :: Undefinable String -> Undefinable Pagination -> Effect (Promise (Array String))

getUtxos :: forall m. MonadAff m => Maybe String -> Maybe Pagination -> m (Array String)
getUtxos amount paginate = liftAff $ toAffE $ getUtxosImpl (toUndefinable amount) (toUndefinable paginate)

foreign import getCollateralImpl :: Effect (Promise (Array String))

getCollateral :: forall m. MonadAff m => m (Array String)
getCollateral = liftAff $ toAffE getCollateralImpl

foreign import getUsedAddressesImpl :: Effect (Promise (Array String))

getUsedAddresses :: forall m. MonadAff m => m (Array String)
getUsedAddresses = liftAff $ toAffE getUsedAddressesImpl

foreign import getUnusedAddressesImpl :: Effect (Promise (Array String))

getUnusedAddresses :: forall m. MonadAff m => m (Array String)
getUnusedAddresses = liftAff $ toAffE getUnusedAddressesImpl

foreign import getChangeAddressImpl :: Effect (Promise String)

getChangeAddress :: forall m. MonadAff m => m String
getChangeAddress = liftAff $ toAffE getChangeAddressImpl

foreign import getRewardAddressImpl :: Effect (Promise String)

getRewardAddress :: forall m. MonadAff m => m String
getRewardAddress = liftAff $ toAffE getRewardAddressImpl

foreign import signDataImpl :: String -> String -> Effect (Promise String)

signData :: forall m. MonadAff m => String -> String -> m String
signData address payload = liftAff $ toAffE $ signDataImpl address payload

-- TODO Function 'balanceTx' needs to be available in the Nami wallet API.
foreign import balanceTxImpl :: String -> Effect (Promise String)

balanceTx :: forall m. MonadAff m => String -> m String
balanceTx tx = liftAff $ toAffE $ balanceTxImpl tx

foreign import signTxImpl :: String -> Undefinable Boolean -> Effect (Promise String)

signTx :: forall m. MonadAff m => String -> Maybe Boolean -> m String
signTx tx partialSign = liftAff $ toAffE $ signTxImpl tx (toUndefinable partialSign)

foreign import submitTxImpl :: String -> Effect (Promise String)

submitTx :: forall m. MonadAff m => String -> m String
submitTx tx = liftAff $ toAffE $ submitTxImpl tx

-- onAccountChange
-- onNetworkChange
