{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TemplateHaskell            #-}

module Wallet.Rollup.Types where

import Cardano.Api qualified as C
import Control.Lens (makeLenses, makeLensesFor)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Map (Map)
import GHC.Generics
import Ledger (CardanoTx, PaymentPubKeyHash (PaymentPubKeyHash), TxOut, cardanoAddressCredential, txOutAddress)
import PlutusLedgerApi.V1 (Credential (PubKeyCredential, ScriptCredential), ValidatorHash, Value)

data SequenceId =
    SequenceId
        { slotIndex :: Int
        , txIndex   :: Int
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLensesFor
     [ ("slotIndex", "slotIndexL")
     , ("txIndex", "txIndexL")
     ]
    ''SequenceId

data DereferencedInput
    = DereferencedInput
          { originalInput :: C.TxIn
          , refersTo      :: TxOut
          }
    | InputNotFound C.TxIn
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

isFound :: DereferencedInput -> Bool
isFound DereferencedInput {} = True
isFound (InputNotFound _)    = False

data BeneficialOwner
    = OwnedByPaymentPubKey PaymentPubKeyHash
    | OwnedByScript ValidatorHash
    deriving (Eq, Show, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON, FromJSONKey, ToJSONKey)

toBeneficialOwner :: TxOut -> BeneficialOwner
toBeneficialOwner txOut =
    case cardanoAddressCredential (txOutAddress txOut) of
        PubKeyCredential pkh -> OwnedByPaymentPubKey (PaymentPubKeyHash pkh)
        ScriptCredential vh  -> OwnedByScript vh

data AnnotatedTx =
    AnnotatedTx
        { sequenceId         :: SequenceId
        , txId               :: C.TxId
        , tx                 :: CardanoTx
        , dereferencedInputs :: [DereferencedInput]
        , balances           :: Map BeneficialOwner Value
        , valid              :: Bool
        }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses 'AnnotatedTx

data Rollup =
    Rollup
        { _previousOutputs :: Map C.TxIn TxOut
        , _rollingBalances :: Map BeneficialOwner Value
        }
    deriving (Show, Eq, Generic)

makeLenses 'Rollup

data RollupState =
    RollupState
        { _currentSequenceId     :: SequenceId
        , _rollup                :: Rollup
        , _annotatedTransactions :: [AnnotatedTx] -- reverse order
        }

makeLenses ''RollupState
