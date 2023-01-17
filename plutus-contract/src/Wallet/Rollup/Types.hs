{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TemplateHaskell            #-}

module Wallet.Rollup.Types where

import Control.Lens (makeLenses, makeLensesFor)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Map (Map)
import GHC.Generics
import Ledger (CardanoTx, PaymentPubKeyHash (PaymentPubKeyHash), TxIn, TxOut, cardanoAddressCredential, txOutAddress)
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential, ScriptCredential), TxId, ValidatorHash, Value)
import Prettyprinter (Pretty, pretty, viaShow)

data TxKey =
    TxKey
        { _txKeyTxId        :: TxId
        , _txKeyTxOutRefIdx :: Integer
        }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty TxKey where
    pretty = viaShow

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
          { originalInput :: TxIn
          , refersTo      :: TxOut
          }
    | InputNotFound TxKey
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
        , txId               :: TxId
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
        { _previousOutputs :: Map TxKey TxOut
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
