-- File auto generated by purescript-bridge! --
module Ledger.Index.Internal where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut (encodeJson, jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Ledger.Crypto (PubKey, Signature)
import Ledger.Slot (Slot)
import Ledger.Tx.Types.Tx (Tx)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Scripts (DatumHash, ScriptError, Validator)
import Plutus.V1.Ledger.Tx (RedeemerPtr, TxIn, TxOut, TxOutRef)
import Plutus.V1.Ledger.Value (Value)
import Type.Proxy (Proxy(Proxy))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.Map as Map

newtype UtxoIndex = UtxoIndex { getIndex :: Map TxOutRef TxOut }

derive instance Eq UtxoIndex

instance Show UtxoIndex where
  show a = genericShow a

instance EncodeJson UtxoIndex where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { getIndex: (E.dictionary E.value E.value) :: _ (Map TxOutRef TxOut) }
    )

instance DecodeJson UtxoIndex where
  decodeJson = defer \_ -> D.decode $ (UtxoIndex <$> D.record "UtxoIndex" { getIndex: (D.dictionary D.value D.value) :: _ (Map TxOutRef TxOut) })

derive instance Generic UtxoIndex _

derive instance Newtype UtxoIndex _

--------------------------------------------------------------------------------

_UtxoIndex :: Iso' UtxoIndex { getIndex :: Map TxOutRef TxOut }
_UtxoIndex = _Newtype

--------------------------------------------------------------------------------

data ValidationError
  = InOutTypeMismatch TxIn TxOut
  | TxOutRefNotFound TxOutRef
  | InvalidScriptHash Validator String
  | InvalidDatumHash String DatumHash
  | MissingRedeemer RedeemerPtr
  | InvalidSignature PubKey Signature
  | ValueNotPreserved Value Value
  | NegativeValue Tx
  | ValueContainsLessThanMinAda Tx TxOut Value
  | NonAdaFees Tx
  | ScriptFailure ScriptError
  | CurrentSlotOutOfRange Slot
  | SignatureMissing PubKeyHash
  | MintWithoutScript String
  | TransactionFeeTooLow Value Value
  | CardanoLedgerValidationError String

derive instance Eq ValidationError

instance Show ValidationError where
  show a = genericShow a

instance EncodeJson ValidationError where
  encodeJson = defer \_ -> case _ of
    InOutTypeMismatch a b -> E.encodeTagged "InOutTypeMismatch" (a /\ b) (E.tuple (E.value >/\< E.value))
    TxOutRefNotFound a -> E.encodeTagged "TxOutRefNotFound" a E.value
    InvalidScriptHash a b -> E.encodeTagged "InvalidScriptHash" (a /\ b) (E.tuple (E.value >/\< E.value))
    InvalidDatumHash a b -> E.encodeTagged "InvalidDatumHash" (a /\ b) (E.tuple (E.value >/\< E.value))
    MissingRedeemer a -> E.encodeTagged "MissingRedeemer" a E.value
    InvalidSignature a b -> E.encodeTagged "InvalidSignature" (a /\ b) (E.tuple (E.value >/\< E.value))
    ValueNotPreserved a b -> E.encodeTagged "ValueNotPreserved" (a /\ b) (E.tuple (E.value >/\< E.value))
    NegativeValue a -> E.encodeTagged "NegativeValue" a E.value
    ValueContainsLessThanMinAda a b c -> E.encodeTagged "ValueContainsLessThanMinAda" (a /\ b /\ c) (E.tuple (E.value >/\< E.value >/\< E.value))
    NonAdaFees a -> E.encodeTagged "NonAdaFees" a E.value
    ScriptFailure a -> E.encodeTagged "ScriptFailure" a E.value
    CurrentSlotOutOfRange a -> E.encodeTagged "CurrentSlotOutOfRange" a E.value
    SignatureMissing a -> E.encodeTagged "SignatureMissing" a E.value
    MintWithoutScript a -> E.encodeTagged "MintWithoutScript" a E.value
    TransactionFeeTooLow a b -> E.encodeTagged "TransactionFeeTooLow" (a /\ b) (E.tuple (E.value >/\< E.value))
    CardanoLedgerValidationError a -> E.encodeTagged "CardanoLedgerValidationError" a E.value

instance DecodeJson ValidationError where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "ValidationError"
    $ Map.fromFoldable
        [ "InOutTypeMismatch" /\ D.content (D.tuple $ InOutTypeMismatch </$\> D.value </*\> D.value)
        , "TxOutRefNotFound" /\ D.content (TxOutRefNotFound <$> D.value)
        , "InvalidScriptHash" /\ D.content (D.tuple $ InvalidScriptHash </$\> D.value </*\> D.value)
        , "InvalidDatumHash" /\ D.content (D.tuple $ InvalidDatumHash </$\> D.value </*\> D.value)
        , "MissingRedeemer" /\ D.content (MissingRedeemer <$> D.value)
        , "InvalidSignature" /\ D.content (D.tuple $ InvalidSignature </$\> D.value </*\> D.value)
        , "ValueNotPreserved" /\ D.content (D.tuple $ ValueNotPreserved </$\> D.value </*\> D.value)
        , "NegativeValue" /\ D.content (NegativeValue <$> D.value)
        , "ValueContainsLessThanMinAda" /\ D.content (D.tuple $ ValueContainsLessThanMinAda </$\> D.value </*\> D.value </*\> D.value)
        , "NonAdaFees" /\ D.content (NonAdaFees <$> D.value)
        , "ScriptFailure" /\ D.content (ScriptFailure <$> D.value)
        , "CurrentSlotOutOfRange" /\ D.content (CurrentSlotOutOfRange <$> D.value)
        , "SignatureMissing" /\ D.content (SignatureMissing <$> D.value)
        , "MintWithoutScript" /\ D.content (MintWithoutScript <$> D.value)
        , "TransactionFeeTooLow" /\ D.content (D.tuple $ TransactionFeeTooLow </$\> D.value </*\> D.value)
        , "CardanoLedgerValidationError" /\ D.content (CardanoLedgerValidationError <$> D.value)
        ]

derive instance Generic ValidationError _

--------------------------------------------------------------------------------

_InOutTypeMismatch :: Prism' ValidationError { a :: TxIn, b :: TxOut }
_InOutTypeMismatch = prism' (\{ a, b } -> (InOutTypeMismatch a b)) case _ of
  (InOutTypeMismatch a b) -> Just { a, b }
  _ -> Nothing

_TxOutRefNotFound :: Prism' ValidationError TxOutRef
_TxOutRefNotFound = prism' TxOutRefNotFound case _ of
  (TxOutRefNotFound a) -> Just a
  _ -> Nothing

_InvalidScriptHash :: Prism' ValidationError { a :: Validator, b :: String }
_InvalidScriptHash = prism' (\{ a, b } -> (InvalidScriptHash a b)) case _ of
  (InvalidScriptHash a b) -> Just { a, b }
  _ -> Nothing

_InvalidDatumHash :: Prism' ValidationError { a :: String, b :: DatumHash }
_InvalidDatumHash = prism' (\{ a, b } -> (InvalidDatumHash a b)) case _ of
  (InvalidDatumHash a b) -> Just { a, b }
  _ -> Nothing

_MissingRedeemer :: Prism' ValidationError RedeemerPtr
_MissingRedeemer = prism' MissingRedeemer case _ of
  (MissingRedeemer a) -> Just a
  _ -> Nothing

_InvalidSignature :: Prism' ValidationError { a :: PubKey, b :: Signature }
_InvalidSignature = prism' (\{ a, b } -> (InvalidSignature a b)) case _ of
  (InvalidSignature a b) -> Just { a, b }
  _ -> Nothing

_ValueNotPreserved :: Prism' ValidationError { a :: Value, b :: Value }
_ValueNotPreserved = prism' (\{ a, b } -> (ValueNotPreserved a b)) case _ of
  (ValueNotPreserved a b) -> Just { a, b }
  _ -> Nothing

_NegativeValue :: Prism' ValidationError Tx
_NegativeValue = prism' NegativeValue case _ of
  (NegativeValue a) -> Just a
  _ -> Nothing

_ValueContainsLessThanMinAda :: Prism' ValidationError { a :: Tx, b :: TxOut, c :: Value }
_ValueContainsLessThanMinAda = prism' (\{ a, b, c } -> (ValueContainsLessThanMinAda a b c)) case _ of
  (ValueContainsLessThanMinAda a b c) -> Just { a, b, c }
  _ -> Nothing

_NonAdaFees :: Prism' ValidationError Tx
_NonAdaFees = prism' NonAdaFees case _ of
  (NonAdaFees a) -> Just a
  _ -> Nothing

_ScriptFailure :: Prism' ValidationError ScriptError
_ScriptFailure = prism' ScriptFailure case _ of
  (ScriptFailure a) -> Just a
  _ -> Nothing

_CurrentSlotOutOfRange :: Prism' ValidationError Slot
_CurrentSlotOutOfRange = prism' CurrentSlotOutOfRange case _ of
  (CurrentSlotOutOfRange a) -> Just a
  _ -> Nothing

_SignatureMissing :: Prism' ValidationError PubKeyHash
_SignatureMissing = prism' SignatureMissing case _ of
  (SignatureMissing a) -> Just a
  _ -> Nothing

_MintWithoutScript :: Prism' ValidationError String
_MintWithoutScript = prism' MintWithoutScript case _ of
  (MintWithoutScript a) -> Just a
  _ -> Nothing

_TransactionFeeTooLow :: Prism' ValidationError { a :: Value, b :: Value }
_TransactionFeeTooLow = prism' (\{ a, b } -> (TransactionFeeTooLow a b)) case _ of
  (TransactionFeeTooLow a b) -> Just { a, b }
  _ -> Nothing

_CardanoLedgerValidationError :: Prism' ValidationError String
_CardanoLedgerValidationError = prism' CardanoLedgerValidationError case _ of
  (CardanoLedgerValidationError a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

data ValidationPhase
  = Phase1
  | Phase2

derive instance Eq ValidationPhase

derive instance Ord ValidationPhase

instance Show ValidationPhase where
  show a = genericShow a

instance EncodeJson ValidationPhase where
  encodeJson = defer \_ -> E.encode E.enum

instance DecodeJson ValidationPhase where
  decodeJson = defer \_ -> D.decode D.enum

derive instance Generic ValidationPhase _

instance Enum ValidationPhase where
  succ = genericSucc
  pred = genericPred

instance Bounded ValidationPhase where
  bottom = genericBottom
  top = genericTop

--------------------------------------------------------------------------------

_Phase1 :: Prism' ValidationPhase Unit
_Phase1 = prism' (const Phase1) case _ of
  Phase1 -> Just unit
  _ -> Nothing

_Phase2 :: Prism' ValidationPhase Unit
_Phase2 = prism' (const Phase2) case _ of
  Phase2 -> Just unit
  _ -> Nothing
