module Schema.Types where

import Prologue
import Chain.Types (_value)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.BigInt.Argonaut (BigInt)
import Data.Foldable (fold, foldMap)
import Data.Functor.Foldable (Fix(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (_2, _Just, over, set)
import Data.Lens.Index (ix)
import Data.Maybe (fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.RawJson (RawJson)
import Data.String.Extra as String
import Data.Traversable (sequence, traverse)
import Data.Tuple.Nested ((/\))
import Foreign.Object as FO
import PlutusTx.AssocMap as AssocMap
import Plutus.V1.Ledger.Interval (Extended(..), Interval(..), LowerBound(..), UpperBound(..))
import Plutus.V1.Ledger.Slot (Slot)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value (CurrencySymbol(..), Value(..))
import Matryoshka (Algebra, ana, cata)
import Playground.Lenses (_recipient, _amount)
import Playground.Types (ContractCall(..), FunctionSchema(..), KnownCurrency(..), _PayToWallet)
import Schema (FormArgumentF(..), FormSchema(..))
import ValueEditor (ValueEvent(..))
import Ledger.CardanoWallet (WalletNumber)

data SimulationAction
  = ModifyActions ActionEvent
  | PopulateAction Int FormEvent

derive instance genericSimulationAction :: Generic SimulationAction _

instance showSimulationAction :: Show SimulationAction where
  show = genericShow

data FormEvent
  = SetField FieldEvent
  | SetSubField Int FormEvent
  | AddSubField
  | RemoveSubField Int

derive instance genericFormEvent :: Generic FormEvent _

instance showFormEvent :: Show FormEvent where
  show value = genericShow value

data FieldEvent
  = SetIntField (Maybe Int)
  | SetBigIntField (Maybe BigInt)
  | SetBoolField Boolean
  | SetStringField String
  | SetHexField String
  | SetRadioField String
  | SetValueField ValueEvent
  | SetPOSIXTimeRangeField (Interval POSIXTime)

derive instance genericFieldEvent :: Generic FieldEvent _

instance showFieldEvent :: Show FieldEvent where
  show = genericShow

data ActionEvent
  = AddAction (ContractCall FormArgument)
  | AddWaitAction BigInt
  | RemoveAction Int
  | SetWaitTime Int BigInt
  | SetWaitUntilTime Int Slot
  | SetPayToWalletValue Int ValueEvent
  | SetPayToWalletRecipient Int WalletNumber

derive instance genericActionEvent :: Generic ActionEvent _

instance showActionEvent :: Show ActionEvent where
  show = genericShow

type Expression = ContractCall RawJson

type FormArgument = Fix FormArgumentF

type Signatures = Array (FunctionSchema FormSchema)

toArgument :: Value -> FormSchema -> FormArgument
toArgument initialValue = ana algebra
  where
  algebra :: FormSchema -> FormArgumentF FormSchema
  algebra FormSchemaUnit = FormUnitF

  algebra FormSchemaBool = FormBoolF false

  algebra FormSchemaInt = FormIntF Nothing

  algebra FormSchemaInteger = FormIntegerF Nothing

  -- text inputs cannot distinguish between `Nothing` and `Just ""` -
  -- use the latter as the default value, or validation behaves weirdly
  algebra FormSchemaString = FormStringF (Just "")

  algebra FormSchemaHex = FormHexF Nothing

  algebra (FormSchemaRadio xs) = FormRadioF xs Nothing

  algebra (FormSchemaArray xs) = FormArrayF xs []

  algebra (FormSchemaMaybe x) = FormMaybeF x Nothing

  algebra FormSchemaValue = FormValueF initialValue

  algebra FormSchemaPOSIXTimeRange = FormPOSIXTimeRangeF defaultTimeRange

  algebra (FormSchemaTuple a b) = FormTupleF a b

  algebra (FormSchemaObject xs) = FormObjectF xs

  algebra (FormSchemaUnsupported x) = FormUnsupportedF x

defaultTimeRange :: Interval POSIXTime
defaultTimeRange =
  Interval
    { ivFrom: LowerBound NegInf true
    , ivTo: UpperBound PosInf true
    }

formArgumentToJson :: FormArgument -> Maybe Json
formArgumentToJson = cata algebra
  where
  algebra :: Algebra FormArgumentF (Maybe Json)
  algebra FormUnitF = Just $ encodeJson (mempty :: Array Unit)

  algebra (FormBoolF b) = Just $ encodeJson b

  algebra (FormIntF n) = encodeJson <$> n

  algebra (FormIntegerF n) = encodeJson <$> n

  algebra (FormStringF str) = encodeJson <$> str

  algebra (FormRadioF _ option) = encodeJson <$> option

  algebra (FormHexF str) = encodeJson <<< String.toHex <$> str

  algebra (FormTupleF (Just fieldA) (Just fieldB)) = Just $ encodeJson [ fieldA, fieldB ]

  algebra (FormTupleF _ _) = Nothing

  algebra (FormMaybeF _ field) = encodeJson <$> field

  algebra (FormArrayF _ fields) = Just $ encodeJson fields

  algebra (FormObjectF fields) = encodeJson <<< FO.fromFoldable <$> traverse sequence fields

  algebra (FormValueF x) = Just $ encodeJson x

  algebra (FormPOSIXTimeRangeF x) = Just $ encodeJson x

  algebra (FormUnsupportedF _) = Nothing

mkInitialValue :: Array KnownCurrency -> BigInt -> Value
mkInitialValue currencies initialBalance = Value { getValue: value }
  where
  value =
    map (map unwrap)
      $ fold
      $ foldMap
          ( \(KnownCurrency { hash, knownTokens }) ->
              map
                ( \tokenName ->
                    AssocMap.Map
                      [ CurrencySymbol { unCurrencySymbol: hash }
                          /\ AssocMap.Map [ tokenName /\ Additive initialBalance ]
                      ]
                )
                $ Array.fromFoldable knownTokens
          )
          currencies

handleFormEvent
  :: Value
  -> FormEvent
  -> FormArgument
  -> FormArgument
handleFormEvent initialValue event = cata (Fix <<< algebra event)
  where
  algebra (SetField (SetIntField n)) (FormIntF _) = FormIntF n

  algebra (SetField (SetBigIntField n)) (FormIntegerF _) = FormIntegerF n

  algebra (SetField (SetBoolField n)) (FormBoolF _) = FormBoolF n

  algebra (SetField (SetStringField s)) (FormStringF _) = FormStringF (Just s)

  algebra (SetField (SetHexField s)) (FormHexF _) = FormHexF (Just s)

  algebra (SetField (SetRadioField s)) (FormRadioF options _) = FormRadioF options (Just s)

  algebra (SetField (SetValueField valueEvent)) (FormValueF value) = FormValueF $ handleValueEvent valueEvent value

  algebra (SetField (SetPOSIXTimeRangeField newInterval)) (FormPOSIXTimeRangeF _) = FormPOSIXTimeRangeF newInterval

  algebra (SetSubField 1 subEvent) (FormTupleF field1 field2) = FormTupleF (handleFormEvent initialValue subEvent field1) field2

  algebra (SetSubField 2 subEvent) (FormTupleF field1 field2) = FormTupleF field1 (handleFormEvent initialValue subEvent field2)

  algebra (SetSubField 0 subEvent) (FormMaybeF schema field) = FormMaybeF schema $ over _Just (handleFormEvent initialValue subEvent) field

  algebra (SetSubField n subEvent) (FormArrayF schema fields) = FormArrayF schema $ over (ix n) (handleFormEvent initialValue subEvent) fields

  algebra (SetSubField n subEvent) (FormObjectF fields) = FormObjectF $ over (ix n <<< _2) (handleFormEvent initialValue subEvent) fields

  -- As the code stands, this is the only guarantee we get that every
  -- value in the array will conform to the schema: the fact that we
  -- create the 'empty' version from the same schema template.
  -- Is more type safety than that possible? Probably.
  -- Is it worth the research effort? Perhaps. :thinking_face:
  algebra AddSubField (FormArrayF schema fields) = FormArrayF schema $ Array.snoc fields (toArgument initialValue schema)

  algebra AddSubField arg = arg

  algebra (RemoveSubField n) (FormArrayF schema fields) = (FormArrayF schema (fromMaybe fields (Array.deleteAt n fields)))

  algebra _ arg = arg

handleActionEvent :: ActionEvent -> Array (ContractCall FormArgument) -> Array (ContractCall FormArgument)
handleActionEvent (AddAction action) = flip Array.snoc action

handleActionEvent (AddWaitAction blocks) = flip Array.snoc (AddBlocks { blocks })

handleActionEvent (RemoveAction index) = fromMaybe <*> Array.deleteAt index

handleActionEvent (SetWaitTime index blocks) = set (ix index) (AddBlocks { blocks })

handleActionEvent (SetPayToWalletValue index valueEvent) = over (ix index <<< _PayToWallet <<< _amount) (handleValueEvent valueEvent)

handleActionEvent (SetPayToWalletRecipient index recipient) = set (ix index <<< _PayToWallet <<< _recipient) recipient

handleActionEvent (SetWaitUntilTime index slot) = set (ix index) (AddBlocksUntil { slot })

handleValueEvent :: ValueEvent -> Value -> Value
handleValueEvent (SetBalance currencySymbol tokenName amount) = set (_value <<< ix currencySymbol <<< ix tokenName) amount

-- | This only exists because of the orphan instance restriction.
traverseFunctionSchema
  :: forall m a b
   . Applicative m
  => (a -> m b)
  -> FunctionSchema a
  -> m (FunctionSchema b)
traverseFunctionSchema f (FunctionSchema { endpointDescription, argument: oldArgument }) = rewrap <$> f oldArgument
  where
  rewrap newArgument = FunctionSchema { endpointDescription, argument: newArgument }
