module ValueEditor where

import Prelude hiding (div, min)
import Bootstrap (col, colFormLabel, col_, formControl, formGroup, formRow_)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (view)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Halogen.HTML (ClassName(ClassName), HTML, div, input, label, text)
import Halogen.HTML.Elements.Keyed as Keyed
import Halogen.HTML.Events (onValueInput)
import Halogen.HTML.Properties (InputType(InputNumber), classes, min, placeholder, required, type_, value)
import PlutusTx.AssocMap as AssocMap
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName, Value(Value))
import Playground.Lenses (_currencySymbol, _tokenName)

data ValueEvent = SetBalance CurrencySymbol TokenName BigInt

derive instance genericValueEvent :: Generic ValueEvent _

instance showValueEvent :: Show ValueEvent where
  show = genericShow

valueForm :: forall p i. (ValueEvent -> i) -> Value -> HTML p i
valueForm handler (Value { getValue: balances }) =
  Keyed.div_
    ( Array.concat
        ( mapWithIndex (currencyRow handler)
            ( Array.sortWith fst
                $ unwrap balances
            )
        )
    )

currencyRow
  :: forall p i
   . (ValueEvent -> i)
  -> Int
  -> Tuple CurrencySymbol (AssocMap.Map TokenName BigInt)
  -> Array (Tuple String (HTML p i))
currencyRow handler currencyIndex (Tuple currencySymbol tokenBalances) =
  mapWithIndex (balanceRow handler currencyIndex currencySymbol)
    (Array.sortWith fst $ unwrap tokenBalances)

balanceRow
  :: forall p i
   . (ValueEvent -> i)
  -> Int
  -> CurrencySymbol
  -> Int
  -> Tuple TokenName BigInt
  -> Tuple String (HTML p i)
balanceRow handler currencyIndex currencySymbol tokenIndex (Tuple tokenName amount) =
  (show currencyIndex <> "-" <> show tokenIndex)
    /\ div
      [ classes
          [ formGroup
          , ClassName "balance"
          , ClassName ("balance-" <> show currencyIndex <> show tokenIndex)
          ]
      ]
      [ formRow_
          $
            [ label
                [ classes [ col, colFormLabel ] ]
                [ text
                    $ case view _currencySymbol currencySymbol, view _tokenName tokenName of
                        "", "" -> "Lovelace"
                        _, other -> other
                ]
            , col_
                [ input
                    [ type_ InputNumber
                    , classes [ formControl, ClassName "balance-amount" ]
                    , value $ BigInt.toString amount
                    , required true
                    , placeholder "Amount"
                    , min zero
                    , onValueInput
                        $ \str ->
                            -- default to 0 in case of empty or invalid input
                            -- (for reasons I have yet to fathom, this doesn't work when you delete "0";
                            -- until I get to the bottom of that, this is at least an improvement)
                            let
                              newAmount = fromMaybe zero $ BigInt.fromString str
                            in
                              do
                                handler $ SetBalance currencySymbol tokenName newAmount
                    ]
                ]
            ]
      ]
