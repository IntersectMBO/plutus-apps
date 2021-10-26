module PlutusTx.AssocMapTests
  ( all
  ) where

import Prelude
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Lens (preview, set)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import PlutusTx.AssocMap (Map(..), unionWith)
import Ledger.Extra (sum)
import Plutus.V1.Ledger.Value (CurrencySymbol(..), TokenName(..), Value(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

all :: TestSuite
all =
  suite "PlutusTx.AssocMap" do
    indexTests
    atTests
    unionWithTests
    unionWithCurrenciesTests
    sumTests

currencies :: CurrencySymbol
currencies = CurrencySymbol { unCurrencySymbol: "Currency" }

usd :: TokenName
usd = TokenName { unTokenName: "USD" }

eur :: TokenName
eur = TokenName { unTokenName: "EUR" }

gbp :: TokenName
gbp = TokenName { unTokenName: "GBP" }

baseValue :: Map CurrencySymbol (Map TokenName BigInt)
baseValue = Map [ Tuple currencies (Map [ Tuple usd $ BigInt.fromInt 10 ]) ]

indexTests :: TestSuite
indexTests =
  suite "Index" do
    test "simple gets" do
      equal (Just (BigInt.fromInt 10)) (preview (ix currencies <<< ix usd) baseValue)
      equal Nothing (preview (ix currencies <<< ix eur) baseValue)
    test "simple sets" do
      equal (Just (BigInt.fromInt 20))
        ( baseValue
            # set (ix currencies <<< ix usd) (BigInt.fromInt 20)
            # preview (ix currencies <<< ix usd)
        )

atTests :: TestSuite
atTests =
  suite "At" do
    test "create" do
      equal
        baseValue
        ( Map []
            # set (at currencies) (Just (Map []))
            # set (ix currencies <<< at usd) (Just (BigInt.fromInt 10))
        )
    test "modify" do
      equal (Just (BigInt.fromInt 20))
        ( baseValue
            # set (ix currencies <<< at usd) (Just (BigInt.fromInt 20))
            # preview (ix currencies <<< ix usd)
        )
    test "delete" do
      equal Nothing
        ( baseValue
            # set (ix currencies <<< at usd) Nothing
            # preview (ix currencies <<< ix usd)
        )

unionWithTests :: TestSuite
unionWithTests = do
  suite "unionWith" do
    let
      a =
        Map
          [ "a" /\ 1
          , "b" /\ 2
          , "c" /\ 3
          ]
    let
      b =
        Map
          [ "b" /\ 1
          , "c" /\ 2
          , "d" /\ 3
          ]
    test "Merge with (+)" do
      equal
        ( Map
            [ "a" /\ 1
            , "b" /\ 3
            , "c" /\ 5
            , "d" /\ 3
            ]
        )
        (unionWith (+) a b)
    test "Merge with (-)" do
      equal
        ( Map
            [ "a" /\ 1
            , "b" /\ 1
            , "c" /\ 1
            , "d" /\ 3
            ]
        )
        (unionWith (-) a b)

unionWithCurrenciesTests :: TestSuite
unionWithCurrenciesTests =
  suite "unionWith - currencies" do
    let
      valueA =
        ( mkMap currencies
            [ Tuple usd 10
            , Tuple eur 20
            ]
        )

      valueB =
        ( mkMap currencies
            [ Tuple eur 30
            , Tuple gbp 40
            ]
        )
    test "addition"
      $ equal
          ( mkMap currencies
              [ Tuple usd 10
              , Tuple eur 50
              , Tuple gbp 40
              ]
          )
          (unionWith (unionWith (+)) valueA valueB)
    test "choice"
      $ equal
          ( mkMap currencies
              [ Tuple usd 10
              , Tuple eur 20
              , Tuple gbp 40
              ]
          )
          (unionWith (unionWith const) valueA valueB)

sumTests :: TestSuite
sumTests =
  suite "sum" do
    let
      valueA =
        mkValue currencies
          [ Tuple usd 10
          , Tuple eur 20
          ]

      valueB =
        mkValue currencies
          [ Tuple eur 30
          , Tuple gbp 40
          ]
    test "sum"
      $ equal
          ( mkValue currencies
              ( [ Tuple usd 10
                , Tuple eur 50
                , Tuple gbp 40
                ]
              )
          )
          (sum valueA valueB)

mkValue :: CurrencySymbol -> Array (Tuple TokenName Int) -> Value
mkValue symbol pairs =
  Value
    { getValue: mkMap symbol pairs }

mkMap :: CurrencySymbol -> Array (Tuple TokenName Int) -> Map CurrencySymbol (Map TokenName BigInt)
mkMap symbol pairs =
  Map
    [ Tuple symbol (Map (mkTokenAmount <$> pairs)) ]
  where
  mkTokenAmount :: Tuple TokenName Int -> Tuple TokenName BigInt
  mkTokenAmount (Tuple token amount) = Tuple token (BigInt.fromInt amount)
