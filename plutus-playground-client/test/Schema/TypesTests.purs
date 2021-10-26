module Schema.TypesTests
  ( all
  ) where

import Prologue
import Data.Argonaut.Core (Json, stringify)
import Data.BigInt.Argonaut as BigInt
import Data.Functor.Foldable (Fix(..))
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Effect.Aff (Aff)
import PlutusTx.AssocMap as AssocMap
import Plutus.V1.Ledger.Value (CurrencySymbol(..), TokenName(..), Value(..))
import Playground.Types (ContractCall(..), FunctionSchema(..), KnownCurrency(..))
import Schema (FormSchema(..), FormArgumentF(..))
import Schema.Types (FormArgument, formArgumentToJson, mkInitialValue, toArgument)
import Test.Unit (TestSuite, Test, suite, test)
import Test.Unit.Assert (equal)
import Validation (ValidationError(..), validate, withPath)
import Ledger.CardanoWallet (WalletNumber(..))
import Wallet.Types (EndpointDescription(..))

all :: TestSuite
all =
  suite "Schema.Types" do
    validateTests
    toArgumentTests
    formArgumentToJsonTests
    mkInitialValueTests

validateTests :: TestSuite
validateTests = do
  test "No validation errors" do
    isValid $ AddBlocks { blocks: one }
    isValid $ makeTestAction $ Fix $ FormIntF (Just 5)
    isValid $ makeTestAction $ Fix $ FormIntegerF (Just (BigInt.fromInt 5))
    isValid $ makeTestAction $ Fix $ FormStringF (Just "TEST")
    isValid $ makeTestAction $ Fix $ FormTupleF (Fix $ FormIntF (Just 5)) (Fix $ FormIntF (Just 6))
    isValid $ makeTestAction $ Fix $ FormArrayF FormSchemaInt []
    isValid $ makeTestAction $ Fix $ FormObjectF []
  --
  test "Validation errors" do
    equal [ withPath [] Unsupported ] $ validate (makeTestAction $ Fix $ FormUnsupportedF "Test case.")
    equal [ withPath [] Required ] $ validate (makeTestAction $ Fix $ FormIntF Nothing)
    equal [ withPath [] Required ] $ validate (makeTestAction $ Fix $ FormIntegerF Nothing)
    equal [ withPath [] Required ] $ validate (makeTestAction $ Fix $ FormStringF Nothing)
    equal
      [ withPath [ "_1" ] Required
      , withPath [ "_2" ] Unsupported
      ]
      (validate (makeTestAction $ Fix $ FormTupleF (Fix $ FormIntF Nothing) (Fix $ FormUnsupportedF "Test.")))
    equal [ withPath [ "_1" ] Required ] $ validate (makeTestAction $ Fix $ FormTupleF (Fix $ FormIntF Nothing) (Fix $ FormIntF (Just 5)))
    equal [ withPath [ "_2" ] Required ] $ validate (makeTestAction $ Fix $ FormTupleF (Fix $ FormIntF (Just 5)) (Fix $ FormIntF Nothing))
    equal [ withPath [ "2" ] Required ]
      $ validate
          ( makeTestAction
              $ Fix
              $ FormArrayF FormSchemaInt
                  [ Fix $ FormIntF (Just 5)
                  , Fix $ FormIntF (Just 6)
                  , Fix $ FormIntF Nothing
                  , Fix $ FormIntF (Just 7)
                  ]
          )
    equal
      [ withPath [ "name" ] Required ]
      ( validate
          ( makeTestAction
              $ Fix
              $ FormObjectF
                  [ Tuple "name" (Fix $ FormStringF Nothing)
                  , Tuple "test" (Fix $ FormIntF (Just 5))
                  ]
          )
      )

toArgumentTests :: TestSuite
toArgumentTests = do
  suite "toArgument" do
    let
      initialValue :: Value
      initialValue =
        Value
          { getValue:
              AssocMap.Map
                [ ( Tuple
                      (CurrencySymbol { unCurrencySymbol: "12345" })
                      ( AssocMap.Map
                          [ Tuple
                              (TokenName { unTokenName: "ADA" })
                              (BigInt.fromInt 100)
                          ]
                      )
                  )
                ]
          }
    test "FormIntF" do
      equal
        (toArgument initialValue FormSchemaInt)
        (Fix (FormIntF Nothing))
    test "Value" do
      equal
        (toArgument initialValue FormSchemaValue)
        (Fix (FormValueF initialValue))

makeTestAction :: FormArgument -> ContractCall FormArgument
makeTestAction argument =
  CallEndpoint
    { caller:
        WalletNumber { getWallet: one }
    , argumentValues:
        FunctionSchema
          { endpointDescription: EndpointDescription { getEndpointDescription: "test" }
          , argument
          }
    }

isValid :: ContractCall FormArgument -> Aff Unit
isValid = validate >>> equal []

formArgumentToJsonTests :: TestSuite
formArgumentToJsonTests = do
  suite "FormArgument to JSON" do
    test "Ints" do
      equalJson
        Nothing
        (formArgumentToJson (Fix $ FormIntF Nothing))
      equalJson
        (Just "5")
        (formArgumentToJson (Fix $ FormIntF (Just 5)))
    test "BigInts" do
      equalJson
        Nothing
        (formArgumentToJson (Fix $ FormIntegerF Nothing))
      equalJson
        (Just "5")
        (formArgumentToJson (Fix $ FormIntegerF (Just (BigInt.fromInt 5))))
    test "Strings" do
      equalJson
        Nothing
        (formArgumentToJson (Fix $ FormStringF Nothing))
      equalJson
        (Just "Test")
        (formArgumentToJson (Fix $ FormStringF (Just "Test")))
    test "Tuples" do
      equalJson
        Nothing
        (formArgumentToJson (Fix $ FormTupleF (Fix $ FormIntF Nothing) (Fix $ FormStringF Nothing)))
      equalJson
        Nothing
        (formArgumentToJson (Fix $ FormTupleF (Fix $ FormIntF Nothing) (Fix $ FormStringF (Just "Test"))))
      equalJson
        Nothing
        (formArgumentToJson (Fix $ FormTupleF (Fix $ FormIntF (Just 5)) (Fix $ FormStringF Nothing)))
      equalJson
        (Just "[5,\"Test\"]")
        (formArgumentToJson (Fix $ FormTupleF (Fix $ FormIntF (Just 5)) (Fix $ FormStringF (Just "Test"))))
    test "Arrays" do
      equalJson
        (Just "[1,2,3 ]")
        ( formArgumentToJson
            ( Fix
                $ FormArrayF FormSchemaInt
                    [ Fix $ FormIntF (Just 1)
                    , Fix $ FormIntF (Just 2)
                    , Fix $ FormIntF (Just 3)
                    ]
            )
        )
    test "Values" do
      equalJson
        ( Just
            "{\"getValue\":[{\"unCurrencySymbol\":\"\"},[[{\"unCurrencySymbol\":\"\"},{\"unTokenName\":\"\"},4]]]}"
        )
        ( formArgumentToJson
            ( Fix
                $ FormValueF
                    ( Value
                        { getValue:
                            AssocMap.Map
                              [ Tuple
                                  (CurrencySymbol { unCurrencySymbol: "" })
                                  ( AssocMap.Map
                                      [ Tuple
                                          (TokenName { unTokenName: "" })
                                          (BigInt.fromInt 4)
                                      ]
                                  )
                              ]
                        }
                    )
            )
        )
    test "Objects" do
      equalJson
        (Just "{\"name\":\"Tester\",\"arg\":20}")
        ( formArgumentToJson
            ( Fix
                $ FormObjectF
                    [ Tuple "name" $ Fix (FormStringF (Just "Tester"))
                    , Tuple "arg" $ Fix (FormIntF (Just 20))
                    ]
            )
        )

mkInitialValueTests :: TestSuite
mkInitialValueTests =
  suite "mkInitialValue" do
    test "balance" do
      equal
        ( Value
            { getValue:
                AssocMap.Map
                  [ Tuple ada $ AssocMap.Map [ Tuple adaToken $ BigInt.fromInt 10 ]
                  , Tuple
                      currencies
                      $ AssocMap.Map
                          [ Tuple usdToken $ BigInt.fromInt 10
                          , Tuple eurToken $ BigInt.fromInt 10
                          ]
                  ]
            }
        )
        ( mkInitialValue
            [ KnownCurrency
                { hash: ""
                , friendlyName: "Ada"
                , knownTokens:
                    NonEmptyList $ TokenName { unTokenName: "" } :| Nil
                }
            , KnownCurrency
                { hash: "Currency"
                , friendlyName: "Currencies"
                , knownTokens:
                    NonEmptyList
                      $ TokenName { unTokenName: "USDToken" }
                      :| TokenName { unTokenName: "EURToken" }
                      : Nil
                }
            ]
            (BigInt.fromInt 10)
        )

ada :: CurrencySymbol
ada = CurrencySymbol { unCurrencySymbol: "" }

currencies :: CurrencySymbol
currencies = CurrencySymbol { unCurrencySymbol: "Currency" }

adaToken :: TokenName
adaToken = TokenName { unTokenName: "" }

usdToken :: TokenName
usdToken = TokenName { unTokenName: "USDToken" }

eurToken :: TokenName
eurToken = TokenName { unTokenName: "EURToken" }

equalJson :: Maybe String -> Maybe Json -> Test
equalJson expected actual = equal expected (stringify <$> actual)
