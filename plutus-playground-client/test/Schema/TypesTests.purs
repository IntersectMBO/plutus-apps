module Schema.TypesTests
  ( all
  ) where

import Prologue
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut.Core (Json, stringify)
import Data.BigInt.Argonaut as BigInt
import Data.Functor.Foldable (Fix(..))
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Ledger.CardanoWallet (WalletNumber(..))
import Playground.Types (ContractCall(..), FunctionSchema(..), KnownCurrency(..))
import Plutus.V1.Ledger.Value (CurrencySymbol(..), TokenName(..), Value(..))
import PlutusTx.AssocMap as AssocMap
import Schema (FormSchema(..), FormArgumentF(..))
import Schema.Types (FormArgument, formArgumentToJson, mkInitialValue, toArgument)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Validation (ValidationError(..), validate, withPath)
import Wallet.Types (EndpointDescription(..))

all :: Spec Unit
all =
  describe "Schema.Types" do
    validateSpec
    toArgumentSpec
    formArgumentToJsonSpec
    mkInitialValueSpec

validateSpec :: Spec Unit
validateSpec = do
  it "No validation errors" do
    isValid $ AddBlocks { blocks: one }
    isValid $ makeTestAction $ Fix $ FormIntF (Just 5)
    isValid $ makeTestAction $ Fix $ FormIntegerF (Just (BigInt.fromInt 5))
    isValid $ makeTestAction $ Fix $ FormStringF (Just "TEST")
    isValid $ makeTestAction $ Fix $ FormTupleF (Fix $ FormIntF (Just 5)) (Fix $ FormIntF (Just 6))
    isValid $ makeTestAction $ Fix $ FormArrayF FormSchemaInt []
    isValid $ makeTestAction $ Fix $ FormObjectF []
  --
  it "Validation errors" do
    validate (makeTestAction $ Fix $ FormUnsupportedF "Test case.") `shouldEqual` [ withPath [] Unsupported ]
    validate (makeTestAction $ Fix $ FormIntF Nothing) `shouldEqual` [ withPath [] Required ]
    validate (makeTestAction $ Fix $ FormIntegerF Nothing) `shouldEqual` [ withPath [] Required ]
    validate (makeTestAction $ Fix $ FormStringF Nothing) `shouldEqual` [ withPath [] Required ]
    validate (makeTestAction $ Fix $ FormTupleF (Fix $ FormIntF Nothing) (Fix $ FormUnsupportedF "Test."))
      `shouldEqual`
        [ withPath [ "_1" ] Required
        , withPath [ "_2" ] Unsupported
        ]
    validate (makeTestAction $ Fix $ FormTupleF (Fix $ FormIntF Nothing) (Fix $ FormIntF (Just 5))) `shouldEqual` [ withPath [ "_1" ] Required ]
    validate (makeTestAction $ Fix $ FormTupleF (Fix $ FormIntF (Just 5)) (Fix $ FormIntF Nothing))
      `shouldEqual`
        [ withPath [ "_2" ] Required ]
    validate
      ( makeTestAction
          $ Fix
          $ FormArrayF FormSchemaInt
              [ Fix $ FormIntF (Just 5)
              , Fix $ FormIntF (Just 6)
              , Fix $ FormIntF Nothing
              , Fix $ FormIntF (Just 7)
              ]
      )
      `shouldEqual`
        [ withPath [ "2" ] Required ]
    ( validate
        ( makeTestAction
            $ Fix
            $ FormObjectF
                [ Tuple "name" (Fix $ FormStringF Nothing)
                , Tuple "test" (Fix $ FormIntF (Just 5))
                ]
        )
    )
      `shouldEqual`
        [ withPath [ "name" ] Required ]

toArgumentSpec :: Spec Unit
toArgumentSpec = do
  describe "toArgument" do
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
    it "FormIntF" do
      toArgument initialValue FormSchemaInt `shouldEqual` Fix (FormIntF Nothing)
    it "Value" do
      toArgument initialValue FormSchemaValue `shouldEqual` Fix (FormValueF initialValue)

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
isValid = validate >>> flip shouldEqual []

formArgumentToJsonSpec :: Spec Unit
formArgumentToJsonSpec = do
  describe "FormArgument to JSON" do
    it "Ints" do
      equalJson
        Nothing
        (formArgumentToJson (Fix $ FormIntF Nothing))
      equalJson
        (Just "5")
        (formArgumentToJson (Fix $ FormIntF (Just 5)))
    it "BigInts" do
      equalJson
        Nothing
        (formArgumentToJson (Fix $ FormIntegerF Nothing))
      equalJson
        (Just "5")
        (formArgumentToJson (Fix $ FormIntegerF (Just (BigInt.fromInt 5))))
    it "Strings" do
      equalJson
        Nothing
        (formArgumentToJson (Fix $ FormStringF Nothing))
      equalJson
        (Just "\"Test\"")
        (formArgumentToJson (Fix $ FormStringF (Just "Test")))
    it "Tuples" do
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
    it "Arrays" do
      equalJson
        (Just "[1,2,3]")
        ( formArgumentToJson
            ( Fix
                $ FormArrayF FormSchemaInt
                    [ Fix $ FormIntF (Just 1)
                    , Fix $ FormIntF (Just 2)
                    , Fix $ FormIntF (Just 3)
                    ]
            )
        )
    it "Values" do
      equalJson
        ( Just
            "{\"getValue\":[[{\"unCurrencySymbol\":\"\"},[[{\"unTokenName\":\"\"},4]]]]}"
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
    it "Objects" do
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

mkInitialValueSpec :: Spec Unit
mkInitialValueSpec =
  describe "mkInitialValue" do
    it "balance" do
      mkInitialValue
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
        `shouldEqual`
          Value
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

equalJson :: forall m. MonadThrow Error m => Maybe String -> Maybe Json -> m Unit
equalJson expected actual = (stringify <$> actual) `shouldEqual` expected
