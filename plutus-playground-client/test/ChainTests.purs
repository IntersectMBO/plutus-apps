module ChainTests
  ( all
  ) where

import Prologue
import Data.Array (mapWithIndex)
import Data.BigInt.Argonaut as BigInt
import Data.Tuple.Nested ((/\))
import PlutusTx.AssocMap as AssocMap
import Plutus.V1.Ledger.Value (CurrencySymbol(..), TokenName(..), Value(..))
import Playground.Types (SimulatorWallet(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Transaction.View (extractAmount)
import Ledger.CardanoWallet (WalletNumber(..))

all :: TestSuite
all =
  suite "Chain" do
    extractAmountsTests

extractAmountsTests :: TestSuite
extractAmountsTests =
  suite "extractAmount" do
    test "All present"
      $ equal
          [ Just (BigInt.fromInt 10)
          , Just (BigInt.fromInt 40)
          , Just (BigInt.fromInt 70)
          ]
          (map (extractAmount (currencies /\ usdToken)) wallets)
    test "All missing"
      $ equal
          [ Nothing
          , Nothing
          , Nothing
          ]
          (map (extractAmount (currencies /\ adaToken)) wallets)
    test "Mixed" do
      equal
        [ Just (BigInt.fromInt 20)
        , Just (BigInt.fromInt 50)
        , Nothing
        ]
        (map (extractAmount (currencies /\ eurToken)) wallets)
      equal
        [ Nothing
        , Just (BigInt.fromInt 30)
        , Just (BigInt.fromInt 60)
        ]
        (map (extractAmount (ada /\ adaToken)) wallets)

wallets :: Array SimulatorWallet
wallets =
  mapWithIndex
    ( \id value ->
        SimulatorWallet
          { simulatorWalletWallet: WalletNumber { getWallet: BigInt.fromInt id }
          , simulatorWalletBalance: value
          }
    )
    values

values :: Array Value
values =
  [ Value
      { getValue:
          AssocMap.Map
            [ currencies
                /\ AssocMap.Map
                    [ usdToken /\ BigInt.fromInt 10
                    , eurToken /\ BigInt.fromInt 20
                    ]
            ]
      }
  , Value
      { getValue:
          AssocMap.Map
            [ ada /\ AssocMap.Map [ adaToken /\ BigInt.fromInt 30 ]
            , currencies
                /\ AssocMap.Map
                    [ usdToken /\ BigInt.fromInt 40
                    , eurToken /\ BigInt.fromInt 50
                    ]
            ]
      }
  , Value
      { getValue:
          AssocMap.Map
            [ ada /\ AssocMap.Map [ adaToken /\ BigInt.fromInt 60 ]
            , currencies
                /\ AssocMap.Map
                    [ usdToken /\ BigInt.fromInt 70
                    ]
            ]
      }
  ]

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
