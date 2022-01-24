module ChainTests
  ( all
  ) where

import Prologue
import Data.Array (mapWithIndex)
import Data.BigInt.Argonaut as BigInt
import Data.Tuple.Nested ((/\))
import Ledger.CardanoWallet (WalletNumber(..))
import Playground.Types (SimulatorWallet(..))
import Plutus.V1.Ledger.Value (CurrencySymbol(..), TokenName(..), Value(..))
import PlutusTx.AssocMap as AssocMap
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transaction.View (extractAmount)

all :: Spec Unit
all =
  describe "Chain" do
    extractAmountsTests

extractAmountsTests :: Spec Unit
extractAmountsTests =
  describe "extractAmount" do
    it "All present"
      $ map (extractAmount (currencies /\ usdToken)) wallets
          `shouldEqual`
            [ Just (BigInt.fromInt 10)
            , Just (BigInt.fromInt 40)
            , Just (BigInt.fromInt 70)
            ]
    it "All missing"
      $ map (extractAmount (currencies /\ adaToken)) wallets
          `shouldEqual`
            [ Nothing
            , Nothing
            , Nothing
            ]
    it "Mixed" do
      map (extractAmount (currencies /\ eurToken)) wallets
        `shouldEqual`
          [ Just (BigInt.fromInt 20)
          , Just (BigInt.fromInt 50)
          , Nothing
          ]
      map (extractAmount (ada /\ adaToken)) wallets
        `shouldEqual`
          [ Nothing
          , Just (BigInt.fromInt 30)
          , Just (BigInt.fromInt 60)
          ]

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
