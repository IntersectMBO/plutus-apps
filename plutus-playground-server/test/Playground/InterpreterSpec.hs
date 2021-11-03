{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Playground.InterpreterSpec
    ( tests
    ) where

import Control.Monad.Except (runExceptT)
import Data.Aeson qualified as JSON
import Data.Aeson.Text qualified as JSON
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Language.Haskell.Interpreter (SourceCode (SourceCode))
import Ledger.Ada qualified as Ada
import Playground.Interpreter (mkExpr, mkRunScript)
import Playground.Types (ContractCall (AddBlocks), Evaluation (Evaluation), PlaygroundError, SimulatorAction,
                         SimulatorWallet (SimulatorWallet), program, sourceCode, wallets)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Wallet.Emulator.Types (WalletNumber (..))

tests :: TestTree
tests = testGroup "Playground.Interpreter" [mkRunScriptTest]

mkRunScriptTest :: TestTree
mkRunScriptTest =
    testGroup
        "mkRunScript"
        [ testCase "Should match a simple template" $ do
              let program =
                      JSON.toJSON
                          ([AddBlocks 2, AddBlocks 4] :: [SimulatorAction])
                  wallets =
                      [ SimulatorWallet (WalletNumber 1) (Ada.toValue 5)
                      , SimulatorWallet (WalletNumber 2) (Ada.toValue 10)
                      ]
              expr :: Either PlaygroundError String <-
                  runExceptT $
                  mkExpr
                      (Evaluation {sourceCode = sourceCode1, wallets, program})
              assertEqual
                  ""
                  (Right $
                   Text.stripEnd $
                   Text.unlines
                       [ "module Main where"
                       , ""
                       , "foo :: Int"
                       , "foo = 5"
                       , ""
                       , ""
                       , "main :: IO ()"
                       , "main = printJson $ stage endpoints " <>
                         toJSONString program <>
                         " " <>
                         toJSONString wallets
                       ])
                  (mkRunScript sourceCode1 . Text.pack <$> expr)
        ]

sourceCode1 :: SourceCode
sourceCode1 =
    SourceCode $ Text.unlines ["module Game where", "", "foo :: Int", "foo = 5"]

toJSONString :: JSON.ToJSON a => a -> Text
toJSONString =
    TL.toStrict .
    JSON.encodeToLazyText . JSON.String . TL.toStrict . JSON.encodeToLazyText
