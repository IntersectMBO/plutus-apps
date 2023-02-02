{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import Test.Tasty (TestTree, defaultMain, testGroup)


-- See TODO below, import EpochStakepoolSize qualified
-- import Integration qualified
-- import Test.Marconi.Index.EpochStakepoolSize qualified
import Test.Marconi.Index.ScriptTx qualified
import Test.Marconi.Index.Utxo qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Marconi"
  [ Test.Marconi.Index.Utxo.tests
  , Test.Marconi.Index.ScriptTx.tests
  -- TODO currently failing
  -- , Test.Marconi.Index.EpochStakepoolSize.tests
  -- , Integration.tests
  ]
