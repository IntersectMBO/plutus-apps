import Cardano.Prelude

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Test.PlutusExample.Plutus
import Test.PlutusExample.ScriptData

main :: IO ()
main =
  defaultMain plutusExampleTests

plutusExampleTests :: TestTree
plutusExampleTests = testGroup "plutus-example"
   [ testProperty "prop_TxId_Api_Ledger_Plutus_Roundtrip" Test.PlutusExample.Plutus.prop_TxId_Api_Ledger_Plutus_Roundtrip
   , testProperty "prop_TxId_Api_Ledger_Roundtrip" Test.PlutusExample.Plutus.prop_TxId_Api_Ledger_Roundtrip

   , testProperty "prop_ScriptData_MyCustomRedeemer" Test.PlutusExample.ScriptData.prop_ScriptData_MyCustomRedeemer
   , testProperty "prop_ScriptData_MyCustomRedeemer_JSON" Test.PlutusExample.ScriptData.prop_ScriptData_MyCustomRedeemer_JSON
   -- TODO: Re-enable these when plutus-apps is bumped to the latest cardano-node
   --  , testProperty "prop_spending_plutus_script" Test.PlutusExample.Direct.TxInLockingPlutus.prop_spending_plutus_script
   --  , testProperty "prop_submit_api_spending_plutus_script" Test.PlutusExample.SubmitApi.TxInLockingPlutus.prop_submit_api_spending_plutus_script
   ]
