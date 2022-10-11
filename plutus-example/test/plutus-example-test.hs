import Cardano.Prelude

import GHC.IO.Encoding
-- import Test.PlutusExample.Direct.CertifyingAndWithdrawingPlutus qualified
-- import Test.PlutusExample.Direct.ScriptContextEquality qualified
-- import Test.PlutusExample.Direct.ScriptContextEqualityMint qualified
import Test.PlutusExample.Direct.TxInLockingPlutus qualified
import Test.PlutusExample.Plutus qualified
import Test.PlutusExample.Script.TxInLockingPlutus qualified
import Test.PlutusExample.ScriptData qualified
import Test.PlutusExample.SubmitApi.TxInLockingPlutus qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain plutusExampleTests

plutusExampleTests :: TestTree
plutusExampleTests = testGroup "plutus-example"
  [ -- Flaky test:
    -- testPropertyNamed "Plutus.Direct.CertifyingAndWithdrawingPlutus" Test.PlutusExample.Direct.CertifyingAndWithdrawingPlutus.hprop_plutus_certifying_withdrawing
    testPropertyNamed "prop_TxId_Api_Ledger_Plutus_Roundtrip" "prop_TxId_Api_Ledger_Plutus_Roundtrip" Test.PlutusExample.Plutus.prop_TxId_Api_Ledger_Plutus_Roundtrip
  , testPropertyNamed "prop_TxId_Api_Ledger_Roundtrip" "prop_TxId_Api_Ledger_Roundtrip" Test.PlutusExample.Plutus.prop_TxId_Api_Ledger_Roundtrip
  -- TODO: uncomment after https://github.com/input-output-hk/cardano-node/issues/4013
  -- , testPropertyNamed "prop_script_ScriptContextEquality" "hprop_plutus_script_context_equality" Test.PlutusExample.Direct.ScriptContextEquality.hprop_plutus_script_context_equality
  -- , testPropertyNamed "prop_direct_ScriptContextEqualityMint" "hprop_plutus_script_context_mint_equality" Test.PlutusExample.Direct.ScriptContextEqualityMint.hprop_plutus_script_context_mint_equality
  , testPropertyNamed "prop_ScriptData_MyCustomRedeemer" "prop_ScriptData_MyCustomRedeemer" Test.PlutusExample.ScriptData.prop_ScriptData_MyCustomRedeemer
  , testPropertyNamed "prop_ScriptData_MyCustomRedeemer_JSON" "prop_ScriptData_MyCustomRedeemer_JSON" Test.PlutusExample.ScriptData.prop_ScriptData_MyCustomRedeemer_JSON
  , testPropertyNamed "prop_direct_spending_plutus_script" "prop_spending_plutus_script" Test.PlutusExample.Direct.TxInLockingPlutus.prop_spending_plutus_script
  , testPropertyNamed "prop_script_spending_plutus_script" "hprop_plutus" Test.PlutusExample.Script.TxInLockingPlutus.hprop_plutus
  , testPropertyNamed "prop_submit_api_spending_plutus_script" "prop_submit_api_spending_plutus_script" Test.PlutusExample.SubmitApi.TxInLockingPlutus.prop_submit_api_spending_plutus_script
  ]
