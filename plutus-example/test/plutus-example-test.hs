import Cardano.Prelude

import GHC.IO.Encoding
-- import Test.PlutusExample.Direct.CertifyingAndWithdrawingPlutus qualified
import Test.PlutusExample.Direct.ScriptContextEquality qualified
import Test.PlutusExample.Direct.ScriptContextEqualityMint qualified
import Test.PlutusExample.Direct.TxInLockingPlutus qualified
import Test.PlutusExample.Plutus qualified
import Test.PlutusExample.Script.TxInLockingPlutus qualified
import Test.PlutusExample.ScriptData qualified
import Test.PlutusExample.SubmitApi.TxInLockingPlutus qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain plutusExampleTests

plutusExampleTests :: TestTree
plutusExampleTests = testGroup "plutus-example"
  [ -- Flaky test:
    -- testProperty "Plutus.Direct.CertifyingAndWithdrawingPlutus" Test.PlutusExample.Direct.CertifyingAndWithdrawingPlutus.hprop_plutus_certifying_withdrawing
    testProperty "prop_TxId_Api_Ledger_Plutus_Roundtrip" Test.PlutusExample.Plutus.prop_TxId_Api_Ledger_Plutus_Roundtrip
  , testProperty "prop_TxId_Api_Ledger_Roundtrip" Test.PlutusExample.Plutus.prop_TxId_Api_Ledger_Roundtrip
  , testProperty "prop_script_ScriptContextEquality" Test.PlutusExample.Direct.ScriptContextEquality.hprop_plutus_script_context_equality
  , testProperty "prop_direct_ScriptContextEqualityMint" Test.PlutusExample.Direct.ScriptContextEqualityMint.hprop_plutus_script_context_mint_equality
  , testProperty "prop_ScriptData_MyCustomRedeemer" Test.PlutusExample.ScriptData.prop_ScriptData_MyCustomRedeemer
  , testProperty "prop_ScriptData_MyCustomRedeemer_JSON" Test.PlutusExample.ScriptData.prop_ScriptData_MyCustomRedeemer_JSON
  , testProperty "prop_direct_spending_plutus_script" Test.PlutusExample.Direct.TxInLockingPlutus.prop_spending_plutus_script
  , testProperty "prop_script_spending_plutus_script" Test.PlutusExample.Script.TxInLockingPlutus.hprop_plutus
  , testProperty "prop_submit_api_spending_plutus_script" Test.PlutusExample.SubmitApi.TxInLockingPlutus.prop_submit_api_spending_plutus_script
  ]
