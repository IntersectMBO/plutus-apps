import Cardano.Prelude

import GHC.IO.Encoding
import Test.PlutusExample.Direct.CertifyingAndWithdrawingPlutus qualified
import Test.PlutusExample.Plutus
import Test.PlutusExample.ScriptData
-- import Test.PlutusExample.Direct.TxInLockingPlutus
-- import Test.PlutusExample.SubmitApi.TxInLockingPlutus
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain plutusExampleTests

plutusExampleTests :: TestTree
plutusExampleTests = testGroup "plutus-example"
  [ -- Fails to meet deadline on MacOS for an unknown reason
    testProperty "Plutus.Direct.CertifyingAndWithdrawingPlutus" Test.PlutusExample.Direct.CertifyingAndWithdrawingPlutus.hprop_plutus_certifying_withdrawing
  , testProperty "prop_TxId_Api_Ledger_Plutus_Roundtrip" Test.PlutusExample.Plutus.prop_TxId_Api_Ledger_Plutus_Roundtrip
  , testProperty "prop_TxId_Api_Ledger_Roundtrip" Test.PlutusExample.Plutus.prop_TxId_Api_Ledger_Roundtrip

  , testProperty "prop_ScriptData_MyCustomRedeemer" Test.PlutusExample.ScriptData.prop_ScriptData_MyCustomRedeemer
  , testProperty "prop_ScriptData_MyCustomRedeemer_JSON" Test.PlutusExample.ScriptData.prop_ScriptData_MyCustomRedeemer_JSON
  -- TODO: Re-enable these when plutus-apps is bumped to the latest cardano-node
  -- cardano-testnet needs to parameterize the configuration used to start the testnet.
  --  , testProperty "prop_spending_plutus_script" Test.PlutusExample.Direct.TxInLockingPlutus.prop_spending_plutus_script
  --  , testProperty "prop_submit_api_spending_plutus_script" Test.PlutusExample.SubmitApi.TxInLockingPlutus.prop_submit_api_spending_plutus_script
  ]
