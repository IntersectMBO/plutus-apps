{-# LANGUAGE TypeApplications #-}
module Spec.PubKey(tests, pubKeyTrace) where

import Control.Monad (void)
import Data.Map qualified as Map

import Ledger.Scripts (unitRedeemer)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Typed.Scripts as Scripts
import Plutus.Contract
import Plutus.Contract.Test
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Trace.Emulator qualified as Trace

import Plutus.Contracts.PubKey (PubKeyError, pubKeyContract)

import Test.Tasty

theContract :: Contract () EmptySchema PubKeyError ()
theContract = do
  pk <- ownFirstPaymentPubKeyHash
  (txOutRef, ciTxOut, pkInst) <- pubKeyContract (mockWalletPaymentPubKeyHash w1) (Ada.adaValueOf 10)
  let lookups = maybe mempty (Constraints.unspentOutputs . Map.singleton txOutRef) ciTxOut
              <> Constraints.plutusV2OtherScript (Scripts.validatorScript pkInst)
  void $ submitTxConstraintsWith @Scripts.Any lookups (Constraints.mustSpendScriptOutput txOutRef unitRedeemer <> Constraints.mustBeSignedBy pk)

tests :: TestTree
tests = testGroup "pubkey"
  [ checkPredicate "works like a public key output"
      (walletFundsChange w1 mempty .&&. assertDone theContract (Trace.walletInstanceTag w1) (const True) "pubkey contract not done")
      pubKeyTrace
  ]

-- | Use 'pubKeyContract' to create a script output that works like a
--   public key output, requiring only the right signature on the spending
--   transaction. Then spend the script output.
pubKeyTrace :: Trace.EmulatorTrace ()
pubKeyTrace = do
    _ <- Trace.activateContractWallet w1 theContract
    void $ Trace.waitNSlots 2
