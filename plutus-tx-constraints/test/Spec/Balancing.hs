{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Spec.Balancing(tests) where

import Control.Lens ((&), (^.))
import Control.Monad.RWS.Strict (ask, evalRWS)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Void (Void)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import Cardano.Api qualified as C
import Cardano.Node.Emulator qualified as E
import Cardano.Node.Emulator.MTL
import Data.Map (Map)
import Data.Sequence (Seq)
import Ledger (CardanoAddress, unitDatum, unitRedeemer)
import Ledger qualified
import Ledger.AddressMap qualified as AM
import Ledger.Test
import Ledger.Tx.CardanoAPI (toCardanoValue)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Typed
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.V1.Generators (someTokenValue)
import Plutus.Script.Utils.V1.Scripts qualified as Scripts
import Plutus.Script.Utils.Value qualified as Value
import PlutusTx (FromData, ToData)


tests :: TestTree
tests =
    testGroup "balancing"
        [ balanceTxnMinAda
        ]

mkTx
  :: ( FromData (Typed.DatumType a)
       , ToData (Typed.DatumType a)
       , ToData (Typed.RedeemerType a)
       )
  => E.Params
  -> Constraints.ScriptLookups a
  -> Constraints.TxConstraints (Typed.RedeemerType a) (Typed.DatumType a)
  -> Constraints.UnbalancedTx
mkTx params lookups constraints =
  Constraints.mkTx params lookups constraints
  & either (error . show) id
  & Constraints.adjustUnbalancedTx (E.emulatorPParams params)
  & either (error . show) snd

submitTxConfirmed :: MonadEmulator m => Constraints.UnbalancedTx -> CardanoAddress -> m ()
submitTxConfirmed (Constraints.UnbalancedCardanoTx utx utxoIndex) addr = do
  let privateKey = lookup addr $ zip E.knownAddresses E.knownPaymentPrivateKeys
  submitUnbalancedTx (Ledger.UtxoIndex utxoIndex) addr utx privateKey
  nextSlot

w1, w2 :: CardanoAddress
w1 : w2 : _ = E.knownAddresses

checkPredicate
  :: String
  -> Map CardanoAddress C.Value
  -> (Seq E.ChainEvent -> Maybe String)
  -> EmulatorM a
  -> TestTree
checkPredicate testName initialDist test contract =
  testCase testName $ do
    let params = def
        (_, log) = evalRWS contract params (emptyEmulatorStateWithInitialDist initialDist)
    for_ (test log) $ \msg ->
      assertFailure $ msg ++ "\n" ++ renderLogs log

balanceTxnMinAda :: TestTree
balanceTxnMinAda =
  let ee = someTokenValue "ee" 1
      ff = someTokenValue "ff" 1
      initialDist = Map.fromList [(w1, either mempty id $ toCardanoValue $ Value.scale 1000 (ee <> ff) <> Ada.lovelaceValueOf 100_000_000)]

  in checkPredicate "balanceTxnMinAda" initialDist (hasValidatedTransactionCountOfTotal 2 2) $ do

        params <- ask
        let
          vHash = Scripts.validatorHash someValidator
          constraints1 =
            Constraints.mustPayToOtherScriptWithDatumInTx
                  vHash
                  unitDatum
                  (Value.scale 100 ff)
            <> Constraints.mustIncludeDatumInTx unitDatum
          utx1 = mkTx @Void params mempty constraints1
        submitTxConfirmed utx1 w1

        utxo <- utxosAt (someCardanoAddress (E.pNetworkId params))
        let
          txOutRef = head (Map.keys utxo)
          constraints2 =
            Constraints.mustSpendScriptOutput txOutRef unitRedeemer
            <> Constraints.mustPayToOtherScriptWithDatumInTx
                  vHash
                  unitDatum
                  (Value.scale 200 ee)
            <> Constraints.mustIncludeDatumInTx unitDatum
          lookups2 =
              Constraints.unspentOutputs utxo
              <> Constraints.plutusV1OtherScript someValidator
          utx2 = mkTx @Void params lookups2 constraints2
        submitTxConfirmed utx2 w1
