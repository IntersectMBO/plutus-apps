{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Spec.Balancing(tests) where

import Cardano.Api qualified as C
import Cardano.Node.Emulator qualified as E
import Cardano.Node.Emulator.MTL (EmulatorError, EmulatorLogs, EmulatorM, MonadEmulator,
                                  emptyEmulatorStateWithInitialDist, nextSlot, submitUnbalancedTx, utxosAt)
import Cardano.Node.Emulator.MTL.Test (hasValidatedTransactionCountOfTotal, renderLogs)
import Control.Lens ((&))
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.RWS.Strict (ask, evalRWS)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Void (Void)
import Ledger (CardanoAddress, CardanoTx, toPlutusAddress, unitDatum, unitRedeemer)
import Ledger qualified
import Ledger.Test (coinMintingPolicyV1, someAddress, someCardanoAddress, someValidator)
import Ledger.Tx.CardanoAPI (toCardanoValue)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Typed
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.V1.Generators (someTokenValue)
import Plutus.Script.Utils.V1.Scripts qualified as Scripts
import Plutus.Script.Utils.V1.Typed.Scripts qualified as TypedScripts
import Plutus.Script.Utils.Value qualified as Value
import PlutusTx (FromData, ToData, toBuiltinData)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

tests :: TestTree
tests =
    testGroup "balancing"
        [ balanceTxnMinAda
        , balanceTxnMinAda2
        , balanceTxnNoExtraOutput
        , balanceTxnCollateral
        , balanceTxnCollateralFail
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

submitTxConfirmed :: MonadEmulator m => CardanoAddress -> Constraints.UnbalancedTx -> m CardanoTx
submitTxConfirmed addr (Constraints.UnbalancedCardanoTx utx utxoIndex) = do
  let privateKey = lookup addr $ zip E.knownAddresses E.knownPaymentPrivateKeys
  tx <- submitUnbalancedTx utxoIndex addr privateKey utx
  nextSlot
  pure tx

submitTxConstraints
  :: MonadEmulator m
  => CardanoAddress
  -> Constraints.ScriptLookups Void
  -> Constraints.TxConstraints Void Void
  -> m CardanoTx
submitTxConstraints addr lookups constraints = do
  params <- ask
  submitTxConfirmed addr
    $ mkTx @Void params lookups constraints

w1, w2 :: CardanoAddress
w1 : w2 : _ = E.knownAddresses

checkPredicate
  :: String
  -> Map CardanoAddress C.Value
  -> (EmulatorLogs -> Either EmulatorError a -> Maybe String)
  -> EmulatorM a
  -> TestTree
checkPredicate testName initialDist test contract =
  testCase testName $ do
    let params = def
        (res, emulatorLogs) = evalRWS (runExceptT contract) params (emptyEmulatorStateWithInitialDist initialDist)
    for_ (test emulatorLogs res) $ \msg ->
      assertFailure $ Text.unpack (renderLogs emulatorLogs) ++ "\n" ++ msg

checkLogPredicate
  :: String
  -> Map CardanoAddress C.Value
  -> (EmulatorLogs -> Maybe String)
  -> EmulatorM a
  -> TestTree
checkLogPredicate testName initialDist test =
  checkPredicate testName initialDist (const . test)

checkResultPredicate
  :: Show a
  => String
  -> Map CardanoAddress C.Value
  -> (a -> Bool)
  -> EmulatorM a
  -> TestTree
checkResultPredicate testName initialDist test =
  checkPredicate testName initialDist $ \_ res ->
    case res of
      Right a  -> if test a then Nothing else Just $ "Unexpected result: " ++ show a
      Left err -> Just $ "Error:" ++ show err

balanceTxnMinAda :: TestTree
balanceTxnMinAda =
  let ee = someTokenValue "ee" 1
      ff = someTokenValue "ff" 1
      initialDist = Map.fromList [(w1, either mempty id $ toCardanoValue $ Value.scale 1000 (ee <> ff) <> Ada.lovelaceValueOf 100_000_000)]

  in checkLogPredicate "balanceTxnMinAda" initialDist (hasValidatedTransactionCountOfTotal 2 2) $ do

        let
          vHash = Scripts.validatorHash someValidator
          constraints1 =
            Constraints.mustPayToOtherScriptWithDatumInTx
                  vHash
                  unitDatum
                  (Value.scale 100 ff)
            <> Constraints.mustIncludeDatumInTx unitDatum
        void $ submitTxConstraints w1 mempty constraints1

        params <- ask
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
        submitTxConstraints w1 lookups2 constraints2


balanceTxnMinAda2 :: TestTree
balanceTxnMinAda2 =
  let vA n = someTokenValue "A" n
      vB n = someTokenValue "B" n
      mps  = TypedScripts.mkForwardingMintingPolicy vHash
      vL n = Value.singleton (Value.mpsSymbol $ Scripts.mintingPolicyHash mps) "L" n
      initialDist = Map.fromList [(w1, either mempty id $ toCardanoValue $ vA 1 <> vB 2 <> Ada.lovelaceValueOf 100_000_000)]
      vHash = Scripts.validatorHash someValidator
      payToWallet w = Constraints.mustPayToAddress $ toPlutusAddress w

      setupContract = do
        -- Make sure there is a utxo with 1 A, 1 B, and 4 ada at w2
        void $ submitTxConstraints w1 mempty
          (payToWallet w2 ( vA 1 <> vB 1 <> Value.scale 2 (Ada.toValue Ledger.minAdaTxOutEstimated)))
        -- Make sure there is a UTxO with 1 B and datum () at the script
        submitTxConstraints w1 mempty
          ( Constraints.mustPayToOtherScriptWithDatumInTx vHash unitDatum (vB 1)
          <> Constraints.mustIncludeDatumInTx unitDatum
          )
        -- utxo0 @ wallet2 = 1 A, 1 B, 4 Ada
        -- utxo1 @ script  = 1 B, 2 Ada

      wallet2Contract = do
        params <- ask
        utxos <- utxosAt $ someCardanoAddress (E.pNetworkId params)
        let txOutRef = case Map.keys utxos of
                          (x:_) -> x
                          []    -> error $ "there's no utxo at the address " <> show someAddress
            lookups =  Constraints.unspentOutputs utxos
                    <> Constraints.plutusV1OtherScript someValidator
                    <> Constraints.plutusV1MintingPolicy mps
            datum = Ledger.Datum $ toBuiltinData (0 :: Integer)
            constraints = Constraints.mustSpendScriptOutput txOutRef unitRedeemer                                        -- spend utxo1
                        <> Constraints.mustPayToOtherScriptWithDatumInTx vHash unitDatum (vB 1)                                       -- 2 ada and 1 B to script
                        <> Constraints.mustPayToOtherScriptWithDatumInTx vHash datum (vB 1) -- 2 ada and 1 B to script (different datum)
                        <> Constraints.mustIncludeDatumInTx unitDatum
                        <> Constraints.mustIncludeDatumInTx datum
                        <> Constraints.mustMintValue (vL 1) -- 1 L and 2 ada to wallet2
        submitTxConstraints w2 lookups constraints

      contract :: EmulatorM ()
      contract = do
        void setupContract
        void wallet2Contract

  in checkLogPredicate "balancing doesn't create outputs with no Ada (2)" initialDist (hasValidatedTransactionCountOfTotal 3 3) contract


balanceTxnNoExtraOutput :: TestTree
balanceTxnNoExtraOutput =
  let vL n = Value.singleton (Scripts.scriptCurrencySymbol coinMintingPolicyV1) "coinToken" n
      initialDist = Map.fromList [(w1, either mempty id $ toCardanoValue $ Ada.lovelaceValueOf 100_000_000)]

      mintingOperation :: EmulatorM Int
      mintingOperation = do
        let val = vL 200
            lookups = Constraints.plutusV1MintingPolicy coinMintingPolicyV1
            constraints = Constraints.mustMintValue val
                <> Constraints.mustPayToAddress (toPlutusAddress w1) (val <> Ada.toValue Ledger.minAdaTxOutEstimated)

        tx <- submitTxConstraints w1 lookups constraints
        pure (length $ Ledger.getCardanoTxOutRefs tx)

  in checkResultPredicate "balancing doesn't create extra output" initialDist (== 2) mintingOperation

balanceTxnCollateralTest :: TestName -> Int -> Integer -> TestTree
balanceTxnCollateralTest name count outputLovelace =
    let ee = someTokenValue "ee" 1
        ff = someTokenValue "ff" 1
        initialDist = Map.fromList [(w1, either mempty id $ toCardanoValue $ Value.scale 1000 (ee <> ff) <> Ada.lovelaceValueOf 3_900_000)]
        vHash = Scripts.validatorHash someValidator

        contract = do
          utxos <- utxosAt w1
          let constraints1 =
                  Constraints.mustPayToOtherScriptWithDatumInTx
                      vHash
                      unitDatum
                      (Value.scale 100 ff <> Ada.lovelaceValueOf outputLovelace)
              lookups = Constraints.unspentOutputs utxos
          submitTxConstraints w1 lookups constraints1

    in checkLogPredicate name initialDist (hasValidatedTransactionCountOfTotal count count) contract

balanceTxnCollateral :: TestTree
balanceTxnCollateral = balanceTxnCollateralTest "can balance collateral with non-ada utxo" 1 1_200_000

balanceTxnCollateralFail :: TestTree
balanceTxnCollateralFail = balanceTxnCollateralTest "won't create return collateral with too little ada (utxoCostPerWord)" 0 2_000_000
