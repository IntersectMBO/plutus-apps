{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Spec.Contract(tests, loopCheckpointContract, initial, upd) where

import Control.Lens hiding ((.>))
import Control.Monad (forever, replicateM_, void)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (catchError)
import Control.Monad.Freer.Extras.Log (LogLevel (Debug))
import Control.Monad.Freer.Extras.Log qualified as Log
import Data.Functor.Apply ((.>))
import Data.Map qualified as Map
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)

import Ledger (Address, PaymentPubKeyHash)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (getCardanoTxId)
import Plutus.Contract as Con
import Plutus.Contract.State qualified as State
import Plutus.Contract.Test (Shrinking (DoShrink, DontShrink), TracePredicate, assertAccumState, assertContractError,
                             assertDone, assertInstanceLog, assertNoFailedTransactions, assertResumableResult,
                             assertUserLog, checkEmulatorFails, checkPredicateOptions, defaultCheckOptions,
                             endpointAvailable, minLogLevel, mockWalletPaymentPubKeyHash, not, w1, w2, w3,
                             waitingForSlot, walletFundsChange, (.&&.))
import Plutus.Contract.Types (ResumableResult (ResumableResult, _finalState), responses)
import Plutus.Contract.Util (loopM)
import Plutus.Script.Utils.V1.Scripts (datumHash)
import Plutus.Trace qualified as Trace
import Plutus.Trace.Emulator (ContractInstanceTag, EmulatorTrace, activateContract, activeEndpoints, callEndpoint)
import Plutus.Trace.Emulator.Types (ContractInstanceLog (_cilMessage),
                                    ContractInstanceMsg (ContractLog, CurrentRequests, HandledRequest, ReceiveEndpointCall, Started, StoppedNoError),
                                    ContractInstanceState (ContractInstanceState, instContractState),
                                    UserThreadMsg (UserLog))
import Plutus.V1.Ledger.Api (Datum (Datum), DatumHash, Validator)
import Plutus.V1.Ledger.Scripts qualified as Ledger
import Plutus.V1.Ledger.Tx (TxOut (txOutDatumHash))
import PlutusTx qualified
import Prelude hiding (not)
import Wallet.Emulator qualified as EM
import Wallet.Emulator.Wallet (mockWalletAddress)

import Plutus.ChainIndex.Types (RollbackState (Committed), TxOutState (Spent, Unspent), TxOutStatus, TxStatus,
                                TxValidity (TxValid))
import Plutus.Contract.Effects (ActiveEndpoint (ActiveEndpoint, aeDescription, aeMetadata))

tests :: TestTree
tests =
    let run :: String -> TracePredicate -> EmulatorTrace () -> _
        run = checkPredicateOptions (defaultCheckOptions & minLogLevel .~ Debug)

        check :: String -> Contract () Schema ContractError () -> _ -> _
        check nm contract pred = run nm (pred contract) (void $ activateContract w1 contract tag)

        tag :: ContractInstanceTag
        tag = "instance 1"

    in
    testGroup "contracts"
        [ check "awaitSlot" (void $ awaitSlot 10) $ \con ->
            waitingForSlot con tag 10

        , check "selectEither" (void $ awaitPromise $ selectEither (isSlot 10) (isSlot 5)) $ \con ->
            waitingForSlot con tag 5

        , check "both" (void $ awaitPromise $ Con.both (isSlot 10) (isSlot 20)) $ \con ->
            waitingForSlot con tag 10

        , check "both (2)" (void $ awaitPromise $ Con.both (isSlot 10) (isSlot 20)) $ \con ->
            waitingForSlot con tag 20

        , check "watchAddressUntilSlot" (void $ watchAddressUntilSlot someAddress 5) $ \con ->
            waitingForSlot con tag 5

        , check "endpoint" (void $ awaitPromise $ endpoint @"ep" pure) $ \con ->
            endpointAvailable @"ep" con tag

        , check "forever" (forever $ awaitPromise $ endpoint @"ep" pure) $ \con ->
            endpointAvailable @"ep" con tag

        , let
            oneTwo :: Promise () Schema ContractError Int = endpoint @"1" pure .> endpoint @"2" pure .> endpoint @"4" pure
            oneThree :: Promise () Schema ContractError Int = endpoint @"1" pure .> endpoint @"3" pure .> endpoint @"4" pure
            con = selectList [void oneTwo, void oneThree]
          in
            run "alternative"
                (endpointAvailable @"2" con tag
                    .&&. not (endpointAvailable @"3" con tag))
                $ do
                    hdl <- activateContract w1 con tag
                    callEndpoint @"1" hdl 1

        , let theContract :: Contract () Schema ContractError () = void $ awaitPromise $ endpoint @"1" @Int pure .> endpoint @"2" @Int pure
          in run "call endpoint (1)"
                (endpointAvailable @"1" theContract tag)
                (void $ activateContract w1 theContract tag)

        , let theContract :: Contract () Schema ContractError () = void $ awaitPromise $ endpoint @"1" @Int pure .> endpoint @"2" @Int pure
          in run "call endpoint (2)"
                (endpointAvailable @"2" theContract tag
                    .&&. not (endpointAvailable @"1" theContract tag))
                (activateContract w1 theContract tag >>= \hdl -> callEndpoint @"1" hdl 1)

        , let theContract :: Contract () Schema ContractError () = void $ awaitPromise $ endpoint @"1" @Int pure .> endpoint @"2" @Int pure
          in run "call endpoint (3)"
                (not (endpointAvailable @"2" theContract tag)
                    .&&. not (endpointAvailable @"1" theContract tag))
                (activateContract w1 theContract tag >>= \hdl -> callEndpoint @"1" hdl 1 >> callEndpoint @"2" hdl 2)

        , let theContract :: Contract () Schema ContractError [ActiveEndpoint] = awaitPromise $ endpoint @"5" @[ActiveEndpoint] pure
              expected = ActiveEndpoint{ aeDescription = EndpointDescription "5", aeMetadata = Nothing}
          in run "active endpoints"
                (assertDone theContract tag ((==) [expected]) "should be done")
                $ do
                    hdl <- activateContract w1 theContract tag
                    _ <- Trace.waitNSlots 1
                    eps <- activeEndpoints hdl
                    void $ callEndpoint @"5" hdl eps

        , let theContract :: Contract () Schema ContractError () = void $ submitTx mempty >> watchAddressUntilSlot someAddress 20
          in run "submit tx"
                (waitingForSlot theContract tag 20)
                (void $ activateContract w1 theContract tag)

        , let smallTx = Constraints.mustPayToPubKey (mockWalletPaymentPubKeyHash w2) (Ada.adaValueOf 10)
              theContract :: Contract () Schema ContractError () = submitTx smallTx >>= awaitTxConfirmed . getCardanoTxId >> submitTx smallTx >>= awaitTxConfirmed . getCardanoTxId
          in run "handle several blockchain events"
                (walletFundsChange w1 (Ada.adaValueOf (-20))
                    .&&. assertNoFailedTransactions
                    .&&. assertDone theContract tag (const True) "all blockchain events should be processed")
                (void $ activateContract w1 theContract tag >> Trace.waitUntilSlot 3)

        , let l = endpoint @"1" pure .> endpoint @"2" pure
              r = endpoint @"3" pure .> endpoint @"4" pure
              theContract :: Contract () Schema ContractError () = void . awaitPromise $ selectEither l r
          in run "select either"
                (assertDone theContract tag (const True) "left branch should finish")
                (activateContract w1 theContract tag >>= (\hdl -> callEndpoint @"1" hdl 1 >> callEndpoint @"2" hdl 2))

        , let theContract :: Contract () Schema ContractError () = void $ loopM (\_ -> fmap Left . awaitPromise $ endpoint @"1" @Int pure) 0
          in run "loopM"
                (endpointAvailable @"1" theContract tag)
                (void $ activateContract w1 theContract tag >>= \hdl -> callEndpoint @"1" hdl 1)

        , let theContract :: Contract () Schema ContractError () =
                  void $ throwing Con._ContractError $ OtherContractError "error"
          in run "throw an error"
                (assertContractError
                    theContract
                    tag
                    (\case { OtherContractError "error" -> True; _ -> False })
                    "failed to throw error")
                (void $ activateContract w1 theContract tag)

        , run "pay to wallet"
            (walletFundsChange w1 (Ada.adaValueOf (-20))
                .&&. walletFundsChange w2 (Ada.adaValueOf 20)
                .&&. assertNoFailedTransactions)
            (void $ Trace.payToWallet w1 w2 (Ada.adaValueOf 20))

        , let theContract :: Contract () Schema ContractError () =
                  void $ awaitUtxoProduced (mockWalletAddress w2)
          in run "await utxo produced"
            (assertDone theContract tag (const True) "should receive a notification")
            (void $ do
                activateContract w1 theContract tag
                Trace.payToWallet w1 w2 (Ada.adaValueOf 20)
                Trace.waitNSlots 1
            )

        , let theContract :: Contract () Schema ContractError () =
                  void ( utxosAt (mockWalletAddress w1)
                     >>= awaitUtxoSpent . fst . head . Map.toList
                       )
          in run "await txout spent"
            (assertDone theContract tag (const True) "should receive a notification")
            (void $ do
                activateContract w1 theContract tag
                Trace.payToWallet w1 w2 (Ada.adaValueOf 20)
                Trace.waitNSlots 1
            )

        , let theContract :: Contract () Schema ContractError PaymentPubKeyHash = ownPaymentPubKeyHash
          in run "own public key"
                (assertDone theContract tag (== mockWalletPaymentPubKeyHash w2) "should return the wallet's public key")
                (void $ activateContract w2 (void theContract) tag)

        , let payment = Constraints.mustPayToPubKey (mockWalletPaymentPubKeyHash w2) (Ada.adaValueOf 10)
              theContract :: Contract () Schema ContractError () = submitTx payment >>= awaitTxConfirmed . Ledger.getCardanoTxId
          in run "await tx confirmed"
            (assertDone theContract tag (const True) "should be done")
            (activateContract w1 theContract tag >> void (Trace.waitNSlots 1))

        , let payment = Constraints.mustPayToPubKey (mockWalletPaymentPubKeyHash w2) (Ada.adaValueOf 10)
              theContract :: Contract () Schema ContractError TxStatus =
                submitTx payment >>= awaitTxStatusChange . Ledger.getCardanoTxId
          in run "await change in tx status"
            (assertDone theContract tag ((==) (Committed TxValid ())) "should be done")
            (activateContract w1 theContract tag >> void (Trace.waitNSlots 1))

        , let c :: Contract [Maybe DatumHash] Schema ContractError () = do
                let w2PubKeyHash = mockWalletPaymentPubKeyHash w2
                let payment = Constraints.mustPayWithDatumToPubKey w2PubKeyHash datum (Ada.adaValueOf 10)
                tx <- submitTx payment
                let txOuts = fmap fst $ Ledger.getCardanoTxOutRefs tx
                -- tell the tx out' datum hash that was specified by 'mustPayWithDatumToPubKey'
                tell [txOutDatumHash (txOuts !! 1)]

              datum = Datum $ PlutusTx.toBuiltinData (23 :: Integer)
              isExpectedDatumHash [Just hash] = hash == datumHash datum
              isExpectedDatumHash _           = False

          in run "mustPayWithDatumToPubKey produces datum in TxOut"
            ( assertAccumState c tag isExpectedDatumHash "should be done"
            ) $ do
              _ <- activateContract w1 c tag
              void (Trace.waitNSlots 2)

        -- verify that 'matchInputOutput' doesn't thrown 'InOutTypeMismatch' error
        -- in case of two transactions with 'mustPayWithDatumToPubKey'
        , let c1 :: Contract [Maybe DatumHash] Schema ContractError () = do
                let w2PubKeyHash = mockWalletPaymentPubKeyHash w2
                let payment = Constraints.mustPayWithDatumToPubKey w2PubKeyHash datum1 (Ada.adaValueOf 10)
                void $ submitTx payment
              c2 :: Contract [Maybe DatumHash] Schema ContractError () = do
                let w3PubKeyHash = mockWalletPaymentPubKeyHash w3
                let payment = Constraints.mustPayWithDatumToPubKey w3PubKeyHash datum2 (Ada.adaValueOf 50)
                void $ submitTx payment

              datum1 = Datum $ PlutusTx.toBuiltinData (23 :: Integer)
              datum2 = Datum $ PlutusTx.toBuiltinData (42 :: Integer)

          in run "mustPayWithDatumToPubKey doesn't throw 'InOutTypeMismatch' error"
            ( assertNoFailedTransactions ) $ do
              _ <- activateContract w1 c1 tag
              void (Trace.waitNSlots 2)
              _ <- activateContract w2 c2 tag
              void (Trace.waitNSlots 2)

        , let c :: Contract [TxOutStatus] Schema ContractError () = do
                -- Submit a payment tx of 10 lovelace to W2.
                let w2PubKeyHash = mockWalletPaymentPubKeyHash w2
                let payment = Constraints.mustPayToPubKey w2PubKeyHash
                                                          (Ada.adaValueOf 10)
                tx <- submitTx payment
                -- There should be 2 utxos. We suppose the first belongs to the
                -- wallet calling the contract and the second one to W2.
                let utxo = head $ fmap snd $ Ledger.getCardanoTxOutRefs tx
                -- We wait for W1's utxo to change status. It should be of
                -- status confirmed unspent.
                s <- awaitTxOutStatusChange utxo
                tell [s]

                -- We submit another tx which spends the utxo belonging to the
                -- contract's caller. It's status should be changed eventually
                -- to confirmed spent.
                pubKeyHash <- ownPaymentPubKeyHash
                ciTxOutM <- unspentTxOutFromRef utxo
                let lookups = Constraints.unspentOutputs (maybe mempty (Map.singleton utxo) ciTxOutM)
                submitTxConstraintsWith @Void lookups $ Constraints.mustSpendPubKeyOutput utxo
                                                     <> Constraints.mustBeSignedBy pubKeyHash
                s <- awaitTxOutStatusChange utxo
                tell [s]

              isExpectedAccumState [Committed TxValid Unspent, Committed TxValid (Spent _)] = True
              isExpectedAccumState _                                                        = False

          in run "await change in tx out status"
            ( assertAccumState c tag isExpectedAccumState "should be done"
            ) $ do
              _ <- activateContract w1 c tag
              void (Trace.waitNSlots 2)

        , run "checkpoints"
            (not (endpointAvailable @"2" checkpointContract tag) .&&. endpointAvailable @"1" checkpointContract tag)
            (void $ activateContract w1 checkpointContract tag >>= \hdl -> callEndpoint @"1" hdl 1 >> callEndpoint @"2" hdl 1)

        , run "error handling & checkpoints"
            (assertDone errorContract tag (\i -> i == 11) "should finish")
            (void $ activateContract w1 (void errorContract) tag >>= \hdl -> callEndpoint @"1" hdl 1 >> callEndpoint @"2" hdl 10 >> callEndpoint @"3" hdl 11)

        , run "loop checkpoint"
            (assertDone loopCheckpointContract tag (\i -> i == 4) "should finish"
            .&&. assertResumableResult loopCheckpointContract tag DoShrink (null . view responses) "should collect garbage"
            .&&. assertResumableResult loopCheckpointContract tag DontShrink ((==) 4 . length . view responses) "should keep everything"
            )
            $ do
                hdl <- activateContract w1 loopCheckpointContract tag
                replicateM_ 4 $ callEndpoint @"1" hdl 1

        , let theContract :: Contract () Schema ContractError () = logInfo @String "waiting for endpoint 1" >> awaitPromise (endpoint @"1" (logInfo . (<>) "Received value: " . show))
              matchLogs :: [EM.EmulatorTimeEvent ContractInstanceLog] -> Bool
              matchLogs lgs =
                  case _cilMessage . EM._eteEvent <$> lgs of
                            [ Started, ContractLog "waiting for endpoint 1", CurrentRequests [_], ReceiveEndpointCall{}, ContractLog "Received value: 27", HandledRequest _, CurrentRequests [], StoppedNoError ] -> True
                            _ -> False
          in run "contract logs"
                (assertInstanceLog tag matchLogs)
                (void $ activateContract w1 theContract tag >>= \hdl -> callEndpoint @"1" hdl 27)

        , let theContract :: Contract () Schema ContractError () = logInfo @String "waiting for endpoint 1" >> awaitPromise (endpoint @"1" (logInfo . (<>) "Received value: " . show))
              matchLogs :: [EM.EmulatorTimeEvent UserThreadMsg] -> Bool
              matchLogs lgs =
                  case EM._eteEvent <$> lgs of
                            [ UserLog "Received contract state", UserLog "Final state: Right Nothing"] -> True
                            _                                                                          -> False
          in run "contract state"
                (assertUserLog matchLogs)
                $ do
                    hdl <- Trace.activateContractWallet w1 theContract
                    Trace.waitNSlots 1
                    ContractInstanceState{instContractState=ResumableResult{_finalState}} <- Trace.getContractState hdl
                    Log.logInfo @String "Received contract state"
                    Log.logInfo @String $ "Final state: " <> show _finalState

        , let theContract :: Contract () Schema ContractError () = void $ awaitSlot 10
              emTrace = do
                void $ Trace.assert "Always succeeds" $ const True
                void $ Trace.waitNSlots 10
          in run "assert succeeds" (waitingForSlot theContract tag 10) emTrace

        , let theContract :: Contract () Schema ContractError () = void $ awaitSlot 10
              emTrace = do
                void $ Trace.assert "Always fails" $ const False
                void $ Trace.waitNSlots 10
          in checkEmulatorFails "assert throws error" (defaultCheckOptions & minLogLevel .~ Debug) (waitingForSlot theContract tag 10) emTrace

        , let c :: Contract () Schema ContractError () = do
                let payment = Constraints.mustSatisfyAnyOf [mempty]
                void $ submitTx payment
          in run "mustSatisfyAnyOf [mempty] works"
            ( assertDone c tag (const True) "should be done"
            ) (void $ activateContract w1 c tag)
        ]

checkpointContract :: Contract () Schema ContractError ()
checkpointContract = void $ do
    checkpoint $ awaitPromise $ endpoint @"1" @Int pure .> endpoint @"2" @Int pure
    checkpoint $ awaitPromise $ endpoint @"1" @Int pure .> endpoint @"3" @Int pure

loopCheckpointContract :: Contract () Schema ContractError Int
loopCheckpointContract = do
    -- repeatedly expose the "1" endpoint until we get a total
    -- value greater than 3.
    -- We can call "1" with different values to control whether
    -- the left or right branch is chosen.
    flip checkpointLoop (0 :: Int) $ \counter -> awaitPromise $ endpoint @"1" @Int $ \vl -> do
        let newVal = counter + vl
        if newVal > 3
            then pure (Left newVal)
            else pure (Right newVal)

errorContract :: Contract () Schema ContractError Int
errorContract = do
    catchError
        (awaitPromise $ endpoint @"1" @Int
                      $ \_ -> throwError (OtherContractError "something went wrong"))
        (\_ -> checkpoint $ awaitPromise $ endpoint @"2" @Int pure .> endpoint @"3" @Int pure)

someAddress :: Address
someAddress = Ledger.scriptAddress someValidator

someValidator :: Validator
someValidator = Ledger.mkValidatorScript $$(PlutusTx.compile [|| \(_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) -> () ||])

type Schema =
    Endpoint "1" Int
        .\/ Endpoint "2" Int
        .\/ Endpoint "3" Int
        .\/ Endpoint "4" Int
        .\/ Endpoint "ep" ()
        .\/ Endpoint "5" [ActiveEndpoint]
        .\/ Endpoint "6" Ledger.Tx

initial :: _
initial = State.initialiseContract loopCheckpointContract

upd :: _
upd = State.insertAndUpdateContract loopCheckpointContract
