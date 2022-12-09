{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module BasicApps where

-- BLOCK0

import Cardano.Node.Emulator.Params (pNetworkId)
import Control.Monad (forever, void)
import Control.Monad.Freer.Extras.Log (LogLevel (Debug, Info))
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (def)
import Data.Text qualified as T
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Ledger (Ada, CardanoAddress, PaymentPubKeyHash (unPaymentPubKeyHash), toPlutusAddress)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, Endpoint, Promise, endpoint, getParams, logInfo, selectList, submitTxConstraints,
                        submitTxConstraintsSpending, type (.\/), utxosAt)
import Plutus.Contract.Test (w1, w2)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Api (Address, ScriptContext (ScriptContext, scriptContextTxInfo), TxInfo (txInfoOutputs),
                             TxOut (TxOut, txOutAddress, txOutValue), Value)
import PlutusTx qualified
import PlutusTx.Prelude (Bool, Maybe (Just, Nothing), Semigroup ((<>)), mapMaybe, mconcat, ($), (&&), (-), (.), (==),
                         (>=))
import Prelude (IO, (<$>), (>>))
import Prelude qualified as Haskell
import Schema (ToSchema)
import Wallet.Emulator.Stream (filterLogLevel)
import Wallet.Emulator.Wallet (Wallet, mockWalletAddress)

-- BLOCK1

data SplitData =
    SplitData
        { recipient1 :: Address -- ^ First recipient of the funds
        , recipient2 :: Address -- ^ Second recipient of the funds
        , amount     :: Ada -- ^ How much Ada we want to lock
        }
    deriving stock (Haskell.Show, Generic)

-- For a 'real' application use 'makeIsDataIndexed' to ensure the output is stable over time
PlutusTx.unstableMakeIsData ''SplitData
PlutusTx.makeLift ''SplitData

-- BLOCK2

validateSplit :: SplitData -> () -> ScriptContext -> Bool
validateSplit SplitData{recipient1, recipient2, amount} _ ScriptContext{scriptContextTxInfo} =
    let half = Ada.divide amount 2
        outputs = txInfoOutputs scriptContextTxInfo
     in
     Ada.fromValue (valuePaidToAddr outputs recipient1) >= half &&
     Ada.fromValue (valuePaidToAddr outputs recipient2) >= (amount - half)
 where
     valuePaidToAddr :: [TxOut] -> Address -> Value
     valuePaidToAddr outs addr =
         let flt TxOut{txOutAddress, txOutValue} | txOutAddress == addr = Just txOutValue
             flt _ = Nothing
         in mconcat $ mapMaybe flt outs

-- BLOCK3

data Split
instance Scripts.ValidatorTypes Split where
    type instance RedeemerType Split = ()
    type instance DatumType Split = SplitData

splitValidator :: Scripts.TypedValidator Split
splitValidator = Scripts.mkTypedValidator @Split
    $$(PlutusTx.compile [|| validateSplit ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.mkUntypedValidator @ScriptContext @SplitData @()

-- BLOCK4

data LockArgs =
        LockArgs
            { recipient1Address :: CardanoAddress
            , recipient2Address :: CardanoAddress
            , totalAda          :: Ada
            }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

type SplitSchema =
        Endpoint "lock" LockArgs
        .\/ Endpoint "unlock" LockArgs

-- BLOCK5

lock :: Promise () SplitSchema T.Text ()
lock = endpoint @"lock" (lockFunds . mkSplitData)

unlock :: Promise () SplitSchema T.Text ()
unlock = endpoint @"unlock" (unlockFunds . mkSplitData)

-- BLOCK6

mkSplitData :: LockArgs -> SplitData
mkSplitData LockArgs{recipient1Address, recipient2Address, totalAda} =
    SplitData
        { recipient1 = toPlutusAddress recipient1Address
        , recipient2 = toPlutusAddress recipient2Address
        , amount = totalAda
        }

-- BLOCK7

lockFunds :: SplitData -> Contract () SplitSchema T.Text ()
lockFunds s@SplitData{amount} = do
    logInfo $ "Locking " <> Haskell.show amount
    let tx = Constraints.mustPayToTheScriptWithDatumInTx s (Ada.toValue amount)
    void $ submitTxConstraints splitValidator tx

-- BLOCK8

unlockFunds :: SplitData -> Contract () SplitSchema T.Text ()
unlockFunds SplitData{recipient1, recipient2, amount} = do
    networkId <- pNetworkId <$> getParams
    let contractAddress = Scripts.validatorCardanoAddress networkId splitValidator
    utxos <- utxosAt contractAddress
    let half = Ada.divide amount 2
        tx =
            Constraints.collectFromTheScript utxos ()
            <> Constraints.mustPayToAddress recipient1 (Ada.toValue half)
            <> Constraints.mustPayToAddress recipient2 (Ada.toValue $ amount - half)
    void $ submitTxConstraintsSpending splitValidator utxos tx

-- BLOCK9

splitPlutusApp :: Contract () SplitSchema T.Text ()
-- BLOCK10

splitPlutusApp = forever $ selectList [lock, unlock]

-- BLOCK11

runSplitDataEmulatorTrace :: IO ()
runSplitDataEmulatorTrace = do
    -- w1, w2, w3, ... are predefined mock wallets used for testing
    let w1Addr = mockWalletAddress w1
    let w2Addr = mockWalletAddress w2
    Trace.runEmulatorTraceIO $ do
        h <- Trace.activateContractWallet w1 splitPlutusApp
        Trace.callEndpoint @"lock" h $ LockArgs w1Addr w2Addr 10_000_000
        void Trace.nextSlot
        Trace.callEndpoint @"unlock" h $ LockArgs w1Addr w2Addr 10_000_000

-- BLOCK12
