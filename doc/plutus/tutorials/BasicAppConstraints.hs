{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
module BasicAppConstraints where

-- BLOCK0

import BasicApps (Split, SplitData (SplitData, amount, recipient1, recipient2), SplitSchema, mkSplitData)
import Cardano.Node.Emulator.Params (pNetworkId)
import Ledger (Ada, PaymentPubKeyHash, ScriptContext, TxOutRef)
import Ledger.Constraints (MkTxError, TxConstraints, UnbalancedTx)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OnChain.V1 qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, Promise, endpoint, getParams, submitTxConstraintsSpending, utxosAt)
import Plutus.Script.Utils.Ada qualified as Ada

import Control.Monad (void)
import Data.Either (Either)
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Prelude (Bool, mappend, ($), (-), (.))
import Prelude (Show, flip, (<$>), (<>))

-- BLOCK1

-- | Create constraints that will be used to spend a locked transaction output
-- from the script address.
--
-- These constraints will be used in the validation script as well as in the
-- transaction creation step.
{-# INLINABLE splitDataConstraints #-}
splitDataConstraints :: SplitData -> TxConstraints () SplitData
splitDataConstraints SplitData{recipient1, recipient2, amount} =
            Constraints.mustPayToAddress recipient1 (Ada.toValue half)
  `mappend` Constraints.mustPayToAddress recipient2 (Ada.toValue $ amount - half)
 where
     half = Ada.divide amount 2

-- BLOCK2

-- | The validation logic is generated with `checkScriptContext` based on the set
-- of constraints.
{-# INLINABLE validateSplit #-}
validateSplit :: SplitData -> () -> ScriptContext -> Bool
validateSplit splitData _ =
    Constraints.checkScriptContext (splitDataConstraints splitData)

-- BLOCK3

splitValidator :: Scripts.TypedValidator Split
splitValidator = Scripts.mkTypedValidator @Split
    $$(PlutusTx.compile [|| validateSplit ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.mkUntypedValidator @ScriptContext @SplitData @()

-- BLOCK4

unlock :: Promise () SplitSchema T.Text ()
unlock = endpoint @"unlock" (unlockFunds . mkSplitData)

-- | Creates a transaction which spends all script outputs from a script address,
-- sums the value of the scripts outputs and splits it between two payment keys.
unlockFunds :: SplitData -> Contract () SplitSchema T.Text ()
unlockFunds splitData = do
    networkId <- pNetworkId <$> getParams
    -- Get the address of the Split validator
    let contractAddress = Scripts.validatorCardanoAddress networkId splitValidator
    -- Get all utxos that are locked by the Split validator
    utxos <- utxosAt contractAddress
    -- Generate constraints which will spend all utxos locked by the Split
    -- validator and split the value evenly between the two payment keys.
    let constraints = Constraints.collectFromTheScript utxos ()
                      <> splitDataConstraints splitData
    -- Create, Balance and submit the transaction
    void $ submitTxConstraintsSpending splitValidator utxos constraints

-- BLOCK5
