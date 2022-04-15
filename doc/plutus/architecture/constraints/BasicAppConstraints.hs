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

import Ledger (Ada, ChainIndexTxOut, PaymentPubKeyHash, ScriptContext, TxOutRef)
import Ledger.Ada qualified as Ada
import Ledger.Constraints (MkTxError, TxConstraints, UnbalancedTx)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts

import Data.Either (Either)
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Prelude (Bool, mappend, ($), (-))
import Prelude (Show, flip, (<>))

-- BLOCK1

data SplitData =
    SplitData
        { recipient1 :: PaymentPubKeyHash -- ^ First recipient of the funds
        , recipient2 :: PaymentPubKeyHash -- ^ Second recipient of the funds
        , amount     :: Ada -- ^ How much Ada we want to lock
        }
    deriving stock (Show, Generic)

-- For a 'real' application use 'makeIsDataIndexed' to ensure the output is stable over time
PlutusTx.unstableMakeIsData ''SplitData
PlutusTx.makeLift ''SplitData

-- BLOCK2

-- Create constraints that will be used to spend a locked transaction output
-- from the script address.
--
-- These constraints will be used in the validation script as well as in the
-- transaction creation step.
{-# INLINABLE splitDataConstraints #-}
splitDataConstraints :: SplitData -> TxConstraints () SplitData
splitDataConstraints SplitData{recipient1, recipient2, amount} =
            Constraints.mustPayToPubKey recipient1 (Ada.toValue half)
  `mappend` Constraints.mustPayToPubKey recipient2 (Ada.toValue $ amount - half)
 where
     half = Ada.divide amount 2

-- BLOCK3

-- | The validation logic is generated with `checkScriptContext` based on the set
-- of constraints.
{-# INLINABLE validateSplit #-}
validateSplit :: SplitData -> () -> ScriptContext -> Bool
validateSplit splitData _ =
    Constraints.checkScriptContext (splitDataConstraints splitData)

data Split
instance Scripts.ValidatorTypes Split where
    type instance RedeemerType Split = ()
    type instance DatumType Split = SplitData

{-# INLINABLE splitValidator #-}
splitValidator :: Scripts.TypedValidator Split
splitValidator = Scripts.mkTypedValidator @Split
    $$(PlutusTx.compile [|| validateSplit ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @SplitData @()

-- BLOCK4

-- | Creates a transaction which locks the amount to be splitted between two
-- payment keys to a script address.
lockFundsTx :: SplitData -> Either MkTxError UnbalancedTx
lockFundsTx s@SplitData { amount } =
    let lookups = Constraints.typedValidatorLookups splitValidator
        constraints = Constraints.mustPayToTheScript s (Ada.toValue amount)
     in Constraints.mkTx lookups constraints

-- BLOCK5

-- | Creates a transaction which spends all script outputs from a script address,
-- sums the value of the scripts outputs and splits it between two payment keys.
--
-- You can get the script address with:
--
-- > let contractAddress = Scripts.validatorAddress splitValidator
--
-- To get the utxos of that script address, you need to query the node.
unlockFundsTx
    :: Map TxOutRef ChainIndexTxOut
    -- ^ Utxos from our script address queried from the node
    -> SplitData
    -> Either MkTxError UnbalancedTx
unlockFundsTx scriptUtxos splitData =
    let lookups = Constraints.typedValidatorLookups splitValidator
               <> Constraints.unspentOutputs scriptUtxos
        scriptUtxoRefs = Map.keys scriptUtxos
        -- We always use the empty redeemer when spending the script output
        constraints = foldMap (flip Constraints.mustSpendOutputFromTheScript ()) scriptUtxoRefs
                   <> splitDataConstraints splitData
     in Constraints.mkTx lookups constraints

-- BLOCK6
