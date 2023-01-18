{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Spec.Uniswap.Endpoints where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger.Tx.Constraints as Constraints
import Plutus.Contract as Contract
import Plutus.Contracts.Currency ()
import Plutus.Contracts.Uniswap.Pool
import Plutus.Contracts.Uniswap.Types
import Plutus.V2.Ledger.Api (Redeemer (Redeemer))
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), dropWhile, flip, unless)
import Prelude as Haskell (Semigroup (..), Show, show)

import Plutus.Contracts.Uniswap.OffChain

type TestUniswapUserSchema =
       Endpoint "bad-remove" BadRemoveParams
       .\/ UniswapUserSchema

type BadEndpoints = Endpoint "bad-remove" BadRemoveParams

-- | Parameters for the @bad-remove@-endpoint, which removes some liquidity from a liquidity pool.
data BadRemoveParams = BadRemoveParams
    { brpCoinA :: Coin A          -- ^ One 'Coin' of the liquidity pair.
    , brpOutA  :: Amount A        -- ^ Amount to try to remove
    , brpCoinB :: Coin B          -- ^ The other 'Coin' of the liquidity pair.
    , brpOutB  :: Amount B        -- ^ Amount to try to remove
    , brpDiff  :: Amount Liquidity-- ^ The amount of liquidity tokens to burn in exchange for liquidity from the pool.
    } deriving (Show, Generic, ToJSON, FromJSON)


-- | A variant on remove which tries to remove different amounts of tokens
badRemove :: forall w s. Uniswap -> BadRemoveParams -> Contract w s Text ()
badRemove us BadRemoveParams{..} = do
    (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us brpCoinA brpCoinB
    --when (brpDiff < 1 || brpDiff >= liquidity) $ throwError "removed liquidity must be positive and less than total liquidity"
    let usInst       = uniswapInstance us
        usScript     = uniswapScript us
        dat          = Pool lp $ liquidity - brpDiff
        psC          = poolStateCoin us
        lC           = mkCoin (liquidityCurrency us) $ lpTicker lp
        psVal        = unitValue psC
        lVal         = valueOf lC brpDiff
        --inVal        = view ciTxOutValue o
        --inA          = amountOf inVal brpCoinA
        --inB          = amountOf inVal brpCoinB
        --(outA, outB) = calculateRemoval inA inB liquidity brpDiff
        --val          = psVal <> valueOf brpCoinA (inA-brpOutA) <> valueOf brpCoinB (inB-brpOutB)
        -- This version allows us to control the submitted transaction more directly (and also reveals a more interesting failed test case).
        val          = psVal <> valueOf brpCoinA brpOutA <> valueOf brpCoinB brpOutB
        redeemer     = Redeemer $ PlutusTx.toBuiltinData Remove

        lookups  = Constraints.typedValidatorLookups usInst          <>
                   Constraints.plutusV2OtherScript usScript                  <>
                   Constraints.plutusV2MintingPolicy (liquidityPolicy us)   <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScriptWithDatumInTx dat val          <>
                   Constraints.mustMintValue (negate lVal)         <>
                   Constraints.mustSpendScriptOutput oref redeemer

    mkTxConstraints lookups tx >>= Contract.adjustUnbalancedTx >>= submitTxConfirmed

    logInfo $ "removed liquidity from pool: " ++ show lp

badEndpoints :: Uniswap -> Promise () BadEndpoints Void ()
badEndpoints us =
  void (handleEndpoint @"bad-remove" $ either (pure . Left) (runError . badRemove us))
  <> badEndpoints us
