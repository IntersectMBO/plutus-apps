{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:coverage-all #-}

module Plutus.Contracts.Uniswap.OffChain
    ( poolStateCoinFromUniswapCurrency, liquidityCoin
    , CreateParams (..)
    , SwapParams (..)
    , CloseParams (..)
    , RemoveParams (..)
    , AddParams (..)
    , UniswapUserSchema, UserContractState (..)
    , UniswapOwnerSchema
    , start, create, add, remove, close, swap, pools
    , ownerEndpoint, userEndpoints
    , findSwapA, findSwapB, covIdx
    -- exported for defining test endpoints
    , findUniswapFactoryAndPool, uniswapInstance, liquidityPolicy
    , uniswapScript, poolStateCoin, liquidityCurrency, lpTicker
    , calculateRemoval, funds
    ) where

import Cardano.Node.Emulator.Internal.Node.Params (testnet)
import Control.Lens ((^?))
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Ledger (CardanoAddress, DecoratedTxOut, datumInDatumFromQuery, decoratedTxOutPlutusValue,
               decoratedTxOutScriptDatum)
import Ledger.Tx.Constraints as Constraints hiding (adjustUnbalancedTx)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Contract
import Plutus.Contract.Test.Coverage.Analysis
import Plutus.Contracts.Currency qualified as Currency
import Plutus.Contracts.Uniswap.OnChain (mkUniswapValidator, validateLiquidityMinting)
import Plutus.Contracts.Uniswap.Pool
import Plutus.Contracts.Uniswap.Types
import Plutus.Script.Utils.V2.Address (mkValidatorCardanoAddress)
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.V2.Ledger.Api (CurrencySymbol, Datum (Datum), DatumHash, MintingPolicy, Redeemer (Redeemer), TokenName,
                             TxOutRef, Validator, Value)
import Plutus.V2.Ledger.Api qualified as V2
import PlutusTx qualified
import PlutusTx.Coverage
import PlutusTx.Prelude hiding (Semigroup (..), dropWhile, flip, unless)
import Prelude as Haskell (Int, Semigroup (..), Show, String, div, dropWhile, flip, show, (^))
import Text.Printf (printf)

data Uniswapping
instance Scripts.ValidatorTypes Uniswapping where
    type instance RedeemerType Uniswapping = UniswapAction
    type instance DatumType    Uniswapping = UniswapDatum

type UniswapOwnerSchema = Endpoint "start" ()

-- | Schema for the endpoints for users of Uniswap.
type UniswapUserSchema =
        Endpoint "create" CreateParams
        .\/ Endpoint "swap"   SwapParams
        .\/ Endpoint "close"  CloseParams
        .\/ Endpoint "remove" RemoveParams
        .\/ Endpoint "add"    AddParams
        .\/ Endpoint "pools"  ()
        .\/ Endpoint "funds"  ()
        .\/ Endpoint "stop"   ()

-- | Type of the Uniswap user contract state.
data UserContractState =
      Pools [((Coin A, Amount A), (Coin B, Amount B))]
    | Funds Value
    | Created
    | Swapped
    | Added
    | Removed
    | Closed
    | Stopped
    deriving (Show, Generic, FromJSON, ToJSON)


uniswapTokenName, poolStateTokenName :: TokenName
uniswapTokenName = "Uniswap"
poolStateTokenName = "Pool State"

uniswapInstance :: Uniswap -> V2.TypedValidator Uniswapping
uniswapInstance us = V2.mkTypedValidator @Uniswapping
    ($$(PlutusTx.compile [|| mkUniswapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode us
        `PlutusTx.applyCode` PlutusTx.liftCode c)
     $$(PlutusTx.compile [|| wrap ||])
  where
    c :: Coin PoolState
    c = poolStateCoin us

    wrap = Scripts.mkUntypedValidator @Scripts.ScriptContextV2 @UniswapDatum @UniswapAction

uniswapScript :: Uniswap -> Validator
uniswapScript = Scripts.validatorScript . uniswapInstance

uniswapAddress :: Uniswap -> CardanoAddress
uniswapAddress = mkValidatorCardanoAddress testnet . uniswapScript

uniswap :: CurrencySymbol -> Uniswap
uniswap cs = Uniswap $ mkCoin cs uniswapTokenName

liquidityPolicy :: Uniswap -> MintingPolicy
liquidityPolicy us = V2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \u t -> Scripts.mkUntypedMintingPolicy (validateLiquidityMinting u t) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode us
        `PlutusTx.applyCode` PlutusTx.liftCode poolStateTokenName

covIdx :: CoverageIndex
covIdx = $refinedCoverageIndex $$(PlutusTx.compile [|| \u s r d c -> check $ mkUniswapValidator u s r d c ||])

liquidityCurrency :: Uniswap -> CurrencySymbol
liquidityCurrency = scriptCurrencySymbol . liquidityPolicy

poolStateCoin :: Uniswap -> Coin PoolState
poolStateCoin = flip mkCoin poolStateTokenName . liquidityCurrency

-- | Gets the 'Coin' used to identity liquidity pools.
poolStateCoinFromUniswapCurrency :: CurrencySymbol -- ^ The currency identifying the Uniswap instance.
                                 -> Coin PoolState
poolStateCoinFromUniswapCurrency = poolStateCoin . uniswap

-- | Gets the liquidity token for a given liquidity pool.
liquidityCoin :: CurrencySymbol -- ^ The currency identifying the Uniswap instance.
              -> Coin A         -- ^ One coin in the liquidity pair.
              -> Coin B         -- ^ The other coin in the liquidity pair.
              -> Coin Liquidity
liquidityCoin cs coinA coinB = mkCoin (liquidityCurrency $ uniswap cs) $ lpTicker $ LiquidityPool coinA coinB

-- | Parameters for the @create@-endpoint, which creates a new liquidity pool.
data CreateParams = CreateParams
    { cpCoinA   :: Coin A   -- ^ One 'Coin' of the liquidity pair.
    , cpCoinB   :: Coin B   -- ^ The other 'Coin'.
    , cpAmountA :: Amount A -- ^ Amount of liquidity for the first 'Coin'.
    , cpAmountB :: Amount B -- ^ Amount of liquidity for the second 'Coin'.
    } deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @swap@-endpoint, which allows swaps between the two different coins in a liquidity pool.
-- One of the provided amounts must be positive, the other must be zero.
data SwapParams = SwapParams
    { spCoinA   :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    , spCoinB   :: Coin B         -- ^ The other 'Coin'.
    , spAmountA :: Amount A       -- ^ The amount the first 'Coin' that should be swapped.
    , spAmountB :: Amount B       -- ^ The amount of the second 'Coin' that should be swapped.
    } deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @close@-endpoint, which closes a liquidity pool.
data CloseParams = CloseParams
    { clpCoinA :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    , clpCoinB :: Coin B         -- ^ The other 'Coin' of the liquidity pair.
    } deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @remove@-endpoint, which removes some liquidity from a liquidity pool.
data RemoveParams = RemoveParams
    { rpCoinA :: Coin A           -- ^ One 'Coin' of the liquidity pair.
    , rpCoinB :: Coin B           -- ^ The other 'Coin' of the liquidity pair.
    , rpDiff  :: Amount Liquidity -- ^ The amount of liquidity tokens to burn in exchange for liquidity from the pool.
    } deriving (Show, Generic, ToJSON, FromJSON)

-- | Parameters for the @add@-endpoint, which adds liquidity to a liquidity pool in exchange for liquidity tokens.
data AddParams = AddParams
    { apCoinA   :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    , apCoinB   :: Coin B         -- ^ The other 'Coin' of the liquidity pair.
    , apAmountA :: Amount A       -- ^ The amount of coins of the first kind to add to the pool.
    , apAmountB :: Amount B       -- ^ The amount of coins of the second kind to add to the pool.
    } deriving (Show, Generic, ToJSON, FromJSON)

-- | Creates a Uniswap "factory". This factory will keep track of the existing liquidity pools and enforce that there will be at most one liquidity pool
-- for any pair of tokens at any given time.
start :: forall w s. Contract w s Text Uniswap
start = do
    addr <- Contract.ownAddress
    cs  <- fmap Currency.currencySymbol $
           mapError (pack . show @Currency.CurrencyError) $
           Currency.mintContract addr [(uniswapTokenName, 1)]
    let c    = mkCoin cs uniswapTokenName
        us   = uniswap cs
        inst = uniswapInstance us
        tx   = mustPayToTheScriptWithDatumInTx (Factory []) $ unitValue c

    mkTxConstraints (Constraints.typedValidatorLookups inst) tx
      >>= adjustUnbalancedTx >>= submitTxConfirmed
    void $ waitNSlots 1

    logInfo @String $ printf "started Uniswap %s at address %s" (show us) (show $ uniswapAddress us)
    return us

-- | Creates a liquidity pool for a pair of coins. The creator provides liquidity for both coins and gets liquidity tokens in return.
create :: forall w s. Uniswap -> CreateParams -> Contract w s Text ()
create us CreateParams{..} = do
    when (unCoin cpCoinA == unCoin cpCoinB) $ throwError "coins must be different"
    when (cpAmountA <= 0 || cpAmountB <= 0) $ throwError "amounts must be positive"
    (oref, o, lps) <- findUniswapFactory us
    let liquidity = calculateInitialLiquidity cpAmountA cpAmountB
        lp        = LiquidityPool {lpCoinA = cpCoinA, lpCoinB = cpCoinB}
    let usInst   = uniswapInstance us
        usScript = uniswapScript us
        usDat1   = Factory $ lp : lps
        usDat2   = Pool lp liquidity
        psC      = poolStateCoin us
        lC       = mkCoin (liquidityCurrency us) $ lpTicker lp
        usVal    = unitValue $ usCoin us
        lpVal    = valueOf cpCoinA cpAmountA <> valueOf cpCoinB cpAmountB <> unitValue psC

        lookups  = Constraints.typedValidatorLookups usInst        <>
                   Constraints.plutusV2OtherScript usScript                <>
                   Constraints.plutusV2MintingPolicy (liquidityPolicy us) <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScriptWithDatumInTx usDat1 usVal                                     <>
                   Constraints.mustPayToTheScriptWithDatumInTx usDat2 lpVal                                     <>
                   Constraints.mustMintValue (unitValue psC <> valueOf lC liquidity)              <>
                   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Create lp)

    mkTxConstraints lookups tx >>= adjustUnbalancedTx >>= submitTxConfirmed

    logInfo $ "created liquidity pool: " ++ show lp

-- | Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
close :: forall w s. Uniswap -> CloseParams -> Contract w s Text ()
close us CloseParams{..} = do
    ((oref1, o1, lps), (oref2, o2, lp, liquidity)) <- findUniswapFactoryAndPool us clpCoinA clpCoinB
    let usInst   = uniswapInstance us
        usScript = uniswapScript us
        usDat    = Factory $ filter (/= lp) lps
        usC      = usCoin us
        psC      = poolStateCoin us
        lC       = mkCoin (liquidityCurrency us) $ lpTicker lp
        usVal    = unitValue usC
        psVal    = unitValue psC
        lVal     = valueOf lC liquidity
        redeemer = Redeemer $ PlutusTx.toBuiltinData Close

        lookups  = Constraints.typedValidatorLookups usInst        <>
                   Constraints.plutusV2OtherScript usScript                <>
                   Constraints.plutusV2MintingPolicy (liquidityPolicy us) <>
                   Constraints.unspentOutputs (Map.singleton oref1 o1 <> Map.singleton oref2 o2)

        tx       = Constraints.mustPayToTheScriptWithDatumInTx usDat usVal <>
                   Constraints.mustMintValue (negate $ psVal <> lVal) <>
                   Constraints.mustSpendScriptOutput oref1 redeemer <>
                   Constraints.mustSpendScriptOutput oref2 redeemer <>
                   Constraints.mustIncludeDatumInTx (Datum $ PlutusTx.toBuiltinData $ Pool lp liquidity)

    mkTxConstraints lookups tx >>= adjustUnbalancedTx >>= submitTxConfirmed

    logInfo $ "closed liquidity pool: " ++ show lp

-- | Removes some liquidity from a liquidity pool in exchange for liquidity tokens.
remove :: forall w s. Uniswap -> RemoveParams -> Contract w s Text ()
remove us RemoveParams{..} = do
    (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us rpCoinA rpCoinB
    when (rpDiff < 1 || rpDiff >= liquidity) $ throwError "removed liquidity must be positive and less than total liquidity"
    let usInst       = uniswapInstance us
        usScript     = uniswapScript us
        dat          = Pool lp $ liquidity - rpDiff
        psC          = poolStateCoin us
        lC           = mkCoin (liquidityCurrency us) $ lpTicker lp
        psVal        = unitValue psC
        lVal         = valueOf lC rpDiff
        inVal        = decoratedTxOutPlutusValue o
        inA          = amountOf inVal rpCoinA
        inB          = amountOf inVal rpCoinB
        (outA, outB) = calculateRemoval inA inB liquidity rpDiff
        val          = psVal <> valueOf rpCoinA outA <> valueOf rpCoinB outB
        redeemer     = Redeemer $ PlutusTx.toBuiltinData Remove

        lookups  = Constraints.typedValidatorLookups usInst          <>
                   Constraints.plutusV2OtherScript usScript                  <>
                   Constraints.plutusV2MintingPolicy (liquidityPolicy us)   <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScriptWithDatumInTx dat val          <>
                   Constraints.mustMintValue (negate lVal)        <>
                   Constraints.mustSpendScriptOutput oref redeemer

    mkTxConstraints lookups tx >>= adjustUnbalancedTx >>= submitTxConfirmed

    logInfo $ "removed liquidity from pool: " ++ show lp

-- | Adds some liquidity to an existing liquidity pool in exchange for newly minted liquidity tokens.
add :: forall w s. Uniswap -> AddParams -> Contract w s Text ()
add us AddParams{..} = do
    (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us apCoinA apCoinB
    when (apAmountA < 0 || apAmountB < 0) $ throwError "amounts must not be negative"
    let outVal = decoratedTxOutPlutusValue o
        oldA   = amountOf outVal apCoinA
        oldB   = amountOf outVal apCoinB
        newA   = oldA + apAmountA
        newB   = oldB + apAmountB
        delL   = calculateAdditionalLiquidity oldA oldB liquidity apAmountA apAmountB
        inVal  = valueOf apCoinA apAmountA <> valueOf apCoinB apAmountB
    when (delL <= 0) $ throwError "insufficient liquidity"
    logInfo @String $ printf "oldA = %d, oldB = %d, newA = %d, newB = %d, delL = %d" oldA oldB newA newB delL

    let usInst       = uniswapInstance us
        usScript     = uniswapScript us
        dat          = Pool lp $ liquidity + delL
        psC          = poolStateCoin us
        lC           = mkCoin (liquidityCurrency us) $ lpTicker lp
        psVal        = unitValue psC
        lVal         = valueOf lC delL
        val          = psVal <> valueOf apCoinA newA <> valueOf apCoinB newB
        redeemer     = Redeemer $ PlutusTx.toBuiltinData Add

        lookups  = Constraints.typedValidatorLookups usInst             <>
                   Constraints.plutusV2OtherScript usScript                     <>
                   Constraints.plutusV2MintingPolicy (liquidityPolicy us)       <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScriptWithDatumInTx dat val          <>
                   Constraints.mustMintValue lVal                  <>
                   Constraints.mustSpendScriptOutput oref redeemer

    logInfo @String $ printf "val = %s, inVal = %s" (show val) (show inVal)
    logInfo $ show lookups
    logInfo $ show tx

    mkTxConstraints lookups tx >>= adjustUnbalancedTx >>= submitTxConfirmed

    logInfo $ "added liquidity to pool: " ++ show lp

-- | Uses a liquidity pool two swap one sort of coins in the pool against the other.
swap :: forall w s. Uniswap -> SwapParams -> Contract w s Text ()
swap us SwapParams{..} = do
    unless (spAmountA > 0 && spAmountB == 0 || spAmountA == 0 && spAmountB > 0) $ throwError "exactly one amount must be positive"
    (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us spCoinA spCoinB
    let outVal = decoratedTxOutPlutusValue o
    let oldA = amountOf outVal spCoinA
        oldB = amountOf outVal spCoinB
    (newA, newB) <- if spAmountA > 0 then do
        let outB = Amount $ findSwapA oldA oldB spAmountA
        when (outB == 0) $ throwError "no payout"
        return (oldA + spAmountA, oldB - outB)
                                     else do
        let outA = Amount $ findSwapB oldA oldB spAmountB
        when (outA == 0) $ throwError "no payout"
        return (oldA - outA, oldB + spAmountB)

    logInfo @String $ printf "oldA = %d, oldB = %d, old product = %d, newA = %d, newB = %d, new product = %d" oldA oldB (unAmount oldA * unAmount oldB) newA newB (unAmount newA * unAmount newB)

    let inst    = uniswapInstance us
        val     = valueOf spCoinA newA <> valueOf spCoinB newB <> unitValue (poolStateCoin us)

        lookups = Constraints.typedValidatorLookups inst                 <>
                  Constraints.plutusV2OtherScript (Scripts.validatorScript inst) <>
                  Constraints.unspentOutputs (Map.singleton oref o)

        tx      = mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Swap) <>
                  Constraints.mustPayToTheScriptWithDatumInTx (Pool lp liquidity) val

    mkTxConstraints lookups tx >>= adjustUnbalancedTx >>= submitTxConfirmed

    logInfo $ "swapped with: " ++ show lp

-- | Finds all liquidity pools and their liquidity belonging to the Uniswap instance.
-- This merely inspects the blockchain and does not issue any transactions.
pools :: forall w s. Uniswap -> Contract w s Text [((Coin A, Amount A), (Coin B, Amount B))]
pools us = do
    utxos <- utxosAt (uniswapAddress us)
    go $ snd <$> Map.toList utxos
  where
    go :: [DecoratedTxOut] -> Contract w s Text [((Coin A, Amount A), (Coin B, Amount B))]
    go []       = return []
    go (o : os) = do
        let v = decoratedTxOutPlutusValue o
        if isUnity v c
            then do
                d <- getUniswapDatum o
                case d of
                    Factory _ -> go os
                    Pool lp _ -> do
                        let coinA = lpCoinA lp
                            coinB = lpCoinB lp
                            amtA  = amountOf v coinA
                            amtB  = amountOf v coinB
                            s     = ((coinA, amtA), (coinB, amtB))
                        logInfo $ "found pool: " ++ show s
                        ss <- go os
                        return $ s : ss
            else go os
      where
        c :: Coin PoolState
        c = poolStateCoin us

-- | Gets the caller's funds.
funds :: forall w s. Contract w s Text Value
funds = do
    addr <- Contract.ownAddress
    os   <- map snd . Map.toList <$> utxosAt addr
    return $ foldMap decoratedTxOutPlutusValue os

getUniswapDatum :: DecoratedTxOut -> Contract w s Text UniswapDatum
getUniswapDatum o = do
  (dh, d) <- maybe
               (throwError "no datum for a txout of a public key address")
               pure
               (o ^? decoratedTxOutScriptDatum)
  Datum e <- maybe (getDatum dh) pure (d ^? datumInDatumFromQuery)
  maybe (throwError "datum hash wrong type")
        pure
        (PlutusTx.fromBuiltinData e)
  where
    getDatum :: DatumHash -> Contract w s Text Datum
    getDatum dh =
      datumFromHash dh >>= \case Nothing -> throwError "datum not found"
                                 Just d  -> pure d

findUniswapInstance ::
    forall a b w s.
    Uniswap
    -> Coin b
    -> (UniswapDatum -> Maybe a)
    -> Contract w s Text (TxOutRef, DecoratedTxOut, a)
findUniswapInstance us c f = do
    let addr = uniswapAddress us
    logInfo @String $ printf "looking for Uniswap instance at address %s containing coin %s " (show addr) (show c)
    utxos <- utxosAt addr
    go  [x | x@(_, o) <- Map.toList utxos, isUnity (decoratedTxOutPlutusValue o) c]
  where
    go [] = throwError "Uniswap instance not found"
    go ((oref, o) : xs) = do
        d <- getUniswapDatum o
        case f d of
            Nothing -> go xs
            Just a  -> do
                logInfo @String $ printf "found Uniswap instance with datum: %s" (show d)
                return (oref, o, a)

findUniswapFactory :: forall w s. Uniswap -> Contract w s Text (TxOutRef, DecoratedTxOut, [LiquidityPool])
findUniswapFactory us@Uniswap{..} = findUniswapInstance us usCoin $ \case
    Factory lps -> Just lps
    Pool _ _    -> Nothing

findUniswapPool :: forall w s. Uniswap -> LiquidityPool -> Contract w s Text (TxOutRef, DecoratedTxOut, Amount Liquidity)
findUniswapPool us lp = findUniswapInstance us (poolStateCoin us) $ \case
        Pool lp' l
            | lp == lp' -> Just l
        _               -> Nothing

findUniswapFactoryAndPool :: forall w s.
                          Uniswap
                          -> Coin A
                          -> Coin B
                          -> Contract w s Text ( (TxOutRef, DecoratedTxOut, [LiquidityPool])
                                               , (TxOutRef, DecoratedTxOut, LiquidityPool, Amount Liquidity)
                                               )
findUniswapFactoryAndPool us coinA coinB = do
    (oref1, o1, lps) <- findUniswapFactory us
    case [ lp'
         | lp' <- lps
         , lp' == LiquidityPool coinA coinB
         ] of
        [lp] -> do
            (oref2, o2, a) <- findUniswapPool us lp
            return ( (oref1, o1, lps)
                   , (oref2, o2, lp, a)
                   )
        _    -> throwError "liquidity pool not found"

findSwapA :: Amount A -> Amount B -> Amount A -> Integer
findSwapA oldA oldB inA
    | ub' <= 1   = 0
    | otherwise  = go 1 ub'
  where
    cs :: Integer -> Bool
    cs outB = checkSwap oldA oldB (oldA + inA) (oldB - Amount outB)

    ub' :: Integer
    ub' = head $ dropWhile cs [2 ^ i | i <- [0 :: Int ..]]

    go :: Integer -> Integer -> Integer
    go lb ub
        | ub == (lb + 1) = lb
        | otherwise      =
      let
        m = div (ub + lb) 2
      in
        if cs m then go m ub else go lb m

findSwapB :: Amount A -> Amount B -> Amount B -> Integer
findSwapB oldA oldB inB = findSwapA (switch oldB) (switch oldA) (switch inB)
  where
    switch = Amount . unAmount

ownerEndpoint :: Contract (Last (Either Text Uniswap)) EmptySchema ContractError ()
ownerEndpoint = do
    e <- mapError absurd $ runError start
    void $ waitNSlots 1
    tell $ Last $ Just e

-- | Provides the following endpoints for users of a Uniswap instance:
--
--      [@create@]: Creates a liquidity pool for a pair of coins. The creator provides liquidity for both coins and gets liquidity tokens in return.
--      [@swap@]: Uses a liquidity pool two swap one sort of coins in the pool against the other.
--      [@close@]: Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
--      [@remove@]: Removes some liquidity from a liquidity pool in exchange for liquidity tokens.
--      [@add@]: Adds some liquidity to an existing liquidity pool in exchange for newly minted liquidity tokens.
--      [@pools@]: Finds all liquidity pools and their liquidity belonging to the Uniswap instance. This merely inspects the blockchain and does not issue any transactions.
--      [@funds@]: Gets the caller's funds. This merely inspects the blockchain and does not issue any transactions.
--      [@stop@]: Stops the contract.
userEndpoints :: Uniswap -> Promise (Last (Either Text UserContractState)) UniswapUserSchema Void ()
userEndpoints us =
    stop
        `select`
    (void (f (Proxy @"create") (const Created) create                 `select`
           f (Proxy @"swap")   (const Swapped) swap                   `select`
           f (Proxy @"close")  (const Closed)  close                  `select`
           f (Proxy @"remove") (const Removed) remove                 `select`
           f (Proxy @"add")    (const Added)   add                    `select`
           f (Proxy @"pools")  Pools           (\us' () -> pools us') `select`
           f (Proxy @"funds")  Funds           (\_us () -> funds))
     <> userEndpoints us)
  where
    f :: forall l a p.
         (HasEndpoint l p UniswapUserSchema, FromJSON p)
      => Proxy l
      -> (a -> UserContractState)
      -> (Uniswap -> p -> Contract (Last (Either Text UserContractState)) UniswapUserSchema Text a)
      -> Promise (Last (Either Text UserContractState)) UniswapUserSchema Void ()
    f _ g c = handleEndpoint @l $ \p -> do
        e <- either (pure . Left) (runError . c us) p
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right a  -> Right $ g a

    stop :: Promise (Last (Either Text UserContractState)) UniswapUserSchema Void ()
    stop = handleEndpoint @"stop" $ \e -> do
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right () -> Right Stopped
