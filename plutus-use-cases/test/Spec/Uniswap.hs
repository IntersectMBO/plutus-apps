{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-redundant-constraints #-}
module Spec.Uniswap where

import Control.Arrow
import Control.Exception hiding (assert)
import Control.Lens hiding (elements)
import Control.Monad
import Plutus.Contract
import Plutus.Contract as Contract hiding (throwError)
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.Certification
import Plutus.Contract.Test.ContractModel
import Plutus.Contracts.Currency qualified as Currency
import Plutus.Contracts.Uniswap hiding (pools, setupTokens, tokenNames, wallets)
import Plutus.Contracts.Uniswap.Trace qualified as Uniswap
import Plutus.Trace.Emulator (EmulatorRuntimeError (GenericError))
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Coverage

import Ledger qualified as Ledger
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value qualified as Value

import Data.Coerce
import Data.Foldable
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid (Last (..))
import Data.Semigroup qualified as Semigroup
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Text qualified as Text
import Data.Void

import Prettyprinter
import Test.QuickCheck hiding ((.&&.))
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Ledger.Tx.Constraints

import Spec.Uniswap.Endpoints

data PoolIndex = PoolIndex SymToken SymToken deriving (Show, Generic)

poolIndex :: SymToken -> SymToken -> PoolIndex
poolIndex t1 t2 = PoolIndex (min t1 t2) (max t1 t2)

instance Eq PoolIndex where
  PoolIndex t1 t2 == PoolIndex t1' t2' = (min t1 t2, max t1 t2) == (min t1' t2', max t1' t2')

instance Ord PoolIndex where
  compare (PoolIndex t1 t2) (PoolIndex t1' t2') = compare (min t1 t2, max t1 t2) (min t1' t2', max t1' t2')

data PoolModel = PoolModel { _coinAAmount    :: Amount A
                           , _coinBAmount    :: Amount B
                           , _liquidities    :: Map Wallet (Amount Liquidity)
                           , _liquidityToken :: SymToken
                           } deriving (Ord, Eq, Show, Generic)

data UniswapModel = UniswapModel { _uniswapToken       :: Maybe SymToken
                                 , _exchangeableTokens :: Set SymToken
                                 , _pools              :: Map PoolIndex PoolModel
                                 , _startedUserCode    :: Set Wallet
                                 } deriving (Show, Generic)

makeLenses ''UniswapModel
makeLenses ''PoolModel

open :: Getter PoolModel Bool
open = to $ \ p -> p ^. coinAAmount > 0 -- If one is bigger than zero the other one is too

prop_Uniswap :: Actions UniswapModel -> Property
prop_Uniswap = propRunActionsWithOptions
  (Uniswap.increaseTransactionLimitsOpts defaultCheckOptionsContractModel)
  defaultCoverageOptions
  (\ _ -> pure True)

deriving instance Eq (ContractInstanceKey UniswapModel w s e params)
deriving instance Show (ContractInstanceKey UniswapModel w s e params)

walletOf :: Action UniswapModel -> Wallet
walletOf a = case a of
  SetupTokens                    -> w1
  Start                          -> w1
  CreatePool w _ _ _ _           -> w
  AddLiquidity w _ _ _ _         -> w
  RemoveLiquidity w _ _ _        -> w
  PerformSwap w _ _ _            -> w
  ClosePool w _ _                -> w
  BadRemoveLiquidity w _ _ _ _ _ -> w
  Bad act                        -> walletOf act

hasPool :: ModelState UniswapModel -> SymToken -> SymToken -> Bool
hasPool s t1 t2 = isJust (s ^. contractState . pools . at (poolIndex t1 t2))

hasOpenPool :: ModelState UniswapModel -> SymToken -> SymToken -> Bool
hasOpenPool s t1 t2 = hasPool s t1 t2
                      && s ^. contractState . pools . at (poolIndex t1 t2) . to fromJust . open

liquidityOf :: ModelState UniswapModel -> Wallet -> SymToken -> SymToken -> Amount Liquidity
liquidityOf s w t1 t2 = sum $ s ^? contractState . pools . at (poolIndex t1 t2) . _Just . liquidities . at w . to sum

totalLiquidity :: ModelState UniswapModel -> SymToken -> SymToken -> Amount Liquidity
totalLiquidity s t1 t2 = sum $ s ^? contractState . pools . at (poolIndex t1 t2) . _Just . liquidities . to sum

hasUniswapToken :: ModelState UniswapModel -> Bool
hasUniswapToken s = isJust $ s ^. contractState . uniswapToken

hasExchangeableToken :: SymToken -> ModelState UniswapModel -> Bool
hasExchangeableToken t = (^. contractState . exchangeableTokens . to (t `elem`))

swapUnless :: Bool -> (a, a) -> (a, a)
swapUnless b (a, a') = if b then (a, a') else (a', a)

mkAmounts :: SymToken -> SymToken -> Integer -> Integer -> (Amount A, Amount B)
mkAmounts t1 t2 a1 a2 = Amount *** Amount $ swapUnless (t1 < t2) (a1, a2)

getAToken, getBToken :: SymToken -> SymToken -> SymToken
getAToken = min
getBToken = max

-- | Create some sample tokens and distribute them to
--   the emulated wallets
setupTokens :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
setupTokens = do
    ownAddr <- Contract.ownAddress
    cur   <- Currency.mintContract ownAddr [(fromString tn, fromIntegral (length wallets) * amount) | tn <- tokenNames]
    let cs = Currency.currencySymbol cur
        v  = mconcat [Value.singleton cs (fromString tn) amount | tn <- tokenNames]

    forM_ wallets $ \w -> do
        let addr = mockWalletAddress w
        when (addr /= ownAddr) $ do
            cs <- mkTxConstraints @Void mempty
                    (mustPayToAddress (Ledger.toPlutusAddress addr) v)
            Contract.adjustUnbalancedTx cs >>= submitTxConfirmed

    tell $ Just $ Semigroup.Last cur
  where
    amount = 1000000

wallets :: [Wallet]
wallets = take 6 knownWallets

tokenNames :: [String]
tokenNames = ["A", "B", "C", "D"]

instance ContractModel UniswapModel where
  -- TODO: add negative tests!
  data Action UniswapModel = SetupTokens
                           -- ^ Give some tokens to wallets `w1..w4`
                           | Start
                           -- ^ Start the system by creating a pool-factory
                           | CreatePool Wallet SymToken Integer SymToken Integer
                           -- ^ Create a liquidity pool
                           | AddLiquidity Wallet SymToken Integer SymToken Integer
                           -- ^ Amount of liquidity added for each token by the wallet
                           | PerformSwap Wallet SymToken SymToken Integer
                           -- ^ Swap the first token for the second token
                           | RemoveLiquidity Wallet SymToken SymToken Integer
                           -- ^ Amount of liquidity to cash in
                           | ClosePool Wallet SymToken SymToken
                           -- ^ Close a liquidity pool
                           | Bad (Action UniswapModel)
                           -- ^ An action included for negative testing
                           | BadRemoveLiquidity Wallet SymToken Integer SymToken Integer Integer
                           -- ^ Remove liquidity, specify amounts to get
                           deriving (Eq, Show, Generic)

  data ContractInstanceKey UniswapModel w s e params where
    OwnerKey :: ContractInstanceKey UniswapModel (Last (Either Text.Text Uniswap)) EmptySchema ContractError ()
    SetupKey :: ContractInstanceKey UniswapModel (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
    WalletKey :: Wallet -> ContractInstanceKey UniswapModel (Last (Either Text.Text UserContractState)) UniswapUserSchema Void SymToken
    BadReqKey :: Wallet -> ContractInstanceKey UniswapModel () BadEndpoints Void SymToken

  initialInstances = []

  instanceWallet OwnerKey      = w1
  instanceWallet SetupKey      = w1
  instanceWallet (WalletKey w) = w
  instanceWallet (BadReqKey w) = w

  instanceContract tokenSem key token = case key of
    OwnerKey    -> ownerEndpoint
    SetupKey    -> setupTokens
    WalletKey _ -> toContract . userEndpoints . Uniswap . Coin . fromAssetId . tokenSem  $ token
    BadReqKey _ -> toContract . badEndpoints  . Uniswap . Coin . fromAssetId . tokenSem  $ token

  initialState = UniswapModel Nothing mempty mempty mempty

  arbitraryAction s =
    frequency $ [ (1, pure Start)
                , (1, pure SetupTokens) ] ++
                [ (3, createPool) | not . null $ s ^. contractState . exchangeableTokens ] ++
                [ (10, gen) | gen <- [add True, swap True, remove True, close]
                           , not . null $ s ^. contractState . exchangeableTokens
                           , not . null $ s ^. contractState . pools ] ++
                [ (1, bad) | bad <- [add False, swap False, remove False]
                           , not . null $ s ^. contractState . exchangeableTokens ] ++
                [ (1, generalRemove) | not . null $ s ^. contractState . exchangeableTokens
                                     , not . null $ s ^. contractState . pools ]
    where
      createPool = do
        w <- elements $ wallets \\ [w1]
        t1 <- elements $ s ^. contractState . exchangeableTokens . to Set.toList
        t2 <- elements $ s ^. contractState . exchangeableTokens . to (Set.delete t1) . to Set.toList
        a1 <- choose (1, 100)
        a2 <- choose (1, 100)
        (tA, tB) <- elements [(t1, t2), (t2, t1)]
        return $ CreatePool w tA a1 tB a2

      add good = do
        w <- elements $ wallets \\ [w1]
        (t1, t2) <- twoTokens good
        a1 <- choose (0, 100)
        a2 <- choose (0, 100)
        (tA, tB) <- elements [(t1, t2), (t2, t1)]
        return . bad $ AddLiquidity w tA a1 tB a2

      swap good = do
        (t1, t2) <- twoTokens good
        w <- elements $ wallets \\ [w1]
        a <- choose (1, 100)
        (tA, tB) <- elements [(t1, t2), (t2, t1)]
        return . bad $ PerformSwap w tA tB a

      remove good = do
        (t1, t2) <- twoTokens good
        let idx = poolIndex t1 t2
        w <- if good && hasOpenPool s t1 t2 then
            -- if the poolIndex exists, but the pool has been closed, then we cannot choose a wallet with liquidity
            elements . fold $ s ^? contractState . pools . at idx . _Just . liquidities . to (Map.filter (0<)) . to Map.keys
          else
          elements $ wallets \\ [w1]
        a <- if good then
          choose (1, sum $ s ^? contractState . pools . at idx . _Just . liquidities . at w . _Just . to unAmount)
          else
          oneof [choose (1,10), choose (1,100)]
        (tA, tB) <- elements [(t1, t2), (t2, t1)]
        return . bad $ RemoveLiquidity w tA tB a

      -- TODO: make an evil version of this endpoint
      close = do
        w <- elements $ wallets \\ [w1]
        PoolIndex t1 t2 <- elements $ s ^. contractState . pools . to Map.keys
        (tA, tB) <- elements [(t1, t2), (t2, t1)]
        return $ ClosePool w tA tB

      generalRemove = do
        r <- remove True
        case r of
          RemoveLiquidity w tA tB a -> do
            (Positive aA, Positive aB) <- arbitrary
            return . bad $ BadRemoveLiquidity w tA aA tB aB a
          _ -> return r

      twoTokens True  = do PoolIndex t1 t2 <- elements $ s ^. contractState . pools . to Map.keys
                           return (t1, t2)
      twoTokens False = two (/=) . elements $ s ^. contractState . exchangeableTokens . to Set.toList

      two p gen = ((,) <$> gen <*> gen) `suchThat` uncurry p

      bad act = if precondition s act then act else Bad act

  startInstances s act = case act of
    Start       -> [ StartContract OwnerKey () ]
    SetupTokens -> [ StartContract SetupKey () ]
    _           -> [ start (walletOf act) t
                   | Just t <- s ^. contractState . uniswapToken . to (:[])
                   , walletOf act `notElem` s ^. contractState . startedUserCode
                   , start <- [StartContract . WalletKey, StartContract . BadReqKey] ]

  precondition s Start                        = not $ hasUniswapToken s
  precondition s SetupTokens                  = null (s ^. contractState . exchangeableTokens)
  precondition s (CreatePool _ t1 a1 t2 a2)   = hasUniswapToken s
                                                && not (hasOpenPool s t1 t2)
                                                && t1 /= t2
                                                && 0 < a1
                                                && 0 < a2
  precondition s (AddLiquidity _ t1 _ t2 _  ) = hasOpenPool s t1 t2
                                                && t1 /= t2
  precondition s (PerformSwap _ t1 t2 a)      = hasOpenPool s t1 t2
                                                && t1 /= t2
                                                && 0 < a
  precondition s (RemoveLiquidity w t1 t2 a)  = hasOpenPool s t1 t2
                                                && t1 /= t2
                                                && Amount a <= liquidityOf s w t1 t2
                                                && Amount a < totalLiquidity s t1 t2
                                                && 0 < a
  precondition s (ClosePool w t1 t2)          = hasOpenPool s t1 t2
                                                && t1 /= t2
                                                && liquidityOf s w t1 t2 == totalLiquidity s t1 t2
  precondition s (Bad badAction)              = (wf badAction &&) . not $ precondition s badAction
    where wf (AddLiquidity _ t1 _ t2 _ )        = wfTokens t1 t2
          wf (PerformSwap _ t1 t2 _)            = wfTokens t1 t2
          wf (RemoveLiquidity _ t1 t2 _)        = wfTokens t1 t2
          wf (BadRemoveLiquidity _ t1 _ t2 _ _) = wfTokens t1 t2
          wf _                                  = error "Pattern match(es) are not exhaustive\nIn an equation for `wf'."

          wfTokens t1 t2 = hasUniswapToken s && t1 /= t2
  precondition s (BadRemoveLiquidity w t1 a1 t2 a2 a) =
    precondition s (RemoveLiquidity w t1 t2 a) &&
    let p = s ^. contractState . pools . at (poolIndex t1 t2) . to fromJust in
    calculateRemoval (p ^. coinAAmount) (p ^. coinBAmount) (sum $ p ^. liquidities) (Amount a)
    ==
    (Amount a1, Amount a2)

  nextState act = case act of
    SetupTokens -> do
      -- Give 1000000 A, B, C, and D token to w1, w2, w3, w4
      -- The tokens will be given to each wallet in a UTxO that needs
      -- to have minAdaTxOutEstimated
      withdraw w1 $ Ada.toValue ((fromInteger . toInteger . length $ wallets) * Ledger.minAdaTxOutEstimated)
      -- Create the tokens
      ts <- forM tokenNames $ \t -> do
        tok <- createToken t
        mint (symAssetIdValue tok (fromIntegral $ length wallets * 1000000))
        return tok
      -- Give the tokens to the wallets
      forM_ wallets $ \ w -> do
        deposit w $ Ada.toValue Ledger.minAdaTxOutEstimated
        deposit w $ mconcat [ symAssetIdValue t 1000000 | t <- ts ]
      exchangeableTokens %= (Set.fromList ts <>)
      wait 21

    Start -> do
      -- Create the uniswap token
      us <- createToken "Uniswap"
      uniswapToken .= Just us
      -- Pay to the UTxO for the uniswap factory
      withdraw w1 (Ada.toValue Ledger.minAdaTxOutEstimated)
      wait 6

    CreatePool w t1 a1 t2 a2 -> do
      startedUserCode %= Set.insert w
      -- Create a new liquidity token unless we
      -- have already created the token in a previous
      -- version of this pool
      mp <- use $ pools . at (poolIndex t1 t2)
      ltok <- case mp of
        Nothing -> createToken "Liquidity"
        Just p  -> pure $ p ^. liquidityToken
      -- Compute the amount of liquidity and token
      let (tokAAmount, tokBAmount) = mkAmounts t1 t2 a1 a2
          liq = calculateInitialLiquidity tokAAmount tokBAmount
      when (liq > 0) $ do
        -- Create the pool
        pools %= Map.insert (poolIndex t1 t2) (PoolModel tokAAmount tokBAmount (Map.singleton w liq) ltok)
        -- Give `w` the liquidity tokens
        let liqVal = symAssetIdValue ltok (fromIntegral $ unAmount liq)
        deposit w liqVal
        mint liqVal
        -- Pay to the pool
        withdraw w $ Ada.toValue Ledger.minAdaTxOutEstimated
        withdraw w $ symAssetIdValue t1 (fromIntegral a1)
                  <> symAssetIdValue t2 (fromIntegral a2)
      wait 5

    AddLiquidity w t1 a1 t2 a2 -> do
      startedUserCode %= Set.insert w
      p <- use $ pools . at (poolIndex t1 t2) . to fromJust
      -- Compute the amount of liquidity token we get
      let (deltaA, deltaB) = mkAmounts t1 t2 a1 a2
          deltaL = calculateAdditionalLiquidity (p ^. coinAAmount)
                                                (p ^. coinBAmount)
                                                (p ^. liquidities . to sum)
                                                deltaA
                                                deltaB
      when (deltaL > 0) $ do
        -- Update the pool with the new token and the new liquidity
        let p' = p
               & liquidities . at w %~ Just . (+deltaL) . sum
               & coinAAmount +~ deltaA
               & coinBAmount +~ deltaB
        pools . at (poolIndex t1 t2) .= Just p'
        -- Give the tokens to the pool
        withdraw w $ symAssetIdValue t1 (fromIntegral a1) <> symAssetIdValue t2 (fromIntegral a2)
        -- Create liquidity token and give it to `w`
        let liqVal = symAssetIdValue (p ^. liquidityToken) (fromIntegral $ unAmount deltaL)
        mint liqVal
        deposit w liqVal
        -- Make sure product increases
        assertSpec "AddLiquidity increases total product" $
          unAmount (p ^. coinAAmount) * unAmount (p ^. coinBAmount) <
            unAmount (p' ^. coinAAmount) * unAmount (p' ^. coinBAmount)
      wait 5

    PerformSwap w t1 t2 a -> do
      startedUserCode %= Set.insert w
      p <- use $ pools . at (poolIndex t1 t2) . to fromJust
      let oldA = p ^. coinAAmount
          oldB = p ^. coinBAmount
          -- Depending on whether t1 < t2
          -- we are either swapping A-token for B-token
          -- or the other way around
          (a', p', lens) = if t1 < t2
                           then let a' = findSwapA oldA oldB (Amount a)
                                    p' = p & coinAAmount +~ Amount a
                                           & coinBAmount -~ Amount a'
                                in (a', p', coinBAmount . to unAmount)
                           else let a' = findSwapB oldA oldB (Amount a)
                                    p' = p & coinBAmount +~ Amount a
                                           & coinAAmount -~ Amount a'
                                in (a', p', coinAAmount . to unAmount)
      when (0 < a' && a' < p ^. lens) $ do
        -- Swap the coins
        withdraw w $ symAssetIdValue t1 $ fromIntegral a
        deposit w $ symAssetIdValue t2 $ fromIntegral a'
        -- Update the pool
        pools . at (poolIndex t1 t2) .= Just p'
        -- Make sure product increases
        assertSpec "PerformSwap increases total product" $
            unAmount (p ^. coinAAmount) * unAmount (p ^. coinBAmount) <
              unAmount (p' ^. coinAAmount) * unAmount (p' ^. coinBAmount)
      wait 5

    RemoveLiquidity w t1 t2 a -> do
      startedUserCode %= Set.insert w
      -- Update the pool
      p <- use $ pools . at (poolIndex t1 t2) . to fromJust
      let inA = p ^. coinAAmount
          inB = p ^. coinBAmount
          liquidity = sum $ p ^. liquidities
          (outA, outB) = calculateRemoval inA inB liquidity (Amount a)
          p' = p
             & coinAAmount .~ outA
             & coinBAmount .~ outB
             & liquidities . at w . _Just -~ Amount a
      pools . at (poolIndex t1 t2) .= Just p'
      -- Take the requisite amount of coin out of the contract
      deposit w $ symAssetIdValue (getAToken t1 t2) (fromIntegral $ unAmount $ inA - outA)
                <> symAssetIdValue (getBToken t1 t2) (fromIntegral $ unAmount $ inB - outB)
      -- Burn the liquidity tokens
      let liqVal = symAssetIdValue (p ^. liquidityToken) $ fromIntegral a
      withdraw w liqVal
      mint $ invSymValue liqVal
      wait 5

    ClosePool w t1 t2 -> do
      startedUserCode %= Set.insert w
      p <- use $ pools . at (poolIndex t1 t2) . to fromJust
      -- Reset the pool
      let p' = p
             & coinAAmount .~ 0
             & coinBAmount .~ 0
             & liquidities .~ Map.empty
      pools . at (poolIndex t1 t2) .= Just p'
      -- Take the rest of the money out of the contract
      deposit w $ symAssetIdValue (getAToken t1 t2) (p ^. coinAAmount . to unAmount . to fromIntegral)
                <> symAssetIdValue (getBToken t1 t2) (p ^. coinBAmount . to unAmount . to fromIntegral)
      let liqVal = symAssetIdValue (p ^. liquidityToken) (p ^. liquidities . at w . to fromJust . to unAmount . to fromIntegral)
      -- Burn the remaining liquidity tokens
      withdraw w liqVal
      mint $ invSymValue liqVal
      -- Return the 2 ada at the script to the wallet
      deposit w $ Ada.toValue Ledger.minAdaTxOutEstimated
      wait 5

    Bad _ -> do
      wait 5

    BadRemoveLiquidity w t1 _ t2 _ a ->
      nextState $ RemoveLiquidity w t1 t2 a      -- because the precondition ensures the amounts are valid

  perform h tokenSem s act = case act of
    SetupTokens -> do
      delay 40
      Trace.observableState (h SetupKey) >>= \case
        Just (Semigroup.Last cur) -> sequence_ [ registerToken tn (toAssetId $ Value.assetClass (Currency.currencySymbol cur) $ fromString tn) | tn <- ["A", "B", "C", "D"]]
        _                         -> Trace.throwError $ GenericError "failed to create currency"

    Start -> do
      delay 5
      Trace.observableState (h OwnerKey) >>= \case
        Last (Just (Right (Uniswap (Coin v)))) -> registerToken "Uniswap" (toAssetId v)
        _                                      -> Trace.throwError $ GenericError "initialisation failed"

    CreatePool w t1 a1 t2 a2 -> do
      let us = s ^. contractState . uniswapToken . to fromJust
          c1 = Coin (fromAssetId $ tokenSem t1)
          c2 = Coin (fromAssetId $ tokenSem t2)
          Coin ac = liquidityCoin (fst . Value.unAssetClass . fromAssetId . tokenSem $ us) c1 c2
      Trace.callEndpoint @"create" (h (WalletKey w)) $ CreateParams c1 c2 (Amount a1) (Amount a2)
      delay 5
      unless (hasPool s t1 t2) $ do
        registerToken "Liquidity" (toAssetId ac)

    AddLiquidity w t1 a1 t2 a2 -> do
      let c1 = Coin (fromAssetId $ tokenSem t1)
          c2 = Coin (fromAssetId $ tokenSem t2)
      Trace.callEndpoint @"add" (h (WalletKey w)) $ AddParams c1 c2 (Amount a1) (Amount a2)
      delay 5

    PerformSwap w t1 t2 a -> do
      let c1 = Coin (fromAssetId $ tokenSem t1)
          c2 = Coin (fromAssetId $ tokenSem t2)
      Trace.callEndpoint @"swap" (h (WalletKey w)) $ SwapParams c1 c2 (Amount a) 0
      delay 5

    RemoveLiquidity w t1 t2 a -> do
      let c1 = Coin (fromAssetId $ tokenSem t1)
          c2 = Coin (fromAssetId $ tokenSem t2)
      Trace.callEndpoint @"remove" (h (WalletKey w)) $ RemoveParams c1 c2 (Amount a)
      delay 5

    BadRemoveLiquidity w t1 a1 t2 a2 a -> do
      let c1 = Coin (fromAssetId $ tokenSem t1)
          c2 = Coin (fromAssetId $ tokenSem t2)
      Trace.callEndpoint @"bad-remove" (h (BadReqKey w)) $ BadRemoveParams c1 (Amount a1) c2 (Amount a2) (Amount a)
      delay 5

    ClosePool w t1 t2 -> do
      let c1 = Coin (fromAssetId $ tokenSem t1)
          c2 = Coin (fromAssetId $ tokenSem t2)
      Trace.callEndpoint @"close" (h (WalletKey w)) $ CloseParams c1 c2
      delay 5

    Bad act -> do
      perform h tokenSem s act

  shrinkAction s a = case a of
    CreatePool w t1 a1 t2 a2           -> [ CreatePool w t1 a1' t2 a2'   | (a1', a2') <- shrink (a1, a2), a1' >= 0, a2' >= 0 ]
    AddLiquidity w t1 a1 t2 a2         -> [ AddLiquidity w t1 a1' t2 a2' | (a1', a2') <- shrink (a1, a2), a1' >= 0, a2' >= 0 ]
    RemoveLiquidity w t1 t2 a          -> [ RemoveLiquidity w t1 t2 a'   | a'         <- shrink a, a' >= 0 ]
    PerformSwap w t1 t2 a              -> [ PerformSwap w t1 t2 a'       | a'         <- shrink a, a' >= 0 ]
    Bad act                            -> Bad <$> shrinkAction s act
    BadRemoveLiquidity w t1 a1 t2 a2 a -> [ BadRemoveLiquidity w t1 a1' t2 a2' a' | (a1', a2', a') <- shrink (a1, a2, a)]
    _                                  -> []

  monitoring _ (Bad act) = tabulate "Bad actions" [actionName act]
  monitoring _ _         = id

-- This doesn't hold
prop_liquidityValue :: Property
prop_liquidityValue = forAllDL liquidityValue (const True)
  where
    liquidityValue :: DL UniswapModel ()
    liquidityValue = do
      anyActions_
      pools <- viewContractState pools
      forM_ (Map.filter (view open) pools) $ \ p -> do
        let cond  = calculateRemoval (p ^. coinAAmount) (p ^. coinBAmount) (p ^. liquidities . to sum) (Amount 1)
                          /= (p ^. coinAAmount, p ^. coinBAmount)
        assert ("Pool\n  " ++ show p ++ "\nforgets single liquidities") cond

noLockProof :: NoLockedFundsProof UniswapModel
noLockProof = defaultNLFP {
      nlfpMainStrategy   = mainStrat,
      nlfpWalletStrategy = walletStrat,
      nlfpOverhead       = const $ toSymValue Ledger.minAdaTxOutEstimated,
      nlfpErrorMargin    = wiggle }
    where
        wiggle (coerce -> s) = fold [ symAssetIdValue t1 (fromIntegral m) <>
                                      symAssetIdValue t2 (fromIntegral m) <>
                                      toSymValue Ledger.minAdaTxOutEstimated
                                    | (PoolIndex t1 t2, p) <- Map.toList (s ^. contractState . pools)
                                    , let numLiqs = length $ p ^. liquidities
                                          m = max 0 (numLiqs - 1) ]
        mainStrat = do
            pools <- viewContractState pools
            forM_ (Map.toList pools) $ \ (PoolIndex t1 t2, p) -> do
              let liqs = Map.toList . Map.filter (>0) $ p ^. liquidities
              forM_ (take (length liqs - 1) liqs) $ \ (w, l) ->
                action $ RemoveLiquidity w t1 t2 (unAmount l)
              when (not . null $ liqs) $ do
                action $ ClosePool (fst . last $ liqs) t1 t2

        walletStrat w = do
            pools <- viewContractState pools
            forM_ (Map.toList pools) $ \ (PoolIndex t1 t2, p) -> do
              let liqs = p ^. liquidities . to (Map.filter (>0))
              when (w `Map.member` liqs) $ do
                if Map.keys liqs == [w]
                then action $ ClosePool w t1 t2
                else action $ RemoveLiquidity w t1 t2 (unAmount . sum $ Map.lookup w liqs)

noLockProofLight :: NoLockedFundsProofLight UniswapModel
noLockProofLight = NoLockedFundsProofLight{nlfplMainStrategy = nlfpMainStrategy noLockProof}

prop_CheckNoLockedFundsProof :: Property
prop_CheckNoLockedFundsProof = checkNoLockedFundsProof noLockProof

prop_CheckNoLockedFundsProofFast :: Property
prop_CheckNoLockedFundsProofFast = checkNoLockedFundsProofFast noLockProof

check_propUniswapWithCoverage :: IO ()
check_propUniswapWithCoverage = void $ do
  cr <- quickCheckWithCoverage (stdArgs { maxSuccess = 1000 })
                                 (set endpointCoverageReq epReqs $ set coverageIndex covIdx $ defaultCoverageOptions)
                                 $ \covopts -> propRunActionsWithOptions @UniswapModel
                                              defaultCheckOptionsContractModel
                                              covopts
                                              (const (pure True))
  writeCoverageReport "Uniswap" cr
  where
    epReqs t ep
      | True = 0
      | t == Trace.walletInstanceTag w1 = 0
      | ep == "create"                  = 20
      | ep == "swap"                    = 15
      | ep == "close"                   = 0.5
      | ep == "remove"                  = 15
      | ep == "add"                     = 20
      | otherwise                       = 0

prop_Whitelist :: Actions UniswapModel -> Property
prop_Whitelist = checkErrorWhitelist defaultWhitelist

tests :: TestTree
tests = testGroup "uniswap" [
    checkPredicateOptions (Uniswap.increaseTransactionLimitsOpts defaultCheckOptions) "can create a liquidity pool and add liquidity"
        (assertNotDone Uniswap.setupTokens
                       (Trace.walletInstanceTag w1)
                       "setupTokens contract should be still running"
        .&&. assertNoFailedTransactions)
        Uniswap.uniswapTrace
    , testProperty "prop_Uniswap" $ withMaxSuccess 20 prop_Uniswap
    , testProperty "prop_UniswapAssertions" $ withMaxSuccess 1000 (propSanityCheckAssertions @UniswapModel)
    , testProperty "prop_NLFP" $ withMaxSuccess 250 prop_CheckNoLockedFundsProofFast
    ]

runTestsWithCoverage :: IO ()
runTestsWithCoverage = do
  ref <- newCoverageRef
  defaultMain (coverageTests ref)
    `catch` \(e :: SomeException) -> do
                covdata <- readCoverageRef ref
                putStrLn . show $ pretty (CoverageReport covIdx covdata)
                throwIO e
  where
    coverageTests ref = testGroup "game state machine tests"
                         [ checkPredicateCoverage "can create a liquidity pool and add liquidity"
                            ref
                            (assertNotDone Uniswap.setupTokens
                                           (Trace.walletInstanceTag w1)
                                           "setupTokens contract should be still running"
                            .&&. assertNoFailedTransactions)
                            Uniswap.uniswapTrace
                          ]

-- | Certification.
certification :: Certification UniswapModel
certification = defaultCertification {
    certNoLockedFundsLight = Just noLockProofLight,
    certCoverageIndex      = covIdx
  }
