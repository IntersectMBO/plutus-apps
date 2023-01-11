{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -fno-warn-name-shadowing #-}

module Plutus.Contract.Test.ContractModel.Internal.ContractInstance where

import Plutus.Trace.Emulator.Types (ContractHandle (..), ContractInstanceTag)

import Data.Aeson qualified as JSON
import Data.Data
import Data.Row (Row)

import Cardano.Api (AssetId)
import Plutus.Contract (Contract)
import Plutus.Contract.Test hiding (not)
import Plutus.Trace.Emulator as Trace (walletInstanceTag)

import Test.QuickCheck.ContractModel

-- | Key-value map where keys and values have three indices that can vary between different elements
--   of the map. Used to store `ContractHandle`s, which are indexed over observable state, schema,
--   and error type.
data IMap (key :: i -> j -> k -> l -> *) (val :: i -> j -> k -> *) where
    IMNil  :: IMap key val
    IMCons :: (Typeable i, Typeable j, Typeable k, Typeable l)
           => key i j k l
           -> val i j k
           -> IMap key val
           -> IMap key val

-- TODO: Should this make sure we don't duplicate keys?
imAppend :: IMap key val -> IMap key val -> IMap key val
imAppend IMNil m           = m
imAppend (IMCons k v m) m' = IMCons k v (imAppend m m')

-- | Look up a value in an indexed map. First checks that the indices agree, using `cast`. Once the
--   type checker is convinced that the indices match we can check the key for equality.
imLookup :: (Typeable i, Typeable j, Typeable k, Typeable l, Typeable key, Typeable val, Eq (key i j k l))
         => key i j k l
         -> IMap key val
         -> Maybe (val i j k)
imLookup _ IMNil = Nothing
imLookup k (IMCons key val m) =
    case cast (key, val) of
        Just (key', val') | key' == k -> Just val'
        _                             -> imLookup k m

imMap :: (forall i j k l. key i j k l -> key' i j k l)
      -> (forall i j k. val i j k -> val' i j k)
      -> IMap key val -> IMap key' val'
imMap _ _ IMNil          = IMNil
imMap f g (IMCons k v m) = IMCons (f k) (g v) (imMap f g m)

-- $walletHandles
--
-- In order to call contract endpoints using `Plutus.Trace.Emulator.callEndpoint`, a `ContractHandle`
-- is required. Contract handles are managed behind the scenes by the `propRunActions` functions,
-- based on a given a list of associations of `ContractInstanceKey`s with `Wallet`s and
-- `Contract`s. Before testing starts, `activateContractWallet` is called for all entries in the
-- list and the mapping from `ContractInstanceKey` to `ContractHandle` is provided in the `HandleFun` argument
-- to `perform`.

-- | The constraints required on contract schemas and error types to enable calling contract
--   endpoints (`Plutus.Trace.Emulator.callEndpoint`).
type SchemaConstraints w schema err =
        ( Typeable w
        , Monoid w
        , JSON.ToJSON w
        , Typeable schema
        , ContractConstraints schema
        , Show err
        , Typeable err
        , JSON.ToJSON err
        , JSON.FromJSON err
        , JSON.ToJSON w
        , JSON.FromJSON w
        )

data WalletContractHandle w s e = WalletContractHandle Wallet (ContractHandle w s e)

type Handles state = IMap (ContractInstanceKey state) WalletContractHandle

handlesAppend :: Handles state -> Handles state -> Handles state
handlesAppend = imAppend

data StartContract state where
  StartContract :: (SchemaConstraints w s e, Typeable p) => ContractInstanceKey state w s e p -> p -> StartContract state

class ( ContractModel state
      , (forall w s e p. Eq (ContractInstanceKey state w s e p))
      , (forall w s e p. Show (ContractInstanceKey state w s e p))
      ) => ContractInstanceModel state where

    -- | To be able to call a contract endpoint from a wallet a `ContractHandle` is required. These
    --   are managed by the test framework and all the user needs to do is provide this contract
    --   instance key type representing the different contract instances that a test needs to work
    --   with, and when creating a property (see `propRunActions_`) provide a list of contract
    --   instance keys together with their wallets and contracts.
    --   Contract instance keys are indexed by the observable state, schema, and error type of the
    --   contract and should be defined as a GADT. For example, a handle type for a contract with
    --   one seller and multiple buyers could look like this.
    --
    --   >  data ContractInstanceKey MyModel w s e where
    --   >      Buyer  :: Wallet -> ContractInstanceKey MyModel MyObsState MySchema MyError MyParams
    --   >      Seller :: ContractInstanceKey MyModel MyObsState MySchema MyError MyParams
    data ContractInstanceKey state :: * -> Row * -> * -> * -> *

    -- | Get the wallet that the contract running at a specific `ContractInstanceKey` should run
    -- in
    instanceWallet :: ContractInstanceKey state w s e p -> Wallet

    -- | The 'ContractInstanceTag' of an instance key for a wallet. Defaults to 'walletInstanceTag'.
    --   You must override this if you have multiple instances per wallet.
    instanceTag :: forall w s e p. SchemaConstraints w s e => ContractInstanceKey state w s e p -> ContractInstanceTag
    instanceTag = walletInstanceTag . instanceWallet

    -- | The initial handles
    initialInstances :: [StartContract state]
    initialInstances = []

    -- | Start new contract instances
    startInstances :: ModelState state
                   -> Action state
                   -> [StartContract state]
    startInstances _ _ = []

    -- | Map a `ContractInstanceKey` `k` to the `Contract` that is started when we start
    -- `k` in a given `ModelState` with a given semantics of `SymToken`s
    instanceContract :: (SymToken -> AssetId)
                     -> ContractInstanceKey state w s e p
                     -> p
                     -> Contract w s e ()
