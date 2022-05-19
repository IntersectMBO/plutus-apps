{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Plutus.PAB.Db.FS.ContractStore where

import Control.Exception (IOException, throw)
import Control.Monad.Catch (catch)
import Control.Monad.Freer (Eff, LastMember, Member, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Reader (Reader, ask)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, toJSON)
import Data.Bifunctor (first)
import Data.ByteString.Lazy.Char8 qualified as L
import Data.ByteString.Lazy.Char8 qualified as LB
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.UUID (toText)
import Database.Beam
import Plutus.PAB.Db.FS.Types (ContractInstance (..), ContractStoreDir (..), SerializableState)
import Plutus.PAB.Effects.Contract (ContractStore (..), PABContract (serialisableState))
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Types (PABError (..))
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Wallet.Types (ContractActivityStatus (Active, Stopped), ContractInstanceId (..))

import Control.Category ((<<<))
import Control.Monad.Freer.Extras (LogMsg)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Data (Proxy)
import Data.Proxy (Proxy (Proxy))
import Data.Traversable (for)
import Data.UUID qualified as UUID
import Plutus.PAB.Effects.Contract.Builtin (Builtin, HasDefinitions, fromResponse, getContract)
import Plutus.PAB.Monitoring.Monitoring (PABMultiAgentMsg)
import Plutus.PAB.Webserver.Types (ContractActivationArgs (caID))
import System.Directory (getDirectoryContents)


mkContractFilePath :: forall t. ContractStoreDir t -> ContractInstanceId -> FilePath
mkContractFilePath (ContractStoreDir dir) cId = dir </> printId cId <> ".json"
  where
    printId (ContractInstanceId t) = T.unpack $ toText t

hoistEither :: forall e effs. Member (Error e) effs => Either e ~> Eff effs
hoistEither (Left e)  = throwError e
hoistEither (Right a) = pure a

handleIO ::
  ( LastMember IO effs
  , Member (Error PABError) effs
  )
  => IO a
  -> (IOException -> Maybe PABError)
  -> Eff effs a
handleIO action errHandler = do
  let
    errHandler' err = case errHandler err of
      Just err' -> pure $ Left err'
      Nothing   -> throw err
  res <- liftIO $ (Right <$> action) `catch` errHandler'
  hoistEither res

getContractInstance
  :: forall effs t.
  ( LastMember IO effs
  , Member (Error PABError) effs
  , FromJSON (Contract.ContractDef t)
  )
  => ContractStoreDir t -> ContractInstanceId -> Eff effs (ContractInstance t)
getContractInstance contractStore contractInstanceId = do
  let
    contractFilePath = mkContractFilePath contractStore contractInstanceId

    fsHandler e | isDoesNotExistError e = Just $ ContractInstanceNotFound contractInstanceId
    fsHandler e = Just $ FSContractStoreError $ "Fetching instance state failed: " <> show e

  content <- handleIO (L.readFile contractFilePath) fsHandler
  hoistEither $ first FSContractStoreError $ eitherDecode content

note :: a -> Maybe b -> Either a b
note err Nothing = Left err
note _ (Just a)  = Right a

getContractState ::
  forall effs t.
  ( LastMember IO effs
  , Member (Error PABError) effs
  , FromJSON (Contract.ContractDef t)
  ) =>
  ContractStoreDir t ->
  ContractInstanceId ->
  Eff effs SerializableState
getContractState contractStore contractInstanceId = do
  ContractInstance _ state _ <- getContractInstance contractStore contractInstanceId
  hoistEither $ note (ContractStateNotFound contractInstanceId) state

getContractInstances ::
  forall effs t.
  ( LastMember IO effs
  , Member (Error PABError) effs
  , FromJSON (Contract.ContractDef t)
  ) =>
  ContractStoreDir t ->
  Eff effs (Map ContractInstanceId (ContractInstance t))
getContractInstances contractStore@(ContractStoreDir dir) = do
  let
    fsHandler e = Just $ FSContractStoreError $ "FS error while reading contract store directory" <> show e

  fileNames <- handleIO (getDirectoryContents dir) fsHandler

  (Map.fromList <<< catMaybes) <$> for fileNames tryContractFile

  where
    tryContractFile :: String -> Eff effs (Maybe (ContractInstanceId, ContractInstance t))
    tryContractFile fileName = case T.unpack <$> T.stripSuffix ".json" (T.pack fileName) of
      Nothing -> pure Nothing
      Just cIdStr -> do
        let
          mUUID = UUID.fromText $ T.pack cIdStr
        uuid <- hoistEither $ note (FSContractStoreError $ "Unable to parse contract instance uuid: " <> cIdStr) mUUID
        let
          contractInstanceId = ContractInstanceId uuid
        (Just <<< (contractInstanceId,)) <$> getContractInstance contractStore contractInstanceId

putContractInstance ::
  ( LastMember IO effs
  , Member (Error PABError) effs
  , ToJSON (Contract.ContractDef t)
  ) =>
  ContractStoreDir t ->
  ContractInstanceId ->
  ContractInstance t ->
  Eff effs ()
putContractInstance contractStore contractInstanceId contractInstance = do
  let
    contractFilePath = mkContractFilePath contractStore contractInstanceId

    fsHandler (e :: IOException) = Just $ FSContractStoreError $ "Fetching instance state failed: " <> show e
    content = encodePretty $ toJSON contractInstance

  handleIO (LB.writeFile contractFilePath content) fsHandler

-- NOTE This is non atomic update but PAB
-- doesn't really care about atomicity
-- of a storage contract update anyway.
-- It just assumes that the storage is
-- only touched by the contract instance
-- thread.
putContractState ::
  forall effs t.
  ( LastMember IO effs
  , Member (Error PABError) effs
  , FromJSON (Contract.ContractDef t)
  , ToJSON (Contract.ContractDef t)
  , PABContract t
  ) =>
  ContractStoreDir t ->
  ContractInstanceId ->
  Contract.State t ->
  Eff effs ()
putContractState contractStore contractInstanceId state = do
  ContractInstance args _ active <- getContractInstance contractStore contractInstanceId
  let
    state' = serialisableState (Proxy :: Proxy t) state
  putContractInstance contractStore contractInstanceId $ ContractInstance args (Just state') active

-- NOTE: All the machinery above is polymorphic regarding the contract type (`Builtin` or not) but
-- this interpreter is dedicated for Contracts which are only `Builtin`s.
handleContractStore ::
  forall effs t.
  ( LastMember IO effs
  , Member (Reader (ContractStoreDir (Builtin t))) effs
  , Member (LogMsg (PABMultiAgentMsg (Builtin t))) effs
  , Member (Error PABError) effs
  , ToJSON t
  , FromJSON t
  , HasDefinitions t
  )
  => ContractStore (Builtin t) ~> Eff effs
handleContractStore action = do
  contractStore <- ask @(ContractStoreDir (Builtin t))
  handle contractStore action
  where
    handle contractStoreDir (Contract.PutStartInstance args instanceId) =
      putContractInstance contractStoreDir instanceId $ ContractInstance args Nothing Active
    handle contractStoreDir (Contract.PutState _ instanceId state) =
      putContractState contractStoreDir instanceId state
    handle contractStoreDir (PutStopInstance instanceId) = do
      ContractInstance args state _ <- getContractInstance contractStoreDir instanceId
      putContractInstance contractStoreDir instanceId $ ContractInstance args state Stopped
    handle contractStoreDir (Contract.GetState instanceId) = do
      ContractInstance { contractInstanceSerialisableState, contractActivationArgs } <- getContractInstance contractStoreDir instanceId
      case contractInstanceSerialisableState of
        Just s -> do
          let
            cId = caID contractActivationArgs

          -- getContract :: a -> SomeBuiltin -- ^ The actual contract function of contract type `a`
          let contract = getContract @t cId
          fromResponse @t instanceId contract s
        Nothing -> throwError (ContractInstanceNotFound instanceId)
    handle contractStoreDir (Contract.GetContracts status) = do
      let
        filterByStatus = case status of
          Just status' -> Map.filter $ \ContractInstance { contractInstanceActivityStatus } ->
            contractInstanceActivityStatus == status'
          Nothing      -> id

      (fmap contractActivationArgs <<< filterByStatus) <$> getContractInstances contractStoreDir
