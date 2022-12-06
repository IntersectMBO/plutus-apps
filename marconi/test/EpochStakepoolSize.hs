{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}

module EpochStakepoolSize where

import Control.Concurrent qualified as IO
import Control.Concurrent.Async qualified as IO
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Map qualified as Map
import Database.SQLite.Simple qualified as SQL
import Streaming.Prelude qualified as S
import System.Directory qualified as IO
import System.FilePath.Posix ((</>))

import Hedgehog qualified as H
import Hedgehog.Extras.Test qualified as HE
import Test.Base qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Streaming qualified as CS
import Testnet.Cardano qualified as TN

import Helpers qualified as TN
import Marconi.Index.EpochStakepoolSize qualified as EpochStakepoolSize


tests :: TestTree
tests = testGroup "EpochStakepoolSize"
  [ testPropertyNamed "prop_epoch_stakepool_size" "test" test
  ]

-- This test creates two sets of payment and stake addresses,
-- transfers funds to them from the genesis address, then creates a
-- stakepool, then stakes the transferred funds in that pool, then
-- waits for when the staked ADA appears in the epoch stakepool size
-- indexer.
--
-- Most of this was done by following relevant sections on Cardano
-- Developer Portal starting from this page:
-- https://developers.cardano.org/docs/stake-pool-course/handbook/create-stake-pool-keys,
-- and discovering how cardano-cli implements its command line
-- interface.
test :: H.Property
test = H.integration . HE.runFinallies . TN.workspace "chairman" $ \tempAbsPath -> do

  -- start testnet
  base <- HE.noteM $ liftIO . IO.canonicalizePath =<< HE.getProjectBase
  let testnetOptions = TN.defaultTestnetOptions
        { TN.epochLength = 10 -- Set very short epochs: 0.2 seconds per slot * 10 slots per epoch = 2 seconds per epoch
        }
  (con, conf, runtime) <- TN.startTestnet testnetOptions base tempAbsPath
  let networkId = TN.getNetworkId runtime
  socketPath <- TN.getSocketPathAbs conf runtime
  pparams <- TN.getAlonzoProtocolParams con

  -- Load genesis keys, these already exist (were already created when testnet started)
  genesisVKey :: C.VerificationKey C.GenesisUTxOKey <- TN.readAs (C.AsVerificationKey C.AsGenesisUTxOKey) $ tempAbsPath </> "shelley/utxo-keys/utxo1.vkey"
  genesisSKey :: C.SigningKey C.GenesisUTxOKey <- TN.readAs (C.AsSigningKey C.AsGenesisUTxOKey) $ tempAbsPath </> "shelley/utxo-keys/utxo1.skey"
  let
    paymentKeyFromGenesisKey = C.castVerificationKey genesisVKey :: C.VerificationKey C.PaymentKey
    genesisVKeyHash = C.PaymentCredentialByKey $ C.verificationKeyHash paymentKeyFromGenesisKey
    genesisAddress :: C.Address C.ShelleyAddr
    genesisAddress = C.makeShelleyAddress networkId genesisVKeyHash C.NoStakeAddress :: C.Address C.ShelleyAddr

  -- Create two payment and stake keys
  (paymentAddress, stakeSKey, stakeCredential) <- liftIO $ createPaymentAndStakeKey networkId
  (paymentAddress2, stakeSKey2, stakeCredential2) <- liftIO $ createPaymentAndStakeKey networkId

  -- Transfer 50 and 70 ADA to both respectively
  let stakedLovelace = 50_000_000
      stakedLovelace2 = 70_000_000
      totalStakedLovelace = stakedLovelace + stakedLovelace2
  TN.submitAwaitTx con =<< TN.mkTransferTx networkId con genesisAddress paymentAddress [C.WitnessGenesisUTxOKey genesisSKey] stakedLovelace
  TN.submitAwaitTx con =<< TN.mkTransferTx networkId con genesisAddress paymentAddress2 [C.WitnessGenesisUTxOKey genesisSKey] stakedLovelace2

  -- Register stake addresses
  TN.submitAwaitTx con =<< registerStakeAddress networkId con pparams genesisAddress genesisSKey stakeCredential
  TN.submitAwaitTx con =<< registerStakeAddress networkId con pparams genesisAddress genesisSKey stakeCredential2

  -- Register a stake pool
  let keyWitnesses =
        [ C.WitnessGenesisUTxOKey genesisSKey
        , C.WitnessStakeKey stakeSKey
        , C.WitnessStakeKey stakeSKey2
        ]

  -- Prepare transaction to register stakepool and stake funds
  (poolVKey :: C.PoolId, tx, txBody) <- registerPool con networkId pparams tempAbsPath keyWitnesses [stakeCredential, stakeCredential2] genesisAddress

  -- start indexer
  found <- liftIO IO.newEmptyMVar
  let dbPath = tempAbsPath </> "epoch_stakepool_sizes.db"
  dbCon <- liftIO $ SQL.open dbPath
  void $ liftIO $ do
    chan <- IO.newChan
    let indexer = CS.ledgerStates (TN.configurationFile runtime) socketPath C.QuickValidation
          & EpochStakepoolSize.toEvents
          & EpochStakepoolSize.sqlite dbCon
          & S.chain (IO.writeChan chan) -- After indexer has written the event to database, we write it to the chan
    void $ (IO.link =<<) $ IO.async $ void $ S.effects indexer

    -- Consume the channel until an event is found which (1) has the
    -- pool ID and (2) has the right amount of lovelace staked.
    (IO.link =<<) $ IO.async $ forever $ do
      EpochStakepoolSize.Event (_epochNo, stakeMap) <- IO.readChan chan
      case Map.lookup poolVKey stakeMap of
        Just lovelace -> when (lovelace == totalStakedLovelace) $ IO.putMVar found () -- Event found!
        _             -> return ()

  -- Submit transaction to create stakepool and stake the funds
  TN.submitAwaitTx con (tx, txBody)

  -- This is filled when the epoch stakepool size has been indexed
  liftIO $ IO.takeMVar found

  -- Let's find it in the database as well
  epochStakes <- liftIO $ EpochStakepoolSize.queryPoolId dbCon poolVKey
  case epochStakes of
    ((_, lovelace) : _) -> H.assert $ lovelace == totalStakedLovelace
    _                   -> fail "Can't find the stake for pool in sqlite!"

-- | This is a pure version of `runStakePoolRegistrationCert` defined in /cardano-node/cardano-cli/src/Cardano/CLI/Shelley/Run/Pool.hs::60
makeStakePoolRegistrationCert_
  :: C.VerificationKey C.StakePoolKey
  -> C.VerificationKey C.VrfKey
  -> C.Lovelace
  -> C.Lovelace
  -> Rational
  -> C.VerificationKey C.StakeKey
  -> [C.VerificationKey C.StakeKey]
  -> [C.StakePoolRelay]
  -> Maybe C.StakePoolMetadataReference
  -> C.NetworkId
  -> C.Certificate
makeStakePoolRegistrationCert_ stakePoolVerKey vrfVerKey pldg pCost pMrgn rwdStakeVerKey sPoolOwnerVkeys relays mbMetadata network = let
  -- Pool verification key
  stakePoolId' = C.verificationKeyHash stakePoolVerKey
  -- VRF verification key
  vrfKeyHash' = C.verificationKeyHash vrfVerKey
  -- Pool reward account
  stakeCred = C.StakeCredentialByKey (C.verificationKeyHash rwdStakeVerKey)
  rewardAccountAddr = C.makeStakeAddress network stakeCred
  -- Pool owner(s)
  stakePoolOwners' = map C.verificationKeyHash sPoolOwnerVkeys
  stakePoolParams =
    C.StakePoolParameters
      { C.stakePoolId = stakePoolId'
      , C.stakePoolVRF = vrfKeyHash'
      , C.stakePoolCost = pCost
      , C.stakePoolMargin = pMrgn
      , C.stakePoolRewardAccount = rewardAccountAddr
      , C.stakePoolPledge = pldg
      , C.stakePoolOwners = stakePoolOwners'
      , C.stakePoolRelays = relays
      , C.stakePoolMetadata = mbMetadata
      }
  in C.makeStakePoolRegistrationCertificate stakePoolParams

-- | Create a payment and related stake keys
createPaymentAndStakeKey :: C.NetworkId -> IO (C.Address C.ShelleyAddr, C.SigningKey C.StakeKey, C.StakeCredential)
createPaymentAndStakeKey networkId = do
  -- Payment key pair: cardano-cli address key-gen
  paymentSKey :: C.SigningKey C.PaymentKey <- C.generateSigningKey C.AsPaymentKey
  let paymentVKey = C.getVerificationKey paymentSKey :: C.VerificationKey  C.PaymentKey
      paymentVKeyHash = C.PaymentCredentialByKey $ C.verificationKeyHash paymentVKey

  -- Stake key pair, cardano-cli stake-address key-gen
  stakeSKey :: C.SigningKey C.StakeKey <- C.generateSigningKey C.AsStakeKey
  let stakeVKey = C.getVerificationKey stakeSKey :: C.VerificationKey C.StakeKey
      stakeCredential = C.StakeCredentialByKey $ C.verificationKeyHash stakeVKey :: C.StakeCredential
      stakeAddressReference = C.StakeAddressByValue stakeCredential :: C.StakeAddressReference

  -- Payment address that references the stake address, cardano-cli address build
  let paymentAddress = C.makeShelleyAddress networkId paymentVKeyHash stakeAddressReference :: C.Address C.ShelleyAddr
  return (paymentAddress, stakeSKey, stakeCredential)


registerStakeAddress
  :: C.NetworkId -> C.LocalNodeConnectInfo C.CardanoMode -> C.ProtocolParameters -> C.Address C.ShelleyAddr -> C.SigningKey C.GenesisUTxOKey -> C.StakeCredential
  -> HE.Integration (C.Tx C.AlonzoEra, C.TxBody C.AlonzoEra)
registerStakeAddress networkId con pparams payerAddress payerSKey stakeCredential = do

  -- Create a registration certificate: cardano-cli stake-address registration-certificate
  let stakeAddressRegCert = C.makeStakeAddressRegistrationCertificate stakeCredential :: C.Certificate

  -- Draft transaction & Calculate fees; Submit the certificate with a transaction
  -- cardano-cli transaction build
  -- cardano-cli transaction sign
  -- cardano-cli transaction submit
  (txIns, totalLovelace) <- TN.getAddressTxInsValue con payerAddress
  let
    dummyFee = 0
    tx0 = (TN.emptyTxBodyContent dummyFee pparams)
      { C.txIns = map (, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) txIns
      , C.txOuts = [TN.mkAddressAdaTxOut payerAddress $ totalLovelace - dummyFee]
      , C.txCertificates = C.TxCertificates C.CertificatesInAlonzoEra [stakeAddressRegCert] (C.BuildTxWith mempty)
      }
    keyWitnesses = [C.WitnessGenesisUTxOKey payerSKey]
  txBody0 :: C.TxBody C.AlonzoEra <- HE.leftFail $ C.makeTransactionBody tx0
  let
    feeLovelace = TN.calculateFee pparams (length $ C.txIns tx0) (length $ C.txOuts tx0) 0 (length keyWitnesses) networkId txBody0 :: C.Lovelace
    fee = C.TxFeeExplicit C.TxFeesExplicitInAlonzoEra feeLovelace
    tx1 = tx0 { C.txFee = fee, C.txOuts = [TN.mkAddressAdaTxOut payerAddress $ totalLovelace - feeLovelace] }

  txBody <- HE.leftFail $ C.makeTransactionBody tx1
  let tx = C.signShelleyTransaction txBody keyWitnesses
  return (tx, txBody)

registerPool
  :: C.LocalNodeConnectInfo C.CardanoMode -> C.NetworkId -> C.ProtocolParameters -> FilePath
  -> [C.ShelleyWitnessSigningKey] -> [C.StakeCredential] -> C.Address C.ShelleyAddr
  -> HE.Integration (C.PoolId, C.Tx C.AlonzoEra, C.TxBody C.AlonzoEra)
registerPool con networkId pparams tempAbsPath   keyWitnesses stakeCredentials payerAddress = do

  -- Create the metadata file
  HE.lbsWriteFile (tempAbsPath </> "poolMetadata.json") . J.encode $ J.object
    -- [ "heavyDelThd" .= J.toJSON @String "300000000000"
    [ "name" .= id @String "TestPool"
    , "description" .= id @String "The pool that tests all the pools"
    , "ticker" .= id @String "TEST"
    , "homepage" .= id @String "https://teststakepool.com"
    ]
  lbs <- HE.lbsReadFile (tempAbsPath </> "poolMetadata.json")
  -- cardano-cli stake-pool metadata-hash --pool-metadata-file
  (_poolMetadata, poolMetadataHash) <- HE.leftFail $ C.validateAndHashStakePoolMetadata $ BL.toStrict lbs

  -- TODO: these are missing from the tutorial? https://developers.cardano.org/docs/stake-pool-course/handbook/register-stake-pool-metadata
  coldSKey :: C.SigningKey C.StakePoolKey <- liftIO $ C.generateSigningKey C.AsStakePoolKey
  let coldVKey = C.getVerificationKey coldSKey :: C.VerificationKey C.StakePoolKey
      coldVKeyHash = (C.verificationKeyHash coldVKey :: C.Hash C.StakePoolKey) :: C.PoolId
  skeyVrf :: C.SigningKey C.VrfKey <- liftIO $ C.generateSigningKey C.AsVrfKey
  let vkeyVrf = C.getVerificationKey skeyVrf :: C.VerificationKey C.VrfKey

  -- A key for where stakepool rewards go
  stakeSKey :: C.SigningKey C.StakeKey <- liftIO $ C.generateSigningKey C.AsStakeKey
  let stakeVKey = C.getVerificationKey stakeSKey :: C.VerificationKey C.StakeKey

  -- Generate Stake pool registration certificate: cardano-cli stake-pool registration-certificate
  let poolRegistration :: C.Certificate
      poolRegistration = makeStakePoolRegistrationCert_
        coldVKey -- stakePoolVerKey; :: C.VerificationKey C.StakePoolKey; node key-gen
        vkeyVrf -- vrfVerKey -> C.VerificationKey C.VrfKey
        0 -- pldg -> C.Lovelace
        0 -- pCost -> C.Lovelace
        0 -- pMrgn -> Rational
        stakeVKey -- rwdStakeVerKey -> C.VerificationKey C.StakeKey -- TODO correct key used?
        [] -- sPoolOwnerVkeys -> [C.VerificationKey C.StakeKey]
        [] -- relays -> [C.StakePoolRelay]
        (Just $ C.StakePoolMetadataReference "" poolMetadataHash) -- -> Maybe C.StakePoolMetadataReference
        networkId -- -> C.NetworkId

  -- Generate delegation certificate pledge: cardano-cli stake-address delegation-certificate
  let delegationCertificates = map (\c -> C.makeStakeAddressDelegationCertificate c coldVKeyHash) stakeCredentials
      keyWitnesses' = keyWitnesses <> [ C.WitnessStakePoolKey coldSKey ]

  -- Create transaction
  do (txIns, totalLovelace) <- TN.getAddressTxInsValue con payerAddress
     let
       dummyFee = 0
       tx0 = (TN.emptyTxBodyContent dummyFee pparams)
         { C.txIns = (map (, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) txIns)
         , C.txOuts = [TN.mkAddressAdaTxOut payerAddress $ totalLovelace - dummyFee]
         , C.txCertificates = C.TxCertificates C.CertificatesInAlonzoEra
           ([poolRegistration] <> delegationCertificates)
           (C.BuildTxWith mempty) -- BuildTxWith build (Map StakeCredential (Witness WitCtxStake era))
         }
     txBody0 :: C.TxBody C.AlonzoEra <- HE.leftFail $ C.makeTransactionBody tx0
     let
       -- cardano-cli transaction calculate-min-fee
       feeLovelace = TN.calculateFee pparams (length $ C.txIns tx0) (length $ C.txOuts tx0) 0 (length keyWitnesses') networkId txBody0 :: C.Lovelace
       fee = C.TxFeeExplicit C.TxFeesExplicitInAlonzoEra feeLovelace
       tx1 = tx0 { C.txFee = fee, C.txOuts = [TN.mkAddressAdaTxOut payerAddress $ totalLovelace - feeLovelace] }

     txBody :: C.TxBody C.AlonzoEra <- HE.leftFail $ C.makeTransactionBody tx1
     let tx = C.signShelleyTransaction txBody keyWitnesses'

     return (coldVKeyHash, tx, txBody)
