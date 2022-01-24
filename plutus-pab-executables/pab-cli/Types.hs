{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
    (
    -- * Generate config types
      AppOpts(..)
    , ConfigCommand(..)
    , NetworkName(..)
    -- * Node config types
    , NodeOpts(..)
    , NodeDirectory(..)
    , NodeDbPath
    , createNodeDbDirPath
    , getNodeDbDirPath
    , NodeSocketPath(..)
    , createNodeSocketFilePath
    , getNodeSocketFilePath
    , removeNodeSocket
    , waitUntilNodeSocketExists
    , NodePort(..)
    -- * Chain index config types
    , ChainIndexOpts(..)
    , ChainIndexDbPath
    , createChainIndexDbPath
    , getChainIndexDbPath
    , ChainIndexPort(..)
    -- * PAB config types
    , PABOpts(..)
    , PABDirectory(..)
    , createPABDirectory
    , PABExe(..)
    , PABConfigPath
    , createPabConfigFilePath
    , getPabConfigFilePath
    , PABDbPath
    , createPabDbFilePath
    , getPabDbFilePath
    , PABPort(..)
    -- * WBE config types
    , WBEDbDirectory
    , createWbeDatabaseDir
    , createWbeDatabaseDirPath
    , getWbeDatabaseDirPath
    , WalletPort(..)
    -- * Other types
    , AppError(..)
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception)
import Control.Monad (unless)
import Control.Monad.Catch (catch)
import Data.Word (Word32)
import Plutus.ChainIndex.Types (Point)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))

data AppOpts =
    AppOpts { appOptsCommand        :: ConfigCommand
            , appOptsNodeOpts       :: NodeOpts
            , appOptsChainIndexOpts :: ChainIndexOpts
            } deriving (Show)

data PABOpts =
    PABOpts { pabOptsExe             :: PABExe
            , pabOptsOutputDir       :: PABDirectory
            , pabOptsPort            :: PABPort
            , pabOptsDbPoolSize      :: Int
            , pabOptsRollbackHistory :: Maybe Int
            , pabOptsPassphrase      :: Maybe String
            , pabOptsResumeFromPoint :: Point
            } deriving (Show)

newtype ChainIndexOpts =
    ChainIndexOpts { chainIndexOptsPort :: ChainIndexPort
                   } deriving (Show)

data NodeOpts =
    NodeOpts { nodeOptsOutputDir :: NodeDirectory
             , nodeOptsPort      :: NodePort
             } deriving (Show)

data ConfigCommand =
    MockNetCommand PABOpts WalletPort
  | NodeWBECommand NetworkName (Maybe PABOpts) WalletPort
  | NodeRemoteWalletCommand NetworkName (Maybe PABOpts)
    deriving (Show)

newtype NodeDirectory = NodeDirectory { unNodeDirectory :: FilePath }
    deriving (Show)

newtype NodeSocketPath = NodeSocketPath { unNodeSocketPath :: FilePath }
    deriving (Show)

-- | Smart constructor for 'NodeSocketPath'.
createNodeSocketFilePath :: NodeDirectory -> NodeSocketPath
createNodeSocketFilePath (NodeDirectory nd) = NodeSocketPath $ nd </> "node.sock"

getNodeSocketFilePath :: NodeSocketPath -> FilePath
getNodeSocketFilePath = unNodeSocketPath

removeNodeSocket :: NodeSocketPath -> IO ()
removeNodeSocket (NodeSocketPath p) =
    -- All kinds of possible exceptions here such as
    -- `isDoesNotExistError`, `isPermissionError`, etc.
    catch (removeFile p) $ \(_ :: IOError) -> pure ()

-- | Waits until the given node socket path finally exists by polling the
-- filesystem.
waitUntilNodeSocketExists :: NodeSocketPath -> IO ()
waitUntilNodeSocketExists (NodeSocketPath np) = go
  where
      go = do
          nodeSocketExists <- doesFileExist np
          unless nodeSocketExists $ threadDelay 5000000 >> go

newtype NodeDbPath = NodeDbPath { unNodeDbPath :: FilePath }
    deriving (Show)

-- | Smart constructor for NodeDbPath'.
createNodeDbDirPath :: NodeDirectory -> NodeDbPath
createNodeDbDirPath (NodeDirectory d) = NodeDbPath $ d </> "db"

getNodeDbDirPath :: NodeDbPath -> FilePath
getNodeDbDirPath = unNodeDbPath

newtype NodePort = NodePort { unNodePort :: Word32 }
    deriving (Show)

newtype PABExe = PABExe { unPABExe :: FilePath }
    deriving (Show)

newtype PABConfigPath = PABConfigPath { unPABConfigPath :: FilePath }
    deriving (Show)

createPabConfigFilePath :: PABDirectory -> PABConfigPath
createPabConfigFilePath (PABDirectory d) =
    PABConfigPath $ d </> "plutus-pab.yaml"

getPabConfigFilePath :: PABConfigPath -> FilePath
getPabConfigFilePath = unPABConfigPath

newtype PABPort = PABPort { unPABPort :: Word32 }
    deriving (Show)

newtype PABDirectory = PABDirectory { unPABDirectory :: FilePath }
    deriving (Show)

createPABDirectory :: PABDirectory -> IO ()
createPABDirectory (PABDirectory d) =
    createDirectoryIfMissing False d

newtype PABDbPath = PABDbPath { unPABDbPath :: FilePath }
    deriving (Show)

-- | Smart constructor for 'PABDbPath.
createPabDbFilePath :: PABDirectory -> PABDbPath
createPabDbFilePath (PABDirectory pd) = PABDbPath $ pd </> "pab-core.db"

getPabDbFilePath :: PABDbPath -> FilePath
getPabDbFilePath = unPABDbPath

-- The show instance should output 'Testnet' and 'Mainnet'. Used in 'fetchNodeConfigFiles'.
data NetworkName = Testnet | Mainnet
    deriving (Show)

newtype ChainIndexPort = ChainIndexPort { unChainIndexPort :: Word32 }
    deriving (Show)

newtype ChainIndexDbPath = ChainIndexDbPath { unChainIndexDbPath :: FilePath }
    deriving (Show)

-- | Smart constructor for NodeDbPath'.
createChainIndexDbPath :: NodeDirectory -> ChainIndexDbPath
createChainIndexDbPath (NodeDirectory d) = ChainIndexDbPath $ d </> "chain-index.db"

getChainIndexDbPath :: ChainIndexDbPath -> FilePath
getChainIndexDbPath = unChainIndexDbPath

newtype WalletPort = WalletPort { unWalletPort :: Word32 }
    deriving (Show)

newtype WBEDbDirectory = WBEDbDirectory { unWBEDbDirectory :: FilePath }
    deriving (Show)

-- | Smart constructor for NodeDbPath'.
createWbeDatabaseDirPath :: NodeDirectory -> WBEDbDirectory
createWbeDatabaseDirPath (NodeDirectory d) = WBEDbDirectory $ d </> "wbe"

createWbeDatabaseDir :: WBEDbDirectory -> IO ()
createWbeDatabaseDir (WBEDbDirectory d)= do
    createDirectoryIfMissing False d

getWbeDatabaseDirPath :: WBEDbDirectory -> FilePath
getWbeDatabaseDirPath = unWBEDbDirectory

data AppError = ChainIndexPortError
              | PABPortError
              | PABExeNotProvidedError
              | PABExeDoesNotExistError
              | PABExeNotExecutableError
              | PABDBPoolSizeError
              | PABRollbackHistoryNotNumberError
              | PABResumeFromBlockIdNotSpecifiedError
              | PABResumeFromSlotNotSpecifiedError
              | PABResumeFromSlotNotANumberError
              | NodePortError
              | UnspecifiedNodeNetworkError
              | WalletPortError
              | DirectoryDoesNotExistError FilePath

instance Exception AppError

instance Show AppError where
    show ChainIndexPortError = "Option --chain-index-port should be a positive number"
    show PABPortError = "Option --pab-port should be a positive number"
    show PABExeNotProvidedError = "Option --pab-exe should be provided"
    show PABExeDoesNotExistError = "Executable specified in --pab-exe is not available in the current PATH"
    show PABExeNotExecutableError = "Current user does not have executable permissions for the executable specified in --pab-exe."
    show PABDBPoolSizeError = "Option --pab-db-pool-size should be a positive number"

    show PABRollbackHistoryNotNumberError = "Option --pab-rollback-history is not a number"
    show PABResumeFromBlockIdNotSpecifiedError = "Option --pab-resume-from-block-id is not specified"
    show PABResumeFromSlotNotSpecifiedError = "Option --pab-resume-from-slot is not specified"
    show PABResumeFromSlotNotANumberError = "Option --pab-resume-from-slot should be positive number"

    show NodePortError = "Option --node-port should be a positive number"
    show UnspecifiedNodeNetworkError = "Unspecified cardano network"
    show WalletPortError = "Option --wallet-port should be a positive number"
    show (DirectoryDoesNotExistError fp) = "The following directory does not exist: " <> fp
