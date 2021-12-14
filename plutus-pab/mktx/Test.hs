{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Main where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.ByteString.Lazy qualified as BSL
import Ledger.Constraints.OffChain (MkTxError, ScriptLookups (..), UnbalancedTx, mkTx)
import Ledger.Constraints.TxConstraints (TxConstraints)
import Ledger.Typed.TypeUtils (Any)
import Plutus.Contract.Wallet (export)
import System.Exit (die)

import PlutusTx qualified

import Data.Aeson
import Data.ByteString.Lazy qualified as B

import Control.Monad (when)
import GHC.Generics
import System.Environment
import System.FilePath ((<.>), (</>))

{-
   XXX after merging Jann's change, import this from
   plutus-contract:Plutus.Contract.Request
 -}
data MkTxLog =
    MkTxLog
        { mkTxLogLookups       :: ScriptLookups Any
        , mkTxLogTxConstraints :: TxConstraints PlutusTx.BuiltinData PlutusTx.BuiltinData
        , mkTxLogResult        :: Either MkTxError UnbalancedTx
        }
        deriving stock (Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

readJSONFile :: FromJSON a => FilePath -> IO a
readJSONFile file = do
    bs <- BSL.readFile file
    case Data.Aeson.eitherDecode bs of
        Left err -> die ("error decoding JSON file " ++ file ++ " " ++ show err)
        Right x  -> pure x

runTxTest :: Bool -> C.ProtocolParameters -> FilePath -> IO ()
runTxTest generate protocol_parameters test_file = do
    putStr ("running test " ++ test_file ++ ": ")
    test_case <- readJSONFile test_file
    let result = mkTx (mkTxLogLookups test_case)
                      (mkTxLogTxConstraints test_case)
    when (result /= mkTxLogResult test_case) (die "mktx failure")
    case result of
        Right unbound_tx -> do
            let exp_file = test_file <.> "export"
                exp_tx = export protocol_parameters C.Mainnet unbound_tx
            if generate
            then B.writeFile exp_file (encode $ toJSON exp_tx)
            else do
              exp_expected <- readJSONFile exp_file
              when (exp_tx /= exp_expected) (die "export failure")
        _                -> pure ()
    putStrLn "[OK]"

testcases :: [String]
testcases = [ "crowdfunding-success-1-mkTx"
            , "crowdfunding-success-2-mkTx"
            , "crowdfunding-success-3-mkTx"
            , "crowdfunding-success-4-mkTx"
            , "currency-1-mkTx"
            , "escrow-redeem_1-1-mkTx"
            , "escrow-redeem_1-2-mkTx"
            , "escrow-redeem_1-3-mkTx"
            , "escrow-redeem_2-1-mkTx"
            , "escrow-redeem_2-2-mkTx"
            , "escrow-redeem_2-3-mkTx"
            , "escrow-redeem_2-4-mkTx"
            , "escrow-refund-1-mkTx"
            , "escrow-refund-2-mkTx"
            , "future-increase-margin-1-mkTx"
            , "future-increase-margin-2-mkTx"
            , "future-increase-margin-3-mkTx"
            , "future-increase-margin-4-mkTx"
            , "future-pay-out-1-mkTx"
            , "future-pay-out-2-mkTx"
            , "future-pay-out-3-mkTx"
            , "future-pay-out-4-mkTx"
            , "future-settle-early-1-mkTx"
            , "future-settle-early-2-mkTx"
            , "future-settle-early-3-mkTx"
            , "future-settle-early-4-mkTx"
            , "game-sm-success_1-1-mkTx"
            , "game-sm-success_2-1-mkTx"
            , "multisig-failure-1-mkTx"
            , "multisig-sm-1-mkTx"
            , "multisig-success-1-mkTx"
            , "ping-pong_1-1-mkTx"
            , "ping-pong_2-1-mkTx"
            , "prism-1-mkTx"
            , "prism-2-mkTx"
            , "prism-3-mkTx"
            , "pubkey-1-mkTx"
            , "pubkey-2-mkTx"
            , "stablecoin_1-1-mkTx"
            , "stablecoin_2-1-mkTx"
            , "token-account-1-mkTx"
            , "token-account-2-mkTx"
            , "uniswap-1-mkTx"
            , "uniswap-2-mkTx"
            , "uniswap-3-mkTx"
            , "uniswap-4-mkTx"
            , "uniswap-5-mkTx"
            , "uniswap-6-mkTx"
            , "uniswap-7-mkTx"
            , "uniswap-8-mkTx"
            , "vesting-1-mkTx"
            , "vesting-2-mkTx"
            ]

main :: IO ()
main = do
    args <- getArgs
    let generate = args == ["generate"]
    -- test transactions against known results
    pparams <- readJSONFile ("mktx" </> "protocol-parameters.json")
    mapM_ (runTxTest generate pparams)
          (map (\x -> "mktx" </> "test" </> x <.> "json") testcases)
    -- XXX test JSON construction/consistency
