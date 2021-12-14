{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

import Plutus.Contract.Wallet (ExportTx, export)
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson as Aeson
import GHC.Generics
import Ledger.Constraints.OffChain (ScriptLookups (..), mkTx)
import Ledger.Constraints.TxConstraints (UntypedConstraints
                                        , TxConstraints(..)
                                        , TxConstraint(..)
                                        , InputConstraint(..)
                                        , OutputConstraint(..))
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.Word (Word32)
import Ledger.Typed.TypeUtils (Any)
import System.Exit (die)
import System.Environment (getArgs)

import Plutus.V1.Ledger.Ada qualified as Ada
import Ledger (unitRedeemer)

{-
   How to get some input data?
     protocol-parameters: protocol-parameters.json in the plutus-use-cases/scripts directory
     network-id:          use 'mainnet' or a number indicating the testnet id
     scriptlookups:       generate some with pab-mktx-gendata
     txconstraints:       generate some with pab-mktx-gendata

 -}

main :: IO ()
main = do
    -- minimal command line parser to avoid pulling in more than the JS lib would use
    args <- getArgs
    case args of
        [pparams, nwid, lookups, constraints] -> do
            let nwid' = if nwid == "mainnet" 
                        then C.Mainnet
                        else C.Testnet . C.NetworkMagic . read $ nwid
                readJSONFile file = do
                    bs <- BSL.readFile file
                    case Aeson.eitherDecode bs of
                        Left err -> die ("error decoding JSON file " ++ file ++ " " ++ show err)
                        Right x -> pure x
            pparams'     <- readJSONFile pparams
            lookups'     <- readJSONFile lookups
            constraints' <- readJSONFile constraints
            let result = makeTransaction pparams' nwid' lookups' constraints'
            BSL.putStrLn (Aeson.encode $ fmap Aeson.toJSON result)
        _ -> do
            die "usage: pab-mktx protocol-parameters.json network-id scriptlookups.json txconstraints.json"

{-
  The function we intend to expose to JavaScript
  
    - All parameters can be supplied as a JSON value based on their
      Aeson FromJSON/ToJSON instances.
    - The result/error is also returned as a JSON value, based on the
      standard schema.

  Questions:
    - Is this the correct functionality to have in JS
    - We use the types
          UntypedConstraints
          ScriptLookups Any
      Are these the best types to use here, or do we need anything to support
      other BuiltinData related types?
    - How do we query the chain index in JS, do we need to make more Haskell
      functionality available for this?
                 
 -}
makeTransaction :: C.ProtocolParameters
                -> C.NetworkId
                -> ScriptLookups Any
                -> UntypedConstraints
                -> Either Aeson.Value ExportTx
makeTransaction params nwid lookups constraints =
    case mkTx lookups constraints of
        Left err   -> Left (Aeson.toJSON err)
        Right ubtx -> either (Left . Aeson.toJSON) Right (export params nwid ubtx)


