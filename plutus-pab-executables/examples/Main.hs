{-# LANGUAGE TypeApplications #-}

module Main
    ( main
    ) where

import ContractExample (ContractExample)
import Data.Proxy
import Plutus.PAB.Effects.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run (runWithExtra)
import Plutus.PAB.Webserver.API.Schema
import Plutus.PAB.Webserver.Handler.Schema

main :: IO ()
main = do
    runWithExtra schemaServer (Proxy @(SchemaAPI (Contract.ContractDef ContractExample))) (Builtin.handleBuiltin @ContractExample)
