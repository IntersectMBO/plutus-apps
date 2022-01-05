{-# LANGUAGE TypeApplications #-}
-- | Start a local cluster of cardano nodes and PAB(s)
module Main where

import ContractExample (ContractExample)
import Plutus.PAB.Effects.Contract.Builtin (handleBuiltin)
import Plutus.PAB.LocalCluster.Run (runWith)

main :: IO ()
main = runWith @ContractExample handleBuiltin
