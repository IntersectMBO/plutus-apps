-- | This module provides a framework for testing Plutus contracts built on "Test.QuickCheck". The
--   testing is model based, so to test a contract you define a type modelling the state of the
--   contract (or set of contracts) and provide an instance of the `ContractModel` class. This
--   instance specifies what operations (`Action`s) the contract supports, how they interact with
--   the model state, and how to execute them in the blockchain emulator ("Plutus.Trace.Emulator").
--   Tests are evaluated by running sequences of actions (random or user-specified) in the emulator
--   and comparing the state of the blockchain to the model state at the end.
--
--   Test cases are written in the `DL` monad, which supports mixing fixed sequences of actions with
--   random actions, making it easy to write properties like
--   /it is always possible to get all funds out of the contract/.
module Plutus.Contract.Test.ContractModel ( module Interface ) where
import Plutus.Contract.Test.ContractModel.Interface as Interface
