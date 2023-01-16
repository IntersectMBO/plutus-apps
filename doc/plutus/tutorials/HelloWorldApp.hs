{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
module HelloWorldApp where

import Data.Text qualified as T
import Plutus.Contract (Contract, EmptySchema, Endpoint, logInfo)
import PlutusTx.Prelude ()

-- BLOCK1

-- | A 'Contract' that logs a message.
hello :: Contract () EmptySchema T.Text ()
hello = logInfo @String "Hello, world"

-- BLOCK2

endpoints :: Contract () EmptySchema T.Text ()
endpoints = hello

type DummySchema = Endpoint "dummy" ()
