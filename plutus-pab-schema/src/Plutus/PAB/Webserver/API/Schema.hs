{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Plutus.PAB.Webserver.API.Schema where

import Plutus.PAB.Webserver.Types.Schema
import Servant.API (Capture, Description, Get, JSON, (:<|>), (:>))
import Wallet.Types (ContractInstanceId)

type SchemaAPI t
    = "api" :> (
    "contract" :> ("instance" :>
                    (Capture "contract-instance-id" ContractInstanceId :>
                        "schema" :> Description "Endpoints' schema of contract instance." :> Get '[JSON] (ContractSchemaResponse t)
                    )
        )
      )
