{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plutus.ChainIndex.Logging where

import Cardano.BM.Configuration (setup)
import Cardano.BM.Configuration.Model qualified as CM
import Cardano.BM.Data.BackendKind (BackendKind (AggregationBK, EKGViewBK, KatipBK, MonitoringBK))
import Cardano.BM.Data.Configuration (Endpoint (Endpoint))
import Cardano.BM.Data.Output (ScribeDefinition (ScribeDefinition, scFormat, scKind, scMaxSev, scMinSev, scName, scPrivacy, scRotation),
                               ScribeFormat (ScText), ScribeKind (StdoutSK), ScribePrivacy (ScPublic))
import Cardano.BM.Data.Severity (Severity (Info))

-- | Logging (definitions from Plutus.PAB.Monitoring.Config)

-- | A default 'CM.Configuration' that logs on 'Info' and above
--   to stdout
defaultConfig :: IO CM.Configuration
defaultConfig = do
  c <- CM.empty
  CM.setMinSeverity c Info
  CM.setSetupBackends c [ KatipBK
                        , AggregationBK
                        , MonitoringBK
                        , EKGViewBK
                        ]
  CM.setDefaultBackends c [KatipBK, AggregationBK, EKGViewBK]
  CM.setSetupScribes c [ ScribeDefinition {
                          scName = "stdout"
                        , scKind = StdoutSK
                        , scFormat = ScText
                        , scPrivacy = ScPublic
                        , scRotation = Nothing
                        , scMinSev = minBound
                        , scMaxSev = maxBound
                        }]
  CM.setDefaultScribes c ["StdoutSK::stdout"]
  CM.setEKGBindAddr c $ Just (Endpoint ("localhost", 12790))
  pure c

-- | Load a 'CM.Configuration' from a YAML file.
loadConfig :: FilePath -> IO CM.Configuration
loadConfig = setup
