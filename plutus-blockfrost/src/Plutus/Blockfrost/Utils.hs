{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeApplications   #-}

module Plutus.Blockfrost.Utils where

import Data.Aeson

fromSucceed :: Result a -> a
fromSucceed (Error a)   = error $ show a
fromSucceed (Success a) = a
