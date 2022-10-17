{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Ledger.Tx.Orphans where

import Ledger.Address.Orphans ()
import Ledger.Builtins.Orphans ()
import Ledger.Credential.Orphans ()
import Ledger.Scripts.Orphans ()
import Ledger.Tx.Orphans.V1 ()
import Ledger.Tx.Orphans.V2 ()
import Ledger.Value.Orphans ()
