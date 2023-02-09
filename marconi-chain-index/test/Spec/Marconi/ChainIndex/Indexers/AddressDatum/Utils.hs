module Spec.Marconi.ChainIndex.Indexers.AddressDatum.Utils
    ( addressInEraToAddressAny
    )
where

import Cardano.Api qualified as C

addressInEraToAddressAny :: C.AddressInEra era -> C.AddressAny
addressInEraToAddressAny (C.AddressInEra _ addr) = C.toAddressAny addr
