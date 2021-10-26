module View.Events
  ( utxoIndexPane
  ) where

import Prologue
import Bootstrap (cardBody_, cardHeader_, card_)
import Chain.View as Chain
import Data.Lens (view)
import Data.Map as Map
import Halogen.HTML (HTML, h2_, text)
import Ledger.Index (UtxoIndex)
import Plutus.V1.Ledger.Tx (TxOut)
import Playground.Lenses (_utxoIndexEntries)
import Types (HAction(..))

utxoIndexPane :: forall p a. UtxoIndex -> HTML p (HAction a)
utxoIndexPane utxoIndex =
  card_
    [ cardHeader_
        [ h2_ [ text "UtxoIndex" ] ]
    , cardBody_
        (snd <<< map utxoEntryPane <$> Map.toUnfoldable (view _utxoIndexEntries utxoIndex))
    ]

utxoEntryPane :: forall p a. TxOut -> HTML p (HAction a)
utxoEntryPane txOut = ChainAction <$> Chain.txOutOfView (const Nothing) false txOut Nothing
