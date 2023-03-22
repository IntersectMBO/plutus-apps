module Plutus.ChainIndex.Compatibility where

import Cardano.Api (Block (..), BlockHeader (..), BlockInMode (..), BlockNo (..), CardanoMode, ChainPoint (..),
                    ChainTip (..), Hash, SlotNo (..))
import Cardano.Api.Shelley (Hash (HeaderHash))
import Data.ByteString.Short (fromShort, toShort)
import Ledger (BlockId (..), Slot (..))
import Plutus.ChainIndex.Tx (ChainIndexTx (..))
import Plutus.ChainIndex.Types (BlockNumber (..), Point (..), Tip (..))
import Plutus.Contract.CardanoAPI qualified as C

fromCardanoTip :: ChainTip -> Tip
fromCardanoTip (ChainTip slotNo hash blockNo) =
    Tip { tipSlot = fromCardanoSlot slotNo
        , tipBlockId = fromCardanoBlockId hash
        , tipBlockNo = fromCardanoBlockNo blockNo
        }
fromCardanoTip ChainTipAtGenesis = TipAtGenesis

fromCardanoPoint :: ChainPoint -> Point
fromCardanoPoint ChainPointAtGenesis = PointAtGenesis
fromCardanoPoint (ChainPoint slot hash) =
    Point { pointSlot = fromCardanoSlot slot
          , pointBlockId = fromCardanoBlockId hash
          }

toCardanoPoint :: Point -> ChainPoint
toCardanoPoint PointAtGenesis = ChainPointAtGenesis
toCardanoPoint (Point slot blockId) =
    ChainPoint (fromIntegral slot) $ toCardanoBlockId blockId

tipFromCardanoBlock
  :: BlockInMode CardanoMode
  -> Tip
tipFromCardanoBlock (BlockInMode (Block (BlockHeader slot hash block) _) _) =
    fromCardanoTip $ ChainTip slot hash block

fromCardanoSlot :: SlotNo -> Slot
fromCardanoSlot (SlotNo slotNo) = Slot $ toInteger slotNo

fromCardanoBlockId :: Hash BlockHeader -> BlockId
fromCardanoBlockId (HeaderHash hash) =
    BlockId $ fromShort hash

toCardanoBlockId :: BlockId -> Hash BlockHeader
toCardanoBlockId (BlockId bs) =
    HeaderHash $ toShort bs

fromCardanoBlockHeader :: BlockHeader -> Tip
fromCardanoBlockHeader (BlockHeader slotNo hash blockNo) =
    Tip { tipSlot = fromCardanoSlot slotNo
        , tipBlockId = fromCardanoBlockId hash
        , tipBlockNo = fromCardanoBlockNo blockNo
        }

fromCardanoBlockNo :: BlockNo -> BlockNumber
fromCardanoBlockNo (BlockNo blockNo) = BlockNumber blockNo

fromCardanoBlock
    :: BlockInMode CardanoMode
    -> [ChainIndexTx]
fromCardanoBlock = C.fromCardanoBlock
