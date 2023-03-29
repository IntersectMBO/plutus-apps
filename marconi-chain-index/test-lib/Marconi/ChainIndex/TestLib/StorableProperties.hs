{-# LANGUAGE FlexibleContexts #-}

module Marconi.ChainIndex.TestLib.StorableProperties where

import Cardano.Api qualified as C
import Control.Monad.IO.Class (liftIO)
import Data.List qualified as List
import Data.Ord (Down (Down))
import Hedgehog ((===))
import Hedgehog qualified as H
import Marconi.ChainIndex.Orphans ()
import Marconi.Core.Storable (Resumable, State, StorableMonad, StorablePoint, resume)

-- | The property verifies that the 'Storable.resumeFromStorage' call returns at least the
-- 'C.ChainPointAtGenesis' point.
--
-- TODO: ChainPointAtGenesis should always be returned by default. Don't need this property test.
propResumingShouldReturnAtLeastTheGenesisPoint ::
    ( Resumable h
    , StorablePoint h ~ C.ChainPoint
    , StorableMonad h ~ IO
    )
    => State h
    -> H.PropertyT IO ()
propResumingShouldReturnAtLeastTheGenesisPoint indexer = do
    actualResumablePoints <- liftIO $ resume indexer
    H.assert $ elem C.ChainPointAtGenesis actualResumablePoints

-- | The property verifies that the 'Storable.resumeFromStorage' call returns a sorted list of chain
-- points in descending order.
propResumablePointsShouldBeSortedInDescOrder ::
    ( Resumable h
    , StorablePoint h ~ C.ChainPoint
    , StorableMonad h ~ IO
    )
    => State h
    -> H.PropertyT IO ()
propResumablePointsShouldBeSortedInDescOrder indexer = do
    actualResumablePoints <- liftIO $ resume indexer
    actualResumablePoints === List.sortOn Down actualResumablePoints
