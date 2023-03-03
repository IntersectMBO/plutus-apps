module Marconi.Core.Trace
  ( ConvertibleEvent(..)
  , TraceEvent(..)
  , Trace
  , trace
  , modelConversion
  , prop_WeakBisimilarity
  , prop_WeakBisimilarity'
  ) where

import Marconi.Core.Model (GrammarBuilder (GrammarBuilder), Index (Insert, New, Rewind),
                           ObservedBuilder (ObservedBuilder))
import Marconi.Core.Model qualified as Model

import Data.Maybe (fromJust)
import GHC.Generics (Generic)

import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Tasty.QuickCheck (Property, (==>))

data TraceEvent =
     TRollForward
   | TRollBack
   deriving (Show, Eq, Generic)

type Trace = [TraceEvent]

-- A trace of an indexer program is an observation over said program
trace :: Index a e n -> Trace
trace New {}        = []
trace (Insert _ ix) = TRollForward : trace ix
trace (Rewind _ ix) = TRollBack : trace ix

-- This will help with converting indexer events back into model events, and runing
-- indexers in some generic monad. I am assuming we only use `IO`.
newtype ConvertibleEvent a e n = ConvertibleEvent
  { convertTrace :: Index a e n -> IO Trace }

modelConversion :: ConvertibleEvent a e n
modelConversion =  ConvertibleEvent $ pure . trace

{- The only property we care about is a form of weak bisimilarity between the model
   trace and the trace produced by the indexer -}

prop_WeakBisimilarity
  :: forall e a n.
     ConvertibleEvent a e n
  -> ObservedBuilder a e n
  -> Property
prop_WeakBisimilarity c (ObservedBuilder ix) =
  -- The new indexer can not handle a depth of 1
  let v = fromJust $ Model.view ix
   in Model.ixDepth v >= 2 ==>
  monadicIO $ do
    let modelTrace = trace ix
    ixTrace <- run $ convertTrace c ix
    assert $ ixTrace == modelTrace

prop_WeakBisimilarity'
  :: forall e a n.
     ConvertibleEvent a e n
  -> GrammarBuilder a e n
  -> Property
prop_WeakBisimilarity' c (GrammarBuilder ix) =
  -- The new indexer can not handle a depth of 1
  let v = fromJust $ Model.view ix
   in Model.ixDepth v >= 2 ==>
  monadicIO $ do
    let modelTrace = trace ix
    ixTrace <- run $ convertTrace c ix
    assert $ ixTrace == modelTrace

