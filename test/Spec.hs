import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import           Data.Maybe            (isNothing)

import           Test.QuickCheck       (Blind (Blind), Fun (Fun), pattern Fn2,
                                        NonPositive (NonPositive))

import           Model                 (new)

tests :: TestTree
tests = testGroup "Utxo index" [hfProperties]

hfProperties :: TestTree
hfProperties = testGroup "Historical fold"
  [ QC.testProperty "Negative or zero depth" $ prop_hfNewReturnsNothing @Int @Int
  ]

prop_hfNewReturnsNothing
  :: Fun (a, b) a
  -> NonPositive Int
  -> a
  -> Bool
prop_hfNewReturnsNothing
  (Fn2 fn)
  (NonPositive depth)
  acc = isNothing $ new fn depth acc

main :: IO ()
main = defaultMain tests
