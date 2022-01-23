import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import           Data.Maybe            (isNothing)

import           Model

tests :: TestTree
tests = testGroup "Utxo index" [hfProperties]

hfProperties :: TestTree
hfProperties = testGroup "Historical fold" []

prop_hfNewReturnsNothing :: (a -> b -> a) -> Int -> a -> Bool
prop_hfNewReturnsNothing fn depth acc = isNothing $ new fn depth acc

main :: IO ()
main = defaultMain tests
