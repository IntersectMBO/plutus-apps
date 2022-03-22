module Test.QuickCheck.DynamicLogic.Utils where

import Test.QuickCheck
import Test.QuickCheck.Property

withSize :: Testable prop => (Int -> prop) -> Property
withSize f = MkProperty . sized $ unProperty . property . f
