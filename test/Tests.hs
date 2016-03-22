{-# LANGUAGE OverloadedStrings #-}
module Tests (tests) where


import Distribution.TestSuite as T
import Test.HUnit


import Data.Proxy
import Language.PureScript.Bridge.TypeInfo


testTypeInfo :: IO Progress
testTypeInfo = do
  let expected = TypeInfo {
    typePackage = "ghc-prim"
  , typeModule = "GHC.Types"
  , typeName = "Int"
  , typeParameters = []
  }
  let tests = test ["Int Test" ~: expected ~=? mkTypeInfo (Proxy :: Proxy Int)]
  r <- runTestTT tests
  return $ if errors r == 0
    then Finished Pass
    else Finished $ Fail (show r)

tests :: IO [T.Test]
tests = return [ Test test1]
  where
    test1 = TestInstance
        { run = testTypeInfo
        , name = "Test TypeInfo code"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right test1
        }
