{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.Tasty
import Test.Tasty.HUnit
import Data.JSON.TH
import Data.ByteString

main = defaultMain $ 
  testCase "A test test case" $ do
    (parseJSON 
      "{ \"one\" : [1, 2, 5] , \"two\" : false , \"three\" : 345 }")
      @?= Right (TestRec [1, 2, 5] False 345)

data TestRec = TestRec { one :: [Int], two :: Bool, three :: Int } 
  deriving (Eq, Show)

mkJSON ''TestRec