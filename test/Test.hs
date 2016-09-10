module Main (main) where

import Test.Hspec

import Data.DoList


main :: IO ()
main = hspec $ describe "DoList" $
  it "(>>)" $
    toList (item 1 >> item 2) `shouldBe` ([1, 2] :: [Int])
