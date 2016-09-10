module Main (main) where

import Test.Hspec

import Data.DoList
import Data.DoMonoid

import Data.Monoid


main :: IO ()
main = hspec $ do
  describe "DoMonoid" $
    it "(>>)" $
      runDoM (DoM (Sum 1) >> DoM (Sum 1)) `shouldBe` Sum (2 :: Int)
  describe "DoList" $
    it "(>>)" $
      toList (item 1 >> item 2) `shouldBe` ([1, 2] :: [Int])