module Main (main) where

import Criterion.Main

import Control.Monad.Writer
import Data.DoList


main :: IO ()
main = defaultMain $ toList $ do
  benchGroup "Sum" $ do
    whnfBench "Writer" execWriter $ do
      tell $ Sum (1 :: Int)
      tell $ Sum 2
      tell $ Sum 3
      tell $ Sum 4
      tell $ Sum 5
    whnfBench "DoList" (mconcat . toList) $ do
      item $ Sum (1 :: Int)
      item $ Sum 2
      item $ Sum 3
      item $ Sum 4
      item $ Sum 5
  benchGroup "List" $ do
    whnfBench "Writer" (last . execWriter) $ do
      tell [1 :: Int]
      tell [2]
      tell [3]
      tell [4]
      tell [5]
    whnfBench "DoList" (last . toList) $ do
      item (1 :: Int)
      item 2
      item 3
      item 4
      item 5
  benchGroup "Combined List" $ do
    whnfBench "Writer" (last . execWriter) $ do
      tell [1 :: Int, 2, 3]
      tell [4, 5, 6]
      tell [7, 8, 9]
    whnfBench "DoList" (last . toList) $ do
      fromList [1 :: Int, 2, 3]
      fromList [3, 4, 5]
      fromList [7, 9, 9]


benchGroup :: String -> DoList Benchmark -> DoList Benchmark
benchGroup name = item . bgroup name . toList

whnfBench :: String -> (a -> b) -> a -> DoList Benchmark
whnfBench name func = item . bench name . whnf func
