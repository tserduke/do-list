module Main (main) where

import Criterion.Main

import Control.Monad.Writer
import Data.DoList
import Data.DoMonoid


main :: IO ()
main = defaultMain $ toList $ do
  benchGroup "Sum" $ do
    whnfBench "Writer"   (sumWriter 1 2 3 4) 5
    whnfBench "DoList"   (sumList   1 2 3 4) 5
    whnfBench "DoMonoid" (sumMonoid 1 2 3 4) 5
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


sumWriter, sumList, sumMonoid :: Int -> Int -> Int -> Int -> Int -> Sum Int

{-# NOINLINE sumWriter #-}
sumWriter x1 x2 x3 x4 x5 = execWriter $ do
  tell $ Sum x1
  tell $ Sum x2
  tell $ Sum x3
  tell $ Sum x4
  tell $ Sum x5

{-# NOINLINE sumList #-}
sumList x1 x2 x3 x4 x5 = mconcat $ toList $ do
  item $ Sum x1
  item $ Sum x2
  item $ Sum x3
  item $ Sum x4
  item $ Sum x5

{-# NOINLINE sumMonoid #-}
sumMonoid x1 x2 x3 x4 x5 = runDoM $ do
  DoM $ Sum x1
  DoM $ Sum x2
  DoM $ Sum x3
  DoM $ Sum x4
  DoM $ Sum x5


benchGroup :: String -> DoList Benchmark -> DoList Benchmark
benchGroup name = item . bgroup name . toList

whnfBench :: String -> (a -> b) -> a -> DoList Benchmark
whnfBench name func = item . bench name . whnf func
