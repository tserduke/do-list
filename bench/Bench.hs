module Main (main) where

import Criterion.Main

import Control.Monad.Writer
import Data.DoList


main :: IO ()
main = defaultMain $ toList $ do
  item $ bgroup "Int" $ toList $ do
    item $ bench "Writer" $ whnf execWriter $ do
      tell $ Sum (1 :: Int)
      tell $ Sum 2
      tell $ Sum 3
      tell $ Sum 4
      tell $ Sum 5
    item $ bench "DoList" $ whnf (mconcat . toList) $ do
      item $ Sum (1 :: Int)
      item $ Sum 2
      item $ Sum 3
      item $ Sum 4
      item $ Sum 5
  item $ bgroup "List" $ toList $ do
    item $ bench "Writer" $ whnf (last . execWriter) $ do
      tell [1 :: Int]
      tell [2]
      tell [3]
      tell [4]
      tell [5]
    item $ bench "DoList" $ whnf (last . toList) $ do
      item (1 :: Int)
      item 2
      item 3
      item 4
      item 5
  item $ bgroup "Nested Lists" $ toList $ do
    item $ bench "Writer" $ whnf (last . execWriter) $ do
      tell [1 :: Int, 2, 3]
      tell [4, 5, 6]
      tell [7, 8, 9]
    item $ bench "DoList" $ whnf (last . toList) $ do
      item [1 :: Int, 2, 3]
      item [3, 4, 5]
      item [7, 9, 9]