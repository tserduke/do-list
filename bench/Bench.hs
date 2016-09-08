module Main (main) where

import Criterion.Main

import Control.Monad.Writer
import Data.DoList


main :: IO ()
main = defaultMain $ toList $ do
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
