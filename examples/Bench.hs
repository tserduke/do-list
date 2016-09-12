{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Criterion.Main
import Data.DoList (DoList, item, toList)

main :: IO ()
main = defaultMain $ toList $ do
  doBench "add"  $ whnf (2 +) (1 :: Int)
  doBench "sub" $ whnf (2 -) (1 :: Int)
  -- Regular criterion benchmarks are injected via list overloading
  [multBench, divBench]

multBench, divBench :: Benchmark
multBench = bench "mult" $ whnf (2 *) (2 :: Int)
divBench = bench "div" $ whnf (2 `div`) (2 :: Int)

-- Now we can define benchmarks with do notation.
doBench :: String -> Benchmarkable -> DoList Benchmark
doBench name = item . bench name
