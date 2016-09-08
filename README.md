# do-list
[DoList] makes it easy to use do notation for defining list structures.

## Examples

### Benchmarks
```haskell
module Main (main) where

import Criterion.Main
import Data.DoList (DoList, item, toList)

main :: IO ()
main = defaultMain $ toList $ do
  doBench "plus"  $ whnf (2 +) (1 :: Int)
  doBench "minus" $ whnf (2 -) (1 :: Int)

-- Now we can define benchmarks with do notation.
doBench :: String -> Benchmarkable -> DoList Benchmark ()
doBench name = item . bench name
```

### Multiline Text
```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import BasicPrelude
import Data.DoList (DoList, toList)

main :: IO ()
main = putStr $ runLines $ do
  "fib 0 = 0"
  "fib 1 = 1"
  "fib n = fib (n-1) + fib (n-2)"

runLines :: DoList Text () -> Text
runLines = unlines . toList
```

[DoList]: https://hackage.haskell.org/package/do-list/docs/Data-DoList.html
