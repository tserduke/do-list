# do-list [![Hackage][hackage-badge]][hackage]
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
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import Data.DoMonoid (runDoM)
import Data.Text.IO as T (putStr)

main :: IO ()
main = T.putStr $ runDoM $ do
  "fib 0 = 0\n"
  "fib 1 = 1\n"
  "fib n = fib (n-1) + fib (n-2)\n"
```

[hackage]: https://hackage.haskell.org/package/do-list
[hackage-badge]: https://img.shields.io/hackage/v/do-list.svg
[DoList]: https://hackage.haskell.org/package/do-list/docs/Data-DoList.html
