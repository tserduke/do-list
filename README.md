# do-list
DoList makes it easy to use do notation syntatic sugar for defining list structures.

## Example
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
