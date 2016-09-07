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
