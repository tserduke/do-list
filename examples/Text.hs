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
