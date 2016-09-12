{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import Data.DoList (DoList, fromList, toList)
import Data.Text as T (Text, append, unlines)
import Data.Text.IO as T (putStr)

main :: IO ()
main = T.putStr $ T.unlines $ toList $ do
  "Here goes indented list:"
  indent $ do
    "1. Item 1"
    indent $ do
      "* Item a"
      "* Item b"
    "2. Item 2"
    "3. Item 3"

indent :: DoList Text -> DoList Text
-- fromList and toList are no-ops
indent = fromList . map (append "  ") . toList
