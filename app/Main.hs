{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import ParseEO as P (pProgram)
import Text.Megaparsec (parseTest)
import Data.Text (pack)

main :: IO ()
main = do
  let file = "./grammars/full-syntax.eo"
  code <- pack <$> readFile file
  print "\n"
  parseTest pProgram code