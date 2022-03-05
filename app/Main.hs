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
import ParseIndents (pItemList, eof)

main :: IO ()
main = do
  let file = "./grammars/list.eo"
  code <- pack <$> readFile file
  putStrLn "\n"
  parseTest (pItemList <* eof) code

  -- let file = "./grammars/code.eo"
  -- code <- pack <$> readFile file
  -- putStrLn "\n"
  -- parseTest pProgram code