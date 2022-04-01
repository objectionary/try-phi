{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import ParseEO as A (tProgram)
import PrettyPrintTree as PP (printTree)
import Text.Megaparsec (parseMaybe)
import Data.Text as DT (pack)
import EnumerateNodes (enumInsertProgram, getProgram)
import ToTerm (getTermProgram)
import PrettyPrintTerm(pprintTop)

main :: IO ()
main = do
  let file = "./grammars/full-syntax.eo"
  code <- pack <$> readFile file
  let t = parseMaybe tProgram code
  let t1 = getProgram <$> t
  let t2 = getTermProgram <$> t1
  putStr "\n\n"
  putStrLn (maybe "not ok tree" printTree t1)
  putStrLn (maybe "not ok term" show t2)
  putStrLn (maybe "not ok EO" pprintTop t2)