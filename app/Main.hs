{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import ParseEO as P (pProgram)
import ParseEOAlt as A (tProgram)
import PrettyPrint as PP
import Text.Megaparsec (parseTest, parseMaybe)
import SimplifyTree(simplifyCST, enumerateNodes)
import Data.Text as DT (pack, replicate, Text(..))
import Text.Printf (printf)

main :: IO ()
main = do
  let file = "./grammars/full-syntax.eo"
  -- let file = "./grammars/code.eo"
  code <- pack <$> readFile file
  let tr1 = parseMaybe tProgram code
  putStr "\n\n"
  case tr1 of
    -- Just t -> print "Tree"
    Just t -> putStr $ show t
    _ -> print "Not ok"
  -- let tr = parseMaybe pProgram code
  -- -- let l = printf "\n%s\n%s\n%s\n" ((DT.replicate 10 "*")::Text) ("\nRESULT\n") ((DT.replicate 10 "*")::Text)
  -- let l = putStrLn "*****************" *> putStrLn "RESULT" *> putStrLn "*********************\n"
  -- case tr of
  --   Just a -> print (enumerateNodes a) *> l *> print (simplifyCST a)
  --   Nothing -> print "No tree"