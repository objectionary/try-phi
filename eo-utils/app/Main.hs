{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import EOParser ( pack, parseCSTProgram, pprintCST, parseTermProgram, pprintTermProgram )
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  let file = "./grammars/snippet.eo"
  -- let file = "./grammars/full-syntax.eo"
  code <- pack . (<> "\n") <$> readFile file
  let t' = parseTermProgram code
  let t = parseCSTProgram code
  putStr "\n\n"
  putStrLn $ either errorBundlePretty pprintCST t
  putStrLn $ either errorBundlePretty pprintTermProgram t'
