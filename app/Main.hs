{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import EOParser ( pack, parseTermProgram, pprintTermProgram )

main :: IO ()
main = do
  let file = "./grammars/full-syntax.eo"
  code <- pack . (<> "\n") <$> readFile file
  let t = parseTermProgram code
  putStr "\n\n"
  putStrLn (maybe "not ok EO" pprintTermProgram t)