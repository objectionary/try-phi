{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where
  
import EOParser


main :: IO ()
main = do
  let file = "./grammars/full-syntax.eo"
  code <- pack . (<> "\n") <$> readFile file
  let t = parseTermProgram code
  putStr "\n\n"
  putStrLn (maybe "not ok EO" pprintTermProgram t)