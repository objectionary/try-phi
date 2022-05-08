module EO.Test where

import EO.EOtoPhi(toMinimalTerm)
import Phi.Minimal.Pretty
import EOParser
import System.Directory(getCurrentDirectory)

test :: IO ()
test = do
  pwd <- getCurrentDirectory
  putStrLn pwd
  let file = "eo-parser/grammars/full-syntax.eo"
  code <- pack . (<> "\n") <$> readFile file
  let p = parseTermProgram code
  case p of
    Just p' -> putStrLn (pprintTermProgram p') >> print (toMinimalTerm p')
    _ -> putStrLn "not Ok"

{-
>>>test
-}
