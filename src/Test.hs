module Test(test, test') where

import EOtoPhi(toMinimalTerm)
import Phi.Minimal.Pretty ()
import EOParser ( pprintTermProgram, parseTermProgram, pack, pprintCST, parseCSTProgram )
import System.Directory(getCurrentDirectory)
import Data.Function ((&))
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple

test' :: IO ()
test' = print "not ok"

test :: String -> IO ()
test s = do
  pwd <- getCurrentDirectory
  putStrLn pwd
  let file = s
  code <- pack . (<> "\n") <$> readFile file
  let p = parseTermProgram code
  -- either (print . errorBundlePretty) pPrint p
      -- t = parseCSTProgram code
  p & either (print . errorBundlePretty) (\p' ->
    do
      -- putStrLn "\nEO tree\n"
      pPrint p'
      putStrLn "\nEO code\n"
      putStrLn (pprintTermProgram p')
      putStrLn "\nPhi Minimal code\n"
      print (toMinimalTerm p')
      putStrLn "\n ")
