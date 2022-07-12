module Test(test, test') where

import EOtoPhi(toMinimalTerm)
import Phi.Minimal.Pretty ()
import EOParser ( pprintTermProgram, parseTermProgram, pack )
import System.Directory(getCurrentDirectory)
import Data.Function ((&))

test' :: IO ()
test' = print "not ok"

test :: String -> IO ()
test s = do
  pwd <- getCurrentDirectory
  putStrLn pwd
  let file = s
  code <- pack . (<> "\n") <$> readFile file
  let p = parseTermProgram code
  p & either print (\p' ->
    do
      putStrLn "\nEO code\n"
      putStrLn (pprintTermProgram p')
      putStrLn "\nPhi Minimal code\n"
      print (toMinimalTerm p')
      putStrLn "\n ")
