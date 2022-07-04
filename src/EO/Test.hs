module EO.Test(test, test') where

import EO.EOtoPhi(toMinimalTerm)
import Phi.Minimal.Pretty
import EOParser
import System.Directory(getCurrentDirectory)

test' :: IO ()
test' = print "not ok"

test :: String -> IO ()
test s = do
  pwd <- getCurrentDirectory
  putStrLn pwd
  let file = s
  code <- pack . (<> "\n") <$> readFile file
  let p = parseTermProgram code
  case p of
    Just p' -> do 
      putStrLn "\nEO code\n"
      putStrLn (pprintTermProgram p')
      putStrLn "\nPhi Minimal code\n"
      print (toMinimalTerm p')
      putStrLn "\n "
    _ -> putStrLn "not Ok"
