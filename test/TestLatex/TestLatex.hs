module TestLatex.TestLatex where

import Phi.Minimal.PPToLatex
import Phi.Minimal.Examples

testLatex :: IO ()
testLatex = do
    let ts = [ex4, ex6, ex7]
    mapM_ (\x -> print x >> putStrLn "") ts 