{-# LANGUAGE FlexibleContexts #-}
module TestLatex.TestLatex where

import qualified Phi.Minimal.PPToLatex as PL(Latex(..))
import Phi.Minimal.Examples
import qualified Phi.Minimal.Pretty as PP

testLatex :: IO ()
testLatex = do
    let ts = [ex0, ex4, ex6, ex7]
    let printEx t = putStrLn "" >> print (PL.Latex t) >> putStrLn "" >> print t
    mapM_ printEx ts