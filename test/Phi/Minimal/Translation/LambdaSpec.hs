module Phi.Minimal.Translation.LambdaSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Phi.Minimal.Model
import           Phi.Minimal.Model.Arbitrary    ()
import           Phi.Minimal.Pretty             ()
import           Phi.Minimal.Translation.Lambda

soundness :: Term -> Bool
soundness term = or
  [ not (null (drop 10 (whnfSteps term)))
  , phiToLambdaIsSound term ]

spec :: Spec
spec =
  describe "translation from phi-calculus to lambda calculus" $ do
    it "is sound" $ withMaxSuccess (10^6) $ property $ \term ->
      soundness term
