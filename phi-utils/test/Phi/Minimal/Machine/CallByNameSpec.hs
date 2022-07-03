module Phi.Minimal.Machine.CallByNameSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Phi.Minimal
import           Phi.Minimal.Machine.CallByName
import           Phi.Minimal.Model.Arbitrary    ()

soundness :: Term -> Bool
soundness term = or
  [ not (null (drop 1000 (whnfSteps term)))
  , whnf term == fromConfiguration (stepThrough (initConfiguration term)) ]

spec :: Spec
spec =
  describe "call-by-name abstract machine" $ do
    it "is sound" $ withMaxSuccess (10^6) $ property $ \term ->
      soundness term
