{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}
module Phi.Minimal.Translation.Lambda where

import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import           Lambda.Model               as Lambda
import qualified Phi.Minimal.Model          as Phi

phiToLambda :: Phi.Term -> Term
phiToLambda = \case
  Phi.Loc n ->
    Abs (App (Var (2*n + 2)) (Var (2 * n + 1) `Cat` Var 0))
  Phi.Dot t a ->
    Dot (App (phiToLambda t) (Obj [])) a
  Phi.App t (a, u) ->
    Abs (App (inc (phiToLambda t)) (Var 0 `With` [(a, inc (phiToLambda u))]))
  Phi.Obj o ->
    let (void, attached) = Phi.splitAttrs o
        void' = [ (a, Dot (Var 0) a) | a <- void ]
        attached' = [ (b, phiToLambda t) | (b, t) <- attached]
     in case "𝜑" `elem` (void <> map fst attached) of
          False ->
            Fix (Abs (Abs (
              Obj (InsOrdHashMap.fromList (void' <> attached')))))
          True ->
            Fix (Abs (Abs (
              App (Dot (App (Var 1) (Var 0)) "𝜑") (Obj []) `With`
              InsOrdHashMap.fromList (void' <> attached'))))

phiToLambdaIsSound :: Phi.Term -> Bool
phiToLambdaIsSound t =
  case matchTerms (phiToLambda t) (phiToLambda (Phi.whnf t)) of
    Left _  -> False
    Right _ -> True

