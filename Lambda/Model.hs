{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE UndecidableInstances       #-}
module Lambda.Model where

import           Control.Applicative        (empty, (<|>))
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import           Data.List                  (sort, transpose, unfoldr)

type Attr = String

data Term
  = Var Int
  | Abs Term
  | App Term Term
  | Dot Term Attr
  | Obj (InsOrdHashMap Attr Term)
  | With Term (InsOrdHashMap Attr Term)
  | Cat Term Term
  | Fix Term
  deriving (Eq)

incFrom :: Int -> Term -> Term
incFrom i = \case
  Var j
    | j >= i    -> Var (j + 1)
    | otherwise -> Var j
  Abs body    -> Abs (incFrom (i + 1) body)
  App fun arg -> App (incFrom i fun) (incFrom i arg)
  Dot t a     -> Dot (incFrom i t) a
  Obj o       -> Obj (incFrom i <$> o)
  With t o    -> With (incFrom i t) o
  Cat l r     -> Cat (incFrom i l) (incFrom i r)
  Fix t       -> Fix (incFrom i t)

inc :: Term -> Term
inc = incFrom 0

substitute :: (Int, Term) -> Term -> Term
substitute (i, u) = \case
  Var j
    | j < i     -> Var j
    | j == i    -> u
    | otherwise -> Var (j - 1)
  Abs body    -> Abs (substitute (i + 1, inc u) body)
  App fun arg -> App (substitute (i, u) fun) (substitute (i, u) arg)
  Dot t a     -> Dot (substitute (i, u) t) a
  Obj o       -> Obj (substitute (i, u) <$> o)
  With t o    -> With (substitute (i, u) t) (substitute (i, u) <$> o)
  Cat l r     -> Cat (substitute (i, u) l) (substitute (i, u) r)
  Fix t       -> Fix (substitute (i, u) t)

whnfStep :: Term -> Maybe Term
whnfStep = \case
  App fun arg ->
    case fun of
      Abs body -> Just (substitute (0, arg) body)
      _        -> App <$> whnfStep fun <*> pure arg
  Dot t a ->
    case t of
      With t' o ->
        case InsOrdHashMap.lookup a o of
          Nothing -> Just (Dot t' a)
          Just x  -> Just x
      Obj o ->
        case InsOrdHashMap.lookup a o of
          Nothing -> Just (Dot (Obj o) a)
          Just x  -> Just x
      _ -> Dot <$> whnfStep t <*> pure a
  Cat l r ->
    case (l, r) of
      (Obj [], _)       -> Just r
      (Obj ol, Obj or') -> Just (Obj (InsOrdHashMap.union or' ol))
      (Obj _, _)        -> Cat l <$> whnfStep r
      _                 -> Cat <$> whnfStep l <*> pure r
  Fix t -> Just (App t (Fix t))
  Abs (App fun arg) ->
    case arg of
      Var 0 -> Just fun
      _     -> Abs . App fun <$> whnfStep arg
  _ -> Nothing

whnfSteps :: Term -> [Term]
whnfSteps t = t : unfoldr (fmap dup . whnfStep) t
  where
    dup x = (x, x)

whnf :: Term -> Term
whnf = \case
  App fun arg ->
    case whnf fun of
      Abs body -> whnf (substitute (0, arg) body)
      fun'     -> App fun' arg
  Dot t a ->
    case whnf t of
      With t' o ->
        case InsOrdHashMap.lookup a o of
          Nothing -> whnf (Dot t' a)
          Just x  -> whnf x
      Obj o ->
        case InsOrdHashMap.lookup a o of
          Nothing -> Dot (Obj o) a
          Just x  -> whnf x
      t' -> Dot t' a
  Cat l r ->
    case (whnf l, whnf r) of
      (Obj ol, Obj or') -> Obj (InsOrdHashMap.union or' ol)
      (l', r')          -> Cat l' r'
  Fix t -> whnf (App t (Fix t))
  t -> t

nf :: Term -> Term
nf = \case
  App fun arg ->
    case whnf fun of
      Abs body -> nf (substitute (0, arg) body)
      fun'     -> App (nf fun') (nf arg)
  Dot t a ->
    case whnf t of
      With t' o ->
        case InsOrdHashMap.lookup a o of
          Nothing -> nf (Dot t' a)
          Just x  -> nf x
      Obj o ->
        case InsOrdHashMap.lookup a o of
          Nothing -> Dot (nf (Obj o)) a
          Just x  -> nf x
      t' -> Dot (nf t') a
  Cat l r ->
    case (whnf l, whnf r) of
      (Obj ol, Obj or') -> nf (Obj (InsOrdHashMap.union or' ol))
      (l', r')          -> Cat (nf l') (nf r')
  Fix t -> nf (App t (Fix t))
  Abs body -> Abs (nf body)
  With t o -> With (nf t) (nf <$> o)
  Obj o -> Obj (nf <$> o)
  t@Var{} -> t

matchEq :: Eq a => a -> a -> Either (Term, Term) a
matchEq x y
  | x == y = Right x
  | otherwise = Left (error "akljfhg")

matchObjs
  :: [(Term, Term)]
  -> InsOrdHashMap Attr Term
  -> InsOrdHashMap Attr Term
  -> Either (Term, Term) (InsOrdHashMap Attr Term)
matchObjs eqs o1 o2
  | sort (InsOrdHashMap.keys o1) == sort (InsOrdHashMap.keys o2) =
      sequence (InsOrdHashMap.unionWith matchTerms'' (Right <$> o1) (Right <$> o2))
  | otherwise = Left (Obj o1, Obj o2)
  where
    matchTerms'' mx my = mx >>= \x -> my >>= \y -> matchTerms' eqs x y

orEither :: Either e a -> Either e a -> Either e a
orEither (Right x) _         = Right x
orEither (Left _) (Left err) = Left err
orEither (Left _) y          = y

interleave :: [a] -> [a] -> [a]
interleave xs []         = xs
interleave [] ys         = ys
interleave (x:xs) (y:ys) = x:y:interleave xs ys

interleaveMany :: [[a]] -> [a]
interleaveMany = concat . transpose

matchTerms :: Term -> Term -> Either (Term, Term) Term
matchTerms = matchTerms' []

matchTerms' :: [(Term, Term)] -> Term -> Term -> Either (Term, Term) Term
matchTerms' eqs tl tr = foldr1 (orEither) $ interleaveMany
  [ matchTermsNaive eqs tl' <$> (take 10 $ whnfSteps tr) | tl' <- take 10 $ whnfSteps tl ]

matchTermsNaive :: [(Term, Term)] -> Term -> Term -> Either (Term, Term) Term
matchTermsNaive eqs tl tr
  | (tl, tr) `elem` eqs = Right tl
  | otherwise =
      case (tl, tr) of
        (Var i, Var j) ->
          Var <$> matchEq i j

        (Abs body1, Abs body2) ->
          Abs <$> go body1 body2

        (App fun1 arg1, App fun2 arg2) ->
          App <$> go fun1 fun2 <*> go arg1 arg2

        (Dot t1 a1, Dot t2 a2) ->
          Dot <$> go t1 t2 <*> matchEq a1 a2

        (Obj o1, Obj o2) -> Obj <$> matchObjs eqs o1 o2

        (With t1 o1, With t2 o2) ->
          With <$> go t1 t2 <*> matchObjs eqs o1 o2

        (Cat l1 r1, Cat l2 r2) ->
          Cat <$> go l1 l2 <*> go r1 r2

        (Fix t1, Fix t2) ->
          Fix <$> go t1 t2

        (_, _) -> Left (tl, tr)
  where
    go = matchTerms' ((tl, tr):eqs)
