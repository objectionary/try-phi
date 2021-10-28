{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
module Phi.Minimal.Model where

import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import           Data.List                  (foldl')
import           GHC.Exts                   (IsList (..))

type Attr = String

newtype Object a = Object { getObject :: InsOrdHashMap Attr (AttrValue a) }
  deriving (Eq, Functor, Foldable, Traversable, IsList)

(.?) :: Object a -> Attr -> Maybe (AttrValue a)
Object o .? a = InsOrdHashMap.lookup a o

(.=) :: Object a -> (Attr, a) -> Object a
Object o .= (a, v) = Object (InsOrdHashMap.insert a (Attached v) o)

data AttrValue a
  = VoidAttr
  | Attached a
  deriving (Eq, Functor, Foldable, Traversable)

splitAttrs :: Object a -> ([Attr], [(Attr, a)])
splitAttrs = foldr add ([], []) . toList
  where
    add (a, VoidAttr) (void, attached)   = (a : void, attached)
    add (a, Attached t) (void, attached) = (void, (a, t) : attached)

data Term
  = Obj (Object Term)
  | Dot Term Attr
  | App Term (Attr, Term)
  | Loc Int
  deriving (Eq)

appList :: Term -> Maybe (Term, [(Attr, Term)])
appList = \case
  App t1 t2 -> Just (go t1 [t2])
  _ -> Nothing
  where
    go (App t x) xs = go t (x:xs)
    go t xs         = (t, xs)

peelApps :: Term -> (Term, [(Attr, Term)])
peelApps = go []
  where
    go xs (App t x) = go (x:xs) t
    go xs t         = (t, xs)

pattern Apps :: Term -> [(Attr, Term)] -> Term
pattern Apps f xs <- (appList -> Just (f, xs))
  where
    Apps f xs = foldl' App f xs

{-# COMPLETE Obj, Dot, Apps, Loc #-}

incLocatorsFrom :: Int -> Term -> Term
incLocatorsFrom k = \case
  Obj o -> Obj (incLocatorsFrom (k + 1) <$> o)
  Dot t a -> Dot (incLocatorsFrom k t) a
  App t (a, u) -> App (incLocatorsFrom k t) (a, incLocatorsFrom k u)
  Loc n
    | n >= k    -> Loc (n + 1)
    | otherwise -> Loc n

incLocators :: Term -> Term
incLocators = incLocatorsFrom 0

substituteLocator :: (Int, Term) -> Term -> Term
substituteLocator (k, v) = \case
  Obj o -> Obj (substituteLocator (k + 1, incLocators v) <$> o)
  Dot t a -> Dot (substituteLocator (k, v) t) a
  App t (a, u) -> App (substituteLocator (k, v) t) (a, substituteLocator (k, v) u)
  Loc n
    | n <  k    -> Loc n
    | n == k    -> v
    | otherwise -> Loc (n - 1)

-- | Compute a term to its weak head normal form (does not compute inside of objects).
whnf :: Term -> Term
whnf = \case
  Dot t a ->
    case whnf t of
      t'@(Obj o) ->
        case o .? a of
          Just VoidAttr     -> Dot t' a
          Just (Attached u) -> whnf (substituteLocator (0, t') u)
          Nothing           ->
            case o .? "𝜑" of
              Just _  -> whnf (Dot (Dot t' "𝜑") a)
              Nothing -> Dot t' a

      t' -> Dot t' a

  App t (a, u) ->
    case whnf t of
      t'@(Obj o) ->
        case o .? a of
          Just VoidAttr     -> Obj (o .= (a, incLocators u))
          Just (Attached _) -> App t' (a, u)
          Nothing           -> App t' (a, u)

      t' -> App t' (a, u)

  t@Obj{} -> t
  t@Loc{} -> t

-- | Compute a term to its normal form.
nf :: Term -> Term
nf = \case
  Dot t a ->
    case whnf t of
      t'@(Obj o) ->
        case o .? a of
          Just VoidAttr     -> Dot (nf t') a
          Just (Attached u) -> nf (substituteLocator (0, t') u)
          Nothing           ->
            case o .? "𝜑" of
              Just _  -> nf (Dot (Dot t' "𝜑") a)
              Nothing -> Dot (nf t') a

      t' -> Dot (nf t') a

  App t (a, u) ->
    case whnf t of
      t'@(Obj o) ->
        case o .? a of
          Just VoidAttr     -> nf (Obj (o .= (a, incLocators u)))
          Just (Attached _) -> App (nf t') (a, nf u)
          Nothing           -> App (nf t') (a, nf u)

      t' -> App (nf t') (a, nf u)

  Obj o -> Obj (nf <$> o)

  t@Loc{} -> t

-- * Examples

-- | Empty object.
--
-- >>> empty
-- ⟦⟧
empty :: Term
empty = Obj []

-- |
-- >>> ex1
-- ⟦ x ↦ ρ⁰ ⟧
ex1 :: Term
ex1 = Obj [ ("x", Attached (Loc 0)) ]

-- |
-- >>> ex2
-- ⟦ x ↦ ⟦ y ↦ ρ¹ ⟧ ⟧.x.y
ex2 :: Term
ex2 = Dot (Dot (Obj [ ("x", Attached (Obj [("y", Attached (Loc 1))])) ]) "x") "y"

-- |
-- >>> ex3
-- ⟦ x ↦ ⟦ x ↦ ρ⁰ ⟧, y ↦ ⟦ x ↦ ⟦ y ↦ ρ¹ ⟧ ⟧.x.y ⟧
ex3 :: Term
ex3 = Obj [ ("x", Attached ex1), ("y", Attached ex2) ]

-- |
-- >>> ex4
-- ⟦ x ↦ ⟦ x ↦ ⟦ x ↦ ρ⁰ ⟧, y ↦ ⟦ x ↦ ⟦ y ↦ ρ¹ ⟧ ⟧.x.y ⟧
--   , y ↦ ⟦ x ↦ ⟦ y ↦ ρ¹ ⟧ ⟧.x.y
--   , z ↦ ⟦ x ↦ ⟦ y ↦ ρ¹ ⟧ ⟧.x.y ⟧
ex4 :: Term
ex4 = Obj [ ("x", Attached ex3), ("y", Attached ex2), ("z", Attached ex1) ]

ex5 :: Term
ex5 = Obj [ ("x", Attached ex4), ("y", Attached ex3), ("z", Attached ex2) ]

ex6 :: Term
ex6 = Dot (Dot (App t ("z", u)) "x") "y"
  where
    t = Obj [ ("x", Attached (Obj [("y", Attached (Dot (Loc 1) "z"))]))
            , ("z", VoidAttr) ]
    u = Obj []

-- * Terms translated from \(\lambda\)-calculus to \(\varphi\)-calculus

-- | Apply a term that represents \(\lambda\)-term to another term.
--
-- >>> app (Loc 0) (Loc 1)
-- ρ⁰(𝜑 ↦ ρ¹).r
app :: Term -> Term -> Term
app f x = Dot (App f ("𝜑", x)) "r"

-- | Wrap body of a \(\lambda\)-term into a translated \(\lambda\)-abstraction.
--
-- >>> identity = lam (Dot (Loc 0) "𝜑")
-- >>> identity
-- ⟦ 𝜑 ↦ ø, r ↦ ρ⁰.𝜑 ⟧
-- >>> app identity identity
-- ⟦ 𝜑 ↦ ø, r ↦ ρ⁰.𝜑 ⟧(𝜑 ↦ ⟦ 𝜑 ↦ ø, r ↦ ρ⁰.𝜑 ⟧).r
-- >>> nf (app identity identity )
-- ⟦ 𝜑 ↦ ø, r ↦ ρ⁰.𝜑 ⟧
lam :: Term -> Term
lam t = Obj [ ("𝜑", VoidAttr), ("r", Attached t) ]

-- | Church numerals translated to \(\varphi\)-calculus.
--
-- >>> church_n 0
-- ⟦ 𝜑 ↦ ø, r ↦ ⟦ 𝜑 ↦ ø, r ↦ ρ⁰.𝜑 ⟧ ⟧
-- >>> church_n 3
-- ⟦ 𝜑 ↦ ø, r ↦ ⟦ 𝜑 ↦ ø, r ↦ ρ¹.𝜑(𝜑 ↦ ρ¹.𝜑(𝜑 ↦ ρ¹.𝜑(𝜑 ↦ ρ⁰.𝜑).r).r).r ⟧ ⟧
church_n :: Int -> Term
church_n n = lam (lam (iterate (app s) z !! n))
  where
    s = Dot (Loc 1) "𝜑"
    z = Dot (Loc 0) "𝜑"

-- | Increment combinator for Church numerals.
--
-- >>> church_n 3 == nf (app church_inc (church_n 2))
-- True
church_inc :: Term
church_inc = lam (lam (lam (app s (app (app n s) z))))
  where
    n = Dot (Loc 2) "𝜑"
    s = Dot (Loc 1) "𝜑"
    z = Dot (Loc 0) "𝜑"

-- | Combinator for adding two Church numerals.
--
-- \(5 = 2 + 3\):
-- >>> church_n 5 == nf (app (app church_plus (church_n 2)) (church_n 3))
-- True
church_plus :: Term
church_plus = lam (lam (lam (lam (app (app n s) (app (app m s) z)))))
  where
    n = Dot (Loc 3) "𝜑"
    m = Dot (Loc 2) "𝜑"
    s = Dot (Loc 1) "𝜑"
    z = Dot (Loc 0) "𝜑"

-- | Combinator for multiplying two Church numerals.
--
-- \(6 = 2 \times 3\):
-- >>> church_n 6 == nf (app (app church_mul (church_n 2)) (church_n 3))
-- True
church_mul :: Term
church_mul = lam (lam (lam (app n (app m s))))
  where
    n = Dot (Loc 2) "𝜑"
    m = Dot (Loc 1) "𝜑"
    s = Dot (Loc 0) "𝜑"

-- | Church-encoded exponentiation combinator.
--
-- \(8 = 2^3\):
-- >>> church_n 8 == nf (app (app church_exp (church_n 2)) (church_n 3))
-- True
church_exp :: Term
church_exp = lam (lam (app m n ))
  where
    n = Dot (Loc 1) "𝜑"
    m = Dot (Loc 0) "𝜑"

