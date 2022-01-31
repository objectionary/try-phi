{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wall #-}

module Phi.Minimal.Model
  ( ex19,
    Object (..),
    Attr,
    Term (..),
    incLocators,
    AttrValue (..),
    substituteLocator,
    (.?),
    splitAttrs,
    whnfSteps,
    whnf,
    nf,
    pattern Apps,
    DataValue(..)
  )
where

import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import           Data.List                  (foldl', unfoldr)
import           GHC.Exts                   (IsList (..))

type Attr = String

newtype Object a = Object
  { getObject :: InsOrdHashMap Attr (AttrValue a)
  }
  deriving (Eq, Functor, Foldable, Traversable, IsList)

-- showObject :: Show a => Object a -> String
-- showObject o = show $ getobject o

-- instance Show a => Object a where
--   show o = showObject o

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


data DataValue =
  DataInteger Integer
  | NoData
  deriving (Eq, Show, Ord)


data Term
  = Obj (Object Term)
  | Dot Term Attr
  | App Term (Attr, Term)
  | Loc Int
  | DataTerm DataValue
  deriving (Eq)

appList :: Term -> Maybe (Term, [(Attr, Term)])
appList =
  \case
    App t1 t2 -> Just (go t1 [t2])
    _ -> Nothing
  where
    go (App t x) xs = go t (x : xs)
    go t xs         = (t, xs)

peelApps :: Term -> (Term, [(Attr, Term)])
peelApps = go []
  where
    go xs (App t x) = go (x : xs) t
    go xs t         = (t, xs)

pattern Apps :: Term -> [(Attr, Term)] -> Term
pattern Apps f xs <-
  (appList -> Just (f, xs))
  where
    Apps f xs = foldl' App f xs

{-# COMPLETE Obj, Dot, Apps, Loc #-}

incLocatorsFrom :: Int -> Term -> Term
incLocatorsFrom k =
  \case
    Obj o -> Obj (incLocatorsFrom (k + 1) <$> o)
    Dot t a -> Dot (incLocatorsFrom k t) a
    App t (a, u) -> App (incLocatorsFrom k t) (a, incLocatorsFrom k u)
    Loc n
      | n >= k -> Loc (n + 1)
      | otherwise -> Loc n
    dataTerm -> dataTerm

incLocators :: Term -> Term
incLocators = incLocatorsFrom 0

substituteLocator :: (Int, Term) -> Term -> Term
substituteLocator (k, v) =
  \case
    Obj o -> Obj (substituteLocator (k + 1, incLocators v) <$> o)
    Dot t a -> Dot (substituteLocator (k, v) t) a
    App t (a, u) ->
      App (substituteLocator (k, v) t) (a, substituteLocator (k, v) u)
    Loc n
      | n < k -> Loc n
      | n == k -> v
      | otherwise -> Loc (n - 1)
    dataTerm -> dataTerm

whnfSteps :: Term -> [Term]
whnfSteps term = term : unfoldr (fmap dup . whnfStep) term
  where
    dup x = (x, x)

whnfStep :: Term -> Maybe Term
whnfStep =
  \case
    Dot t a ->
      case t of
        Obj o ->
          case o .? a of
            Just VoidAttr -> Nothing
            Just (Attached u) -> Just (substituteLocator (0, t) u)
            Nothing ->
              case o .? "𝜑" of
                Just _  -> Just (Dot (Dot t "𝜑") a)
                Nothing -> Nothing
        _ -> (`Dot` a) <$> whnfStep t
    App t (a, u) ->
      case t of
        Obj o ->
          case o .? a of
            Just VoidAttr     -> Just (Obj (o .= (a, incLocators u)))
            Just (Attached _) -> Nothing
            Nothing           -> Nothing
        _ -> (`App` (a, u)) <$> whnfStep t
    Obj {} -> Nothing
    Loc {} -> Nothing
    _dataTerm -> Nothing

-- | Compute a term to its weak head normal form (does not compute inside of objects).
whnf :: Term -> Term
whnf =
  \case
    Dot t a ->
      case whnf t of
        t'@(Obj o) ->
          case o .? a of
            Just VoidAttr -> Dot t' a
            Just (Attached u) -> whnf (substituteLocator (0, t') u)
            Nothing ->
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
    t@Obj {} -> t
    t@Loc {} -> t
    dataTerm -> dataTerm

-- | Compute a term to its normal form.
nf :: Term -> Term
nf =
  \case
    Dot t a ->
      case whnf t of
        t'@(Obj o) ->
          case o .? a of
            Just VoidAttr -> Dot (nf t') a
            Just (Attached u) -> nf (substituteLocator (0, t') u)
            Nothing ->
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
    t@Loc {} -> t
    dataTerm -> dataTerm

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
ex1 = Obj [("x", Attached (Loc 0))]

-- |
-- >>> ex2
-- ⟦ x ↦ ⟦ y ↦ ρ¹ ⟧ ⟧.x.y
ex2 :: Term
ex2 = Dot (Dot (Obj [("x", Attached (Obj [("y", Attached (Loc 1))]))]) "x") "y"

-- |
-- >>> ex3
-- ⟦ x ↦ ⟦ x ↦ ρ⁰ ⟧, y ↦ ⟦ x ↦ ⟦ y ↦ ρ¹ ⟧ ⟧.x.y ⟧
ex3 :: Term
ex3 = Obj [("x", Attached ex1), ("y", Attached ex2)]

-- |
-- >>> ex4
-- ⟦ x ↦ ⟦ x ↦ ⟦ x ↦ ρ⁰ ⟧, y ↦ ⟦ x ↦ ⟦ y ↦ ρ¹ ⟧ ⟧.x.y ⟧
--   , y ↦ ⟦ x ↦ ⟦ y ↦ ρ¹ ⟧ ⟧.x.y
--   , z ↦ ⟦ x ↦ ⟦ y ↦ ρ¹ ⟧ ⟧.x.y ⟧
ex4 :: Term
ex4 = Obj [("x", Attached ex3), ("y", Attached ex2), ("z", Attached ex1)]

ex5 :: Term
ex5 = Obj [("x", Attached ex4), ("y", Attached ex3), ("z", Attached ex2)]

ex6 :: Term
ex6 = Dot (Dot (App t ("z", u)) "x") "y"
  where
    t =
      Obj
        [ ("x", Attached (Obj [("y", Attached (Dot (Loc 1) "z"))])),
          ("z", VoidAttr)
        ]
    u = Obj []

ex7 :: Term
ex7 = App t ("y", u)
  where
    t = Obj [("x", VoidAttr), ("y", VoidAttr), ("z", Attached (Loc 0))]
    u = App (Dot (Dot (Loc 1) "x") "a") ("y", (Dot (Loc 1) "a"))

ex8 :: Term
ex8 =
  Dot (Obj [("x", Attached $ App (Dot (Loc 3) "y") ("y", Dot (Loc 3) "z"))]) "x"

ex9 :: Term
ex9 = App (Dot t "d") ("b", Loc 0)
  where
    t =
      Obj
        [ ("a", VoidAttr),
          ("b", VoidAttr),
          ("c", Attached (Loc 2)),
          ("d", Attached (Obj [])),
          ("e", Attached (Loc 3))
        ]

ex10 :: Term
ex10 = Dot t "z"
  where
    t =
      Obj
        [ ("x", VoidAttr),
          ("y", VoidAttr),
          ("z", Attached (App (Loc 0) ("y", Loc 0)))
        ]

ex11 :: Term
ex11 = Dot (Obj [("z", Attached (Obj [("z", Attached (Loc 2))]))]) "z"

ex12 :: Term
ex12 =
  App
    (Dot (Obj [("x", Attached (Obj [])), ("z", Attached (Loc 0))]) "x")
    ("y", Loc 1)

ex13 :: Term
ex13 = App (Dot (Obj [("x", Attached (Obj []))]) "x") ("y", Loc 0)

ex14 :: Term
ex14 = Dot (Obj [("x", Attached (App t ("y", Loc 0)))]) "x"
  where
    t = Obj [("y", VoidAttr), ("x", Attached (Loc 2))]

ex15 :: Term
ex15 = Dot (Obj [("a", Attached (Dot ex14 "x"))]) "a"

ex16 :: Term
ex16 = Dot (Dot t "a") "y"
  where
    t =
      Obj
        [ ( "a",
            Attached
              ( Obj
                  [ ("z", Attached (Loc 1)),
                    ("y", Attached (Obj [("x", Attached (Loc 1))]))
                  ]
              )
          )
        ]

ex17 :: Term
ex17 = Dot (Dot (Obj [("x", Attached $ App t ("a", Loc 1))]) "x") "y"
  where
    t = Obj [("a", VoidAttr), ("y", Attached $ Obj [("z", Attached (Loc 1))])]

-- ⟦ v3 ↦ ø, a3 ↦ ρ⁰(v3 ↦ ρ¹.v2)(v3 ↦ ρ¹) ⟧.a3
ex18 :: Term
ex18 = Dot (Obj [("x", VoidAttr), ("y", Attached t)]) "y"
  where
    t = App (Loc 0) ("x", Loc 1)

ex19 :: Term
ex19 = Dot (Obj [("x", VoidAttr), ("y", Attached t)]) "y"
  where
    t = App (Loc 0) ("x", DataTerm (DataInteger 1))

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
lam t = Obj [("𝜑", VoidAttr), ("r", Attached t)]

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
church_exp = lam (lam (app m n))
  where
    n = Dot (Loc 1) "𝜑"
    m = Dot (Loc 0) "𝜑"
