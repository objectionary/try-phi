{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wall #-}

module Phi.Minimal.Model
  ( 
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
  -- TO-DO store a list of free attributes
  -- and a map of attached attributes
  = Obj {body :: Object Term}
  | Dot {t::Term, a::Attr}
  | App {t::Term, at::(Attr, Term)}
  | Loc {i::Int}
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