{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Phi.Minimal.EO.Pretty where

import  Prettyprinter as Pretty
import Data.List(intercalate)

import Phi.Minimal.Model
    ( Term(..),
      DataValue(..),
      AttrValue(..),
      Object (..),
      Attr,
      splitAttrs )

-- inline phi
-- ^2.a.b(c->^0.d)

-- inline eo
-- ^2.a.b ^0.d:c

-- multiline
-- b.
--   a.
--     ^.
--       ^
--   d.:c
--     $


-- inline phi
-- t -> [a->^0.b, c->?]

-- inline eo
-- [c] ($.b > a) > t

-- multiline
-- [c] > t
--   b. > a
--     $

-- inline phi
-- t(a -> 4.b)

-- inline eo
-- t 4.b:a

-- multiline
-- t
--   b.:a
--     4

-- inline phi
-- [a->^0]

-- inline eo
-- [] ($ > a)

-- multiline
-- []
--   $ > a
 
type Separator = String

sepAttach :: Separator
sepAttach = " > "
sepArgument :: Separator
sepArgument = ":"

ppTerm :: Term -> Doc ann
ppTerm =
  \case
    Obj o -> ppObjAttr Nothing o sepAttach 
    Dot t a -> ppDotAttr Nothing  (t, a) sepAttach
    App t (a, u) -> ppAppAttr Nothing  (t, (Just a, u)) sepAttach
    Loc n -> ppLoc n
    DataTerm t -> 
      case t of
        DataInteger i -> ppInt Nothing i

ppTermAttr :: Maybe Attr -> Term -> Separator -> Doc ann
ppTermAttr a =
  \case
    Obj o -> ppObjAttr a o
    Dot t b -> ppDotAttr a (t, b)
    App t1 (b, t2) -> ppAppAttr a (t1, (Just b, t2))
    Loc n -> \_ -> ppLoc n
    DataTerm t -> 
      case t of 
        DataInteger i-> \_ -> ppInt a i

ppInt :: Maybe Attr -> Integer -> Doc ann
ppInt a i = 
  case a of 
    Just attr -> pretty i <> pretty sepAttach <> pretty attr
    Nothing -> pretty i


-- TODO several or number?
ppLoc :: Int -> Doc ann
ppLoc n
  | n == 0 = pretty ("$"::String)
  | n > 0 = pretty $ intercalate "." (replicate n "^" )
  | otherwise = error "incorrect locator value"


ppObjAttr :: Maybe Attr -> Object Term -> Separator -> Doc ann
ppObjAttr a o separator =
  nest 2 $
  encloseSep "[" "]" " " (pretty <$> void) <>
  case a of 
    Just attr -> pretty separator <> pretty attr
    Nothing  -> ""
  <> hardline 
  <> ppAttachedAttrs attached separator
  where
    (void, attached) = splitAttrs o

ppAttachedAttrs :: [(Attr, Term)] -> Separator -> Doc ann
ppAttachedAttrs attrs separator = 
  vsep $ map (\(a,t) -> ppTermAttr a t separator) attrs'
  where 
    attrs' = map (\(a, t) -> (Just a, t)) attrs

ppDotAttr :: Maybe Attr -> (Term, Attr) -> Separator -> Doc ann
ppDotAttr a (t, b) separator =
  nest 2 $ 
  pretty b <> 
  "." <> 
  case a of
    Just attr -> pretty separator <> pretty attr
    Nothing  -> ""
  <>
  hardline <>
  ppTerm t


-- TODO support application to object?
ppAppAttr :: Maybe Attr -> (Term, (Maybe Attr, Term)) -> Separator -> Doc ann
ppAppAttr a (t1, (b, t2)) separator = 
  ppTermAttr a t1 separator <> 
  hardline <>
  indent 2
  (ppTermAttr b t2 sepArgument)

ppAttrWithValue :: (Attr, AttrValue Term) -> Doc ann
ppAttrWithValue (a, value) =
  group $ group (pretty a <+> ">") <+> ppAttrValue value

ppAttrValue :: AttrValue Term -> Doc ann
ppAttrValue =
  \case
    VoidAttr -> "?"
    Attached t -> ppTerm t
