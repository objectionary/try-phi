{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Phi.Minimal.EO.Pretty where

-- TODO use Prettyprinter since Doc is deprecated
-- import           Data.Text.Prettyprint.Doc as Doc
import  Prettyprinter as Pretty

import Phi.Minimal.Model
    ( Term(..),
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
 

ppTerm :: Term -> Doc ann
ppTerm =
  \case
    Obj o -> ppObjAttr "" o
    Dot t a -> ppTerm t <> dot <> pretty a
    App t (a, u) -> ppTerm t <> parens (ppAttrWithValue (a, Attached u))
    Loc n -> ppLoc n
    INTEGER i -> ppInt i

ppTermAttr :: Attr -> Term -> Doc ann
ppTermAttr a =
  \case
    Obj o -> ppObjAttr a o
    Dot t b -> ppDotAttr a (t, b)
    App t1 (b, t2) -> ppApp t1 b t2
    Loc n -> ppLoc n
    INTEGER i -> ppInt i

ppInt :: Integer -> Doc ann
ppInt i = pretty $ show i

ppLoc :: Int -> Doc ann
ppLoc n = pretty ("^" <> n')
  where
    n' = map toSuperscript (show n)
    toSuperscript c =
      case lookup c (zip "1234567890" "¹²³⁴⁵⁶⁷⁸⁹⁰") of
        Just c' -> c'
        _       -> c

ppObjAttr :: Attr -> Object Term -> Doc ann
ppObjAttr a o =
  nest 2 $
  encloseSep "[" "]" " " (pretty <$> void) <+>
  ">" <+> pretty a <> hardline <> ppAttachedAttrs attached
  where
    (void, attached) = splitAttrs o

ppAttachedAttrs :: [(Attr, Term)] -> Doc ann
ppAttachedAttrs = vsep . map (uncurry ppTermAttr)

ppDotAttr :: Attr -> (Term, Attr) -> Doc ann
ppDotAttr a (t, b) =
  nest 2 $ pretty b <> "." <+> ">" <+> pretty a <> hardline <> ppTerm t


-- assume unnest after printing t1
ppApp :: Term -> Attr -> Term -> Doc ann
ppApp t1 a t2 = ppTerm t1 <+> ppTerm t2 <> ":" <> pretty a

ppAttrWithValue :: (Attr, AttrValue Term) -> Doc ann
ppAttrWithValue (a, value) =
  group $ group (pretty a <+> ">") <+> ppAttrValue value

ppAttrValue :: AttrValue Term -> Doc ann
ppAttrValue =
  \case
    VoidAttr -> "?"
    Attached t -> ppTerm t
