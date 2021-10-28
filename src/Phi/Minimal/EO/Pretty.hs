{-# OPTIONS_GHC -Wall -fno-warn-orphans        #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Phi.Minimal.EO.Pretty where

import           Data.Text.Prettyprint.Doc as Doc

import           Phi.Minimal.Model

instance Show Term where show = show . pretty
instance Pretty Term where pretty = ppTerm

ppTerm :: Term -> Doc ann
ppTerm = \case
  Obj o -> let (_, attached) = splitAttrs o in ppAttachedAttrs attached
  Dot t a -> ppTerm t <> dot <> pretty a
  App t (a, u) ->
    ppTerm t <> parens (ppAttrWithValue (a, Attached u))
  Loc n -> ppLoc n

ppObjAttr :: Attr -> Object Term -> Doc ann
ppObjAttr a o = nest 2 $
  encloseSep "[" "]" " " (pretty <$> void) <+> ">" <+> pretty a
  <> hardline <> ppAttachedAttrs attached
  where
    (void, attached) = splitAttrs o

ppAttachedAttrs :: [(Attr, Term)] -> Doc ann
ppAttachedAttrs = vsep . map (uncurry ppTermAttr)

ppDotAttr :: Attr -> (Term, Attr) -> Doc ann
ppDotAttr a (t, b) = nest 2 $
  "." <> pretty b <+> ">" <+> pretty a
  <> hardline <> ppTerm t

ppTermAttr :: Attr -> Term -> Doc ann
ppTermAttr a = \case
  Obj o -> ppObjAttr a o
  Dot t b -> ppDotAttr a (t, b)
  Apps _t _args -> error "not implemented"
  Loc n -> ppLoc n

ppAttrWithValue :: (Attr, AttrValue Term) -> Doc ann
ppAttrWithValue (a, value) = group $
  group (pretty a <+> "↦") <+> ppAttrValue value

ppAttrValue :: AttrValue Term -> Doc ann
ppAttrValue = \case
  VoidAttr -> "ø"
  Attached t -> ppTerm t

ppLoc :: Int -> Doc ann
ppLoc n = pretty ("ρ" <> n')
  where
    n' = map toSuperscript (show n)
    toSuperscript c =
      case lookup c (zip "1234567890" "¹²³⁴⁵⁶⁷⁸⁹⁰") of
        Just c' -> c'
        _       -> c

