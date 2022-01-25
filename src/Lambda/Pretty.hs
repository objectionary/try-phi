{-# OPTIONS_GHC -Wall -fno-warn-orphans        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Lambda.Pretty where

import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import           Prettyprinter  as Doc

import           Lambda.Model

instance Show Term where
  show = show . pretty

instance Pretty Term where
  pretty = ppTerm

ppTerm :: Term -> Doc ann
ppTerm =
  \case
    Var n -> pretty n
    Abs body -> "λ" <> ppTerm body
    App fun arg -> ppTermFun fun <+> ppTermArg arg
    Dot t a -> ppTermArg t <> dot <> pretty a
    With t o -> ppTermArg t <+> "with" <+> ppObj o
    Obj o -> ppObj o
    Cat l r -> ppTermArg l <+> "‖" <+> ppTermArg r
    Fix t -> "fix" <+> ppTermArg t

ppTermFun :: Term -> Doc ann
ppTermFun =
  \case
    t@Var {} -> ppTerm t
    t@Dot {} -> ppTerm t
    t@Obj {} -> ppTerm t
    t@App {} -> ppTerm t
    t@Fix {} -> ppTerm t
    t@Abs {} -> parens (ppTerm t)
    t@With {} -> parens (ppTerm t)
    t@Cat {} -> parens (ppTerm t)

ppTermArg :: Term -> Doc ann
ppTermArg =
  \case
    t@Var {} -> ppTerm t
    t@Dot {} -> ppTerm t
    t@Obj {} -> ppTerm t
    t@App {} -> parens (ppTerm t)
    t@Fix {} -> parens (ppTerm t)
    t@Abs {} -> parens (ppTerm t)
    t@With {} -> parens (ppTerm t)
    t@Cat {} -> parens (ppTerm t)

encloseSepAfter :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseSepAfter bra ket separator =
  \case
    [] -> bra <> ket
    [doc] -> bra <> doc <> ket
    docs -> bra <> mconcat (addSepAfter docs) <> ket
  where
    addSepAfter []         = []
    addSepAfter [doc]      = [doc]
    addSepAfter (doc:docs) = (doc <> separator) : addSepAfter docs

ppObj :: InsOrdHashMap Attr Term -> Doc ann
ppObj o
  | null o = "{}"
  | otherwise =
    group .
    nest 2 .
    encloseSepAfter ("{" <> line) (nest (-2) (line <> "}")) (comma <> line) .
    map ppAttrWithValue . InsOrdHashMap.toList $
    o

ppAttrWithValue :: (Attr, Term) -> Doc ann
ppAttrWithValue (a, value) = group $ group (pretty a <+> "=") <+> ppTerm value
