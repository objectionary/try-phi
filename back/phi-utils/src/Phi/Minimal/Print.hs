{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Phi.Minimal.Print(ppPhiSource) where

import           Data.Graph.Inductive.PatriciaTree    (Gr)
import           Data.HashMap.Strict.InsOrd           (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd           as InsOrdHashMap

import           Prettyprinter            as Doc
import           Phi.Minimal.Machine.CallByName
import qualified Phi.Minimal.Machine.CallByName.Graph as Graph
import           Phi.Minimal.Model

ppPhiSource :: Term -> Doc ann
ppPhiSource =
  \case
    Obj o -> ppObj o
    Dot t a -> ppPhiSource t <> dot <> ppAttr a
    App t (a, u) -> ppPhiSource t <> parens (ppAttrWithValue (a, Attached u))
    Loc n -> ppLoc n
    DataTerm t ->
      case t of
        DataInteger i -> pretty i
        NoData -> pretty $ show NoData

ppInt :: Integer -> Doc ann
ppInt = pretty

enc :: [Doc ann] -> Doc ann
enc ds = brackets ds'
  where
    ds' =
      case ds of
        [] -> mconcat []
        [doc] -> line <> indent 2 doc <> line <> indent (-2) mempty
        x:xs -> line <> indent 2 x <> indent (-2) mempty <> mconcat ((\x -> line <> comma <> indent 1 x <> indent (-1) mempty) <$> xs) <> line


-- enc' = list

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

ppObj :: Object Term -> Doc ann
ppObj o
  | null (getObject o) = brackets mempty
  | otherwise =
    enc .
    -- encloseSepAfter ("[" <> line) (nest (-2) (line <> "]")) (comma <> line) .
    map ppAttrWithValue . InsOrdHashMap.toList . getObject $
    o

ppAttrWithValue :: (Attr, AttrValue Term) -> Doc ann
ppAttrWithValue (a, value) =
  ppAttr a <+> "->" <+> ppAttrValue value

ppAttr :: Attr -> Doc ann
ppAttr a
  | a == "@" = "@"
  | otherwise = pretty a

ppAttrValue :: AttrValue Term -> Doc ann
ppAttrValue =
  \case
    VoidAttr -> "?"
    Attached t -> ppPhiSource t

ppLoc :: Int -> Doc ann
ppLoc n = pretty ("^" <> show n)