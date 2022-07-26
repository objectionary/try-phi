{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Phi.Minimal.PPToLatex (pretty, Latex (..)) where

import Data.Function ((&))
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.List (intercalate, partition)
import qualified Data.Text as T
import Phi.Minimal.Model
  ( Attr,
    AttrValue (..),
    DataValue (..),
    Object (Object, getObject),
    Term (..),
  )
import Prettyprinter as Doc hiding (indent, line')

strip = T.unpack . T.strip . T.pack

{-
& \ff{book2}(\ff{isbn}) \mapsto \llbracket \br
& \quad \ff{title} \mapsto \ff{"Object Thinking"}, \br
& \quad \ff{price} \mapsto \ff{memory} \br
& \rrbracket. \\

https://arxiv.org/pdf/2111.13384.pdf#page=8

https://github.com/objectionary/eo/blob/546279ffc483e1be7decb85ad7e067631fbe8d72/paper/sections/calculus.tex#L347
-}

newtype Latex a = Latex a

instance Show (Latex Term) where
  show s = unlines $ strip <$> lines (show $ amp' <> pretty s)

instance Pretty (Latex Term) where
  pretty (Latex p) = ppTerm p

instance Show (Latex (AttrValue Term)) where
  show = show . pretty

instance Pretty (Latex (AttrValue Term)) where
  pretty (Latex p) = ppAttrValue p

ppTerm :: Term -> Doc ann
ppTerm =
  \case
    Obj o -> ppObj o
    Dot t a -> ppTerm t <> dot <> ppAttr a
    App t (a, u) -> ppTerm t <> parens (ppAttrWithValue (a, Attached u))
    Loc n -> ppLoc n
    DataTerm t ->
      case t of
        DataInteger i -> pretty i
        NoData -> pretty $ show NoData

encloseSepAfter :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseSepAfter bra ket separator =
  \case
    [] -> bra <> ket
    [doc] -> bra <> doc <> ket
    docs -> bra <> mconcat (addSepAfter docs) <> ket
  where
    addSepAfter [] = []
    addSepAfter [doc] = [doc]
    addSepAfter (doc : docs) = (doc <> separator) : addSepAfter docs

llbracket :: Doc ann
llbracket = "\\llbracket"

rrbracket :: Doc ann
rrbracket = "\\rrbracket"

encBrackets :: Doc ann -> Doc ann
encBrackets = enclose llbracket rrbracket

amp :: Doc ann
amp = "&"

amp' :: Doc ann
amp' = amp <> space

quad :: Doc ann
quad = "\\quad"

replicate' :: Int -> Doc ann -> Doc ann -> Doc ann
replicate' n doc sep' = encloseSep "" "" sep' (replicate n doc)

insertQuad :: Doc ann
insertQuad =
  amp <> space
    <> nesting
      ( \n ->
          if n == 0
            then ""
            else replicate' (n `div` 2) quad space <> space
      )

line' :: Doc ann
line' = space <> "\\phiBreak" <> line

ppObj :: Object Term -> Doc ann
ppObj o
  | null (getObject o) = encBrackets ""
  | otherwise =
    o
      & getObject
      & InsOrdHashMap.toList
      & filter (\(_, v) -> v /= VoidAttr)
      & map (\x -> insertQuad <> ppAttrWithValue x)
      & (\l -> 
          if null l 
            then llbracket <+> rrbracket 
            else encloseSepAfter
              ( llbracket <> line'
              )
              (nest (-2) (line' <> insertQuad <> rrbracket))
              (comma <> line') l)
      & nest 2

ff :: Doc ann -> Doc ann
ff = enclose "\\phiAttr{" "}"

ppAttrWithValue :: (Attr, AttrValue Term) -> Doc ann
ppAttrWithValue (a', value) =
  ff (ppAttr a')
    <+> ( case value of
            Attached (Obj (Object o)) -> fas
              where
                (un, _) = partition ((==) VoidAttr . snd) (InsOrdHashMap.toList o)
                un' = fst <$> un
                fas
                  | null un' = ""
                  | otherwise = encloseSepAfter "("  ")" ", " (ppAttr <$> un')
            _ -> ""
        )
    <+> "\\mapsto"
    <+> ppAttrValue value

ppAttr :: Attr -> Doc ann
ppAttr a'
  | a' == "@" = "\\varphi"
  | otherwise = pretty a'

-- TODO print void attrs in parentheses after attr name
ppAttrValue :: AttrValue Term -> Doc ann
ppAttrValue =
  \case
    -- FIXME throw Exception
    VoidAttr -> "!Shouldn't print a value of an unattached attribute!"
    Attached t' -> ppTerm t'

ppLoc :: Int -> Doc ann
ppLoc n
  | n > 0 = pretty (intercalate "." (replicate n "\\rho"))
  | otherwise = "\\xi"