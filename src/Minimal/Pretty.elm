module Minimal.Pretty exposing (..)

import Minimal.Syntax exposing (..)
import Dict
import Tuple

-- |
--
-- > ppTerm (Locator 3)
-- "ρ.ρ.ρ" : String
ppTerm : Term -> String
ppTerm term =
  case term of
    Object o ->
      if Dict.isEmpty o
        then "⟦⟧"
        else ppObject o
    App t (a, s) ->
      ppTerm t ++ parens (ppAttrWithValue (a, Attached s))
    Dot t a -> ppTerm t ++ "." ++ ppAttr a
    Locator n -> ppLocator n

ppLocator : Int -> String
ppLocator n =
  case n of
    0 -> "ξ"
    1 -> "ρ"
    _ -> "ρ." ++ ppLocator (n - 1)

ppObject : Object -> String
ppObject
  = enclose "⟦ " " ⟧"
 << punctuate ", "
 << List.map ppAttrWithValue
 << List.sortBy Tuple.first
 << Dict.toList

ppAttrWithValue : (AttrName, AttrValue) -> String
ppAttrWithValue (a, t) = ppAttr a ++ " ↦ " ++ ppAttrValue t

ppAttrValue : AttrValue -> String
ppAttrValue v =
  case v of
    Void -> "∅"
    Attached t -> ppTerm t

ppAttr : AttrName -> String
ppAttr a = a
--  case a of
--    AttrIdent x -> x
--    AttrPhi     -> "𝜑"
--    AttrDelta   -> "δ"

parens : String -> String
parens = enclose "(" ")"

enclose : String -> String -> String -> String
enclose l r s = l ++ s ++ r

punctuate : String -> List String -> String
punctuate sep = String.concat << List.intersperse sep
