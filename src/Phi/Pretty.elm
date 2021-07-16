module Phi.Pretty exposing (..)

import Phi.Syntax exposing (..)
import Dict
import Tuple

ppSteps : (d -> String) -> List (Result String (Term d)) -> String
ppSteps ppData results = String.concat (List.intersperse "\n⇝ " (List.map (ppResult ppData) results))

ppResult : (d -> String) -> Result String (Term d) -> String
ppResult ppData res =
  case res of
    Err err -> "[ERROR]: " ++ err
    Ok term -> ppTerm ppData term

ppTerm : (d -> String) -> Term d -> String
ppTerm ppData term =
  case term of
    Var a -> ppLocator a
    App t (a, s) -> ppTerm ppData t ++ parens (ppAttr a ++ " ↦ " ++ ppTerm ppData s)
    Dot t a -> ppAsLocator ppData t ++ "." ++ ppAttr a
    Object o ->
      case Dict.get "δ" o of
        Just (Data d) -> ppData d
        _ -> if Dict.isEmpty o
                then "⟦⟧"
                else ppObject ppData o
    FreeAttr -> "∅"
    Data d -> ppData d
    Atom name _ _ _ -> "<ATOM " ++ name ++ ">"

ppObject : (d -> String) -> Object d -> String
ppObject ppData
  = enclose "⟦ " " ⟧"
 << punctuate ", "
 << List.map (ppAttrWithValue ppData)
 << List.sortBy Tuple.first
 << Dict.toList

ppAttrWithValue : (d -> String) -> (Attr, Term d) -> String
ppAttrWithValue ppData (a, t) = ppAttr a ++ " ↦ " ++ ppTerm ppData t

ppAsLocator : (d -> String) -> Term d -> String
ppAsLocator ppData t =
  case t of
    App _ _ -> ppTerm ppData t
    _     -> ppTerm ppData t

ppAttr : Attr -> String
ppAttr a = a
--  case a of
--    AttrIdent x -> x
--    AttrPhi     -> "𝜑"
--    AttrDelta   -> "δ"

ppLocator : Locator -> String
ppLocator l = l
--  case l of
--    AttrLocator a -> ppAttr a
--    Rho     -> "ρ"
--    Xi      -> "ξ"

parens : String -> String
parens = enclose "(" ")"

enclose : String -> String -> String -> String
enclose l r s = l ++ s ++ r

punctuate : String -> List String -> String
punctuate sep = String.concat << List.intersperse sep
