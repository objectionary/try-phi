module Phi.Minimal.Syntax exposing (Term(..))

{-| This module defines the syntax for the minimal phi calculus.

# Definition
@docs Term

-}

import Dict exposing (Dict)

type alias Attr = String

type AttrValue
  = Void
  | Attached Term

type alias Object = Dict Attr AttrValue

type alias Substitution = Dict Int Term


{-| Term 

# Hey

-}
type Term
  = Object Object
  | Dot Term Attr
  | App Term (Attr, Term)
  | Locator Int

substituteXi : Term -> Term -> Term
substituteXi = substituteLocator 0

substituteLocator : Int -> Term -> Term -> Term
substituteLocator k parent term =
  case term of
    Locator n ->
      if n == k
        then parent
        else Locator n
    Dot t a ->
      Dot (substituteLocator k parent t) a
    App t (a, u) ->
      App (substituteLocator k parent t) (a, substituteLocator k parent u)
    Object o -> Object (Dict.map (\a t ->
      case t of
        Void -> Void
        Attached u -> Attached (substituteLocator (k + 1) (incLocators parent) u)
      ) o)

incLocators : Term -> Term
incLocators = incLocatorsFrom 0

incLocatorsFrom : Int -> Term -> Term
incLocatorsFrom k term =
  case term of
    Object o -> Object (Dict.map (\a t ->
      case t of
        Void -> Void
        Attached u -> Attached (incLocatorsFrom (k + 1) u)) o)
    Dot t a -> Dot (incLocatorsFrom k t) a
    App t (a, u) -> App (incLocatorsFrom k t) (a, incLocatorsFrom k u)
    Locator n ->
      if n >= k
        then Locator (n + 1)
        else Locator n

whnf : Term -> Term
whnf term =
  case term of
    Locator n -> Locator n
    App t (a, u) ->
      case whnf t of
        Object o ->
          case Dict.get a o of
            Just Void -> Object (Dict.insert a (Attached (incLocators u)) o)
            Just _  -> App (Object o) (a, u)
            Nothing -> App (Object o) (a, u)
        t2 -> App t2 (a, u)

    Dot t a ->
      case whnf t of
        Object o ->
          case Dict.get a o of
            Just (Attached u) -> substituteXi u (Object o)
            Just Void -> Dot (Object o) a
            Nothing ->
              if Dict.member "𝜑" o
                then Dot (Dot (Object o) "𝜑") a
                else Dot (Object o) a
        t2 -> Dot t2 a

    Object o -> Object o
