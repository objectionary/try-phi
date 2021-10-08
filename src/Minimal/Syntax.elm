module Minimal.Syntax exposing
    ( Term(..), Object, AttrName, AttrValue(..)
    , Substitution, substituteXi, substituteLocator
    , whnf
    , incLocators, incLocatorsFrom
    )

{-| This module defines the syntax for the minimal 𝜑-calculus.


# Definition

@docs Term, Object, Attr, AttrValue


# Locator substitution

@docs Substitution, substituteXi, substituteLocator


# Evaluation

@docs whnf


# Helpers

@docs incLocators, incLocatorsFrom

-}

import Dict exposing (Dict)


{-| Attribute name-}
type alias AttrName =
    String


{-| Value of attribute inside an object -}
type AttrValue
    = Void
    | Attached Term


{-| Object is a dictionary of attribute names and their values -}
type alias Object =
    Dict AttrName AttrValue


{-| TODO -}
type alias Substitution =
    Dict Int Term


{-| A term of 𝜑-calculus is in one of the following forms:

1.  Object term: ⟦ a₁ ↦ ∅, …, aₖ ↦ ∅, b₁ ↦ t₁, …, bₙ ↦ tₙ ⟧
1.  Attribute access via dot: t.a
1.  Application: t₁(a ↦ t₂)
1.  Parent object locator: ρⁿ

-}
type Term
    = Object Object
    | Dot Term AttrName
    | App Term ( AttrName, Term )
    | Locator Int


{-| Replace all parent objects locators referencing the closest outermost term (i.e. ξ or ρ⁰) with a given term.

    >>> t = Dot (Object (Dict.fromList [("x", Attached (Locator 0)), ("y", Attached (Locator 1))])) "x"
    >>> ppTerm t
    "⟦ x ↦ ξ, y ↦ ρ ⟧.x" : String
    >>> ppTerm (substituteXi t t)
    "⟦ x ↦ ξ, y ↦ ⟦ x ↦ ξ, y ↦ ρ.ρ ⟧.x ⟧.x" : String

-}
substituteXi : Term -> Term -> Term
substituteXi =
    substituteLocator 0


{-| TODO
-}
substituteLocator : Int -> Term -> Term -> Term
substituteLocator k parent term =
    case term of
        Locator n ->
            if n < k then
                Locator n

            else if n == k then
                parent

            else
                Locator (n - 1)

        Dot t a ->
            Dot (substituteLocator k parent t) a

        App t ( a, u ) ->
            App (substituteLocator k parent t) ( a, substituteLocator k parent u )

        Object o ->
            Object
                (Dict.map
                    (\a t ->
                        case t of
                            Void ->
                                Void

                            Attached u ->
                                Attached (substituteLocator (k + 1) (incLocators parent) u)
                    )
                    o
                )


{-| TODO
-}
incLocators : Term -> Term
incLocators =
    incLocatorsFrom 0


{-| TODO
-}
incLocatorsFrom : Int -> Term -> Term
incLocatorsFrom k term =
    case term of
        Object o ->
            Object
                (Dict.map
                    (\a t ->
                        case t of
                            Void ->
                                Void

                            Attached u ->
                                Attached (incLocatorsFrom (k + 1) u)
                    )
                    o
                )

        Dot t a ->
            Dot (incLocatorsFrom k t) a

        App t ( a, u ) ->
            App (incLocatorsFrom k t) ( a, incLocatorsFrom k u )

        Locator n ->
            if n >= k then
                Locator (n + 1)

            else
                Locator n


{-| Compute a 𝜑-term to weak head normal form (WHNF).

    > t = Dot (Object (Dict.fromList [("x", Attached (Dot (Object (Dict.fromList [("y", Attached (Locator 2))])) "y"))])) "x"
    Dot (Object (Dict.fromList [("x",Attached (Dot (Object (Dict.fromList [("y",Attached (Locator 2))])) "y"))])) "x"
        : Term
    > ppTerm t
    "⟦ x ↦ ⟦ y ↦ ρ.ρ ⟧.y ⟧.x" : String
    > ppTerm (whnf t)
    "ξ" : String

-}
whnf : Term -> Term
whnf term =
    case term of
        Locator n ->
            Locator n

        App t ( a, u ) ->
            case whnf t of
                Object o ->
                    case Dict.get a o of
                        Just Void ->
                            Object (Dict.insert a (Attached (incLocators u)) o)

                        Just _ ->
                            App (Object o) ( a, u )

                        Nothing ->
                            App (Object o) ( a, u )

                t2 ->
                    App t2 ( a, u )

        Dot t a ->
            case whnf t of
                Object o ->
                    case Dict.get a o of
                        Just (Attached u) ->
                            whnf (substituteXi (Object o) u)

                        Just Void ->
                            Dot (Object o) a

                        Nothing ->
                            if Dict.member "𝜑" o then
                                Dot (Dot (Object o) "𝜑") a

                            else
                                Dot (Object o) a

                t2 ->
                    Dot t2 a

        Object o ->
            Object o
