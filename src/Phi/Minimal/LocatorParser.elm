module Phi.Minimal.LocatorParser exposing (..)

{-| A term of Minimal 𝜑-calculus typed in online editor is in
one of the following forms:

1.  Object term: [ a₁ -> ?, …, aₖ -> ?, b₁ -> t₁, …, bₙ -> tₙ ]
2.  Attribute access: t.a
3.  Application: t₁(a -> t₂)
4.  Parent object locator: (^.)ⁿ, $ for n = 0

-}

import Parser exposing (..)
import Phi.Minimal.Syntax exposing (..)
import Set
import Debug exposing(..)

{-| TODO: parse locator of form (^.)ⁿ.a. …

        run locatorParser "^.^.a.a" == Ok ((Dot (Locator 2) "a") "a")
        run locatorParser "^.a.^.a" == Err ...

-}

locatorParser : Parser Term 
locatorParser = loop (Locator 0) locator 

locator : Term -> Parser (Parser.Step Term Term)
locator t =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed (Loop << dot t)
                |. symbol "."
                |= attr 
            , succeed (Done t)
            ]



{- Construct a Locator Term. Handle (^.)ⁿ cases

   dot (Locator 0) "$" == Locator 0
   dot (Locator n) "^" == (Locator n+1) "^" -- n >= 0
   dot (Locator n) a == Dot (Locator n) a -- for valid a
   dot _ "^" == /Err .../
   dot _ "$" == /Err .../
   dot t a == Dot t a -- for valid a
-}


dot : Term -> AttrName -> Term
dot t a =
    let
        aValidated =
            validatedAttrName a
    in
    case ( t, a ) of
        ( Locator 0, "$" ) ->
            t

        ( Locator n, "^" ) ->
            if n > 0 then
                Locator (n + 1)

            else
                Debug.todo "Error: n < 0"

        ( Locator _, _ ) ->
            
            Dot t aValidated
        
        (_, "$") -> Debug.todo "Error: $ not in prefix"

        (_, "^") -> Debug.todo "Error: ^ not in prefix"

        _ -> Dot t aValidated



attr : Parser AttrName
attr =
    oneOf
        [ succeed "𝜑" |. symbol "@"
        -- actually, variable names
        , variable
            { start = \c -> Char.isAlpha c
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList []
            }
        ]

validatedAttrName : AttrName -> AttrName
validatedAttrName a =
    case run attr a of
        Ok b ->
            b

        _ ->
            "Error: invalid attribute name"