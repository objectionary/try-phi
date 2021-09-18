module Phi.Minimal.Parser exposing (..)

{-| A term of Minimal 𝜑-calculus typed in online editor is in
one of the following forms:

1.  Object term: [ a₁ -> ?, …, aₖ -> ?, b₁ -> t₁, …, bₙ -> tₙ ]
2.  Attribute access: t.a
3.  Application: t(a -> u)
4.  Parent object locator: (^.)ⁿ, $ for n = 0

-}

import Dict
import Parser exposing (..)
import Phi.Minimal.Syntax exposing (..)
import Set


{-| Parse String to get a Term
-}
parse : String -> Result (List DeadEnd) Term
parse =
    Parser.run (term |. spaces |. end)


term : Parser Term
term =
    lazy (\_ -> termNoDotApp)
        |> andThen (\t -> loop t termHelper)


termHelper : Term -> Parser (Parser.Step Term Term)
termHelper t =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed (Loop << Dot t)
                |. symbol "."
                |= attr
            , succeed (Loop << App t)
                |. symbol "("
                |. spaces
                |= attrAssignment
                |. spaces
                |. symbol ")"
            , succeed (Done t)
            ]


{-| TODO
-}
mkDot : Term -> List Attr -> Term
mkDot =
    List.foldl (\a t -> Dot t a)



-- TODO parse ^.^.^ and count


termNoDotApp : Parser Term
termNoDotApp =
    oneOf
        [ succeed identity
            |. symbol "("
            |= lazy (\_ -> term)
            |. symbol ")"
        , succeed (Object << Dict.fromList)
            |= sequence
                { start = "["
                , separator = ","
                , end = "]"
                , spaces = spaces
                , trailing = Forbidden
                , item = attrAssignment
                }
        ]


attrAssignment : Parser ( Attr, AttrValue )
attrAssignment =
    succeed (\x y -> ( x, y ))
        |= attr
        |. spaces
        |. symbol "->"
        |. spaces
        |= lazy (\_ -> Attached term)


attr : Parser Attr
attr =
    oneOf
        [ succeed "𝜑" |. symbol "@"
        -- actually, variable names
        , variable
            { start = \c -> Char.isAlpha c
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "ρ", "ξ" ]
            }
        ]


locator : Parser Term
locator =
    oneOf
        [ succeed "ρ" |. symbol "^"
        , succeed "ξ" |. symbol "$"
        , succeed "𝜑" |. symbol "@"
        , succeed "δ" |. keyword "__data__"
        , variable
            { start = Char.isAlpha
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList []
            }
        ]
