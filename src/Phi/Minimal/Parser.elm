module Phi.Minimal.Parser exposing (..)

{-| A term of Minimal 𝜑-calculus typed in online editor is in
one of the following forms:

1.  Object term: [ a₁ -> ?, …, aₖ -> ?, b₁ -> t₁, …, bₙ -> tₙ ]
2.  Attribute access: t.a
3.  Application: t₁(a -> t₂)
4.  Parent object locator: (^.)ⁿ, $ for n = 0

-}


import Dict
import Parser exposing (..)
import Phi.Minimal.Syntax exposing (..)
import Set

{-| Parse String to get an error or a Term-}
parse : String -> Result (List DeadEnd) Term
parse =
    Parser.run (term |. spaces |. end)

{-| -}
term : Parser Term
term =
    lazy (\_ -> termNoDotApp)
        |> andThen (\t -> loop t repeatedDotOrApp)


{-| Parser for repeated dot (.) accesses and applications and for attaching them to a given term -}
repeatedDotOrApp : Term -> Parser (Parser.Step Term Term)
repeatedDotOrApp t =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed (Loop << Dot t)
                |. symbol "."
                |= attr
            , succeed (Loop << App t)
                |. symbol "("
                |. spaces
                |= attrAttachedAssignment
                |. spaces
                |. symbol ")"
            , succeed (Done t)
            ]

{-| Parse a term that is not a DOT or APP. -}
termNoDotApp : Parser Term
termNoDotApp = oneOf [ termObject , locator ]

{-| Object term parser. -}
termObject : Parser Term
termObject = succeed (Object << Dict.fromList)
  |= sequence
      { start = "["
      , separator = ","
      , end = "]"
      , spaces = spaces
      , trailing = Forbidden
      , item = attrAssignment
      }

{-| Parser for assignments inside application. -}
attrAttachedAssignment : Parser ( AttrName, Term )
attrAttachedAssignment =
    succeed (\x y -> ( x, y ))
        |= attr
        |. spaces
        |. symbol "->"
        |. spaces
        |= lazy (\_ -> term)

{-| Parser for assignments inside object. -}
attrAssignment : Parser ( AttrName, AttrValue )
attrAssignment =
    succeed (\x y -> ( x, y ))
        |= attr
        |. spaces
        |. symbol "->"
        |. spaces
        |= oneOf
          [ succeed Attached |= lazy (\_ -> term)
          , succeed Void |. symbol "?"
          ]

{-| Parse an attribute name. -}
attr : Parser AttrName
attr =
    oneOf
        [ succeed "𝜑" |. symbol "@"
        -- actually, variable names
        , variable
            { start = \c -> Char.isAlpha c
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "^", "$" ]
            }
        ]

{-| Parse a locator.

    > run locator "$.^"
    Ok (Locator 1)
        : Result (List DeadEnd) Phi.Minimal.Syntax.Term

    > run locator "^.^.^.^"
    Ok (Locator 4)
        : Result (List DeadEnd) Phi.Minimal.Syntax.Term
-}
locator : Parser Term 
locator = succeed (Locator << List.sum)
  |= sep1
    { separator = "."
    , spaces = succeed ()
    , item = singleLocator
    , trailing = Forbidden
    }

{-| Parse a sequence of one or more items separated by a separator.

    > opts = {separator = ";", spaces = succeed (), item = int, trailing = Forbidden}
    > run (sep1 opts) "1;2;3"
    Ok [1,2,3]
-}
sep1 :
  { separator : String
  , spaces : Parser ()
  , item : Parser a
  , trailing : Trailing
  }
  -> Parser (List a)
sep1 options =
  succeed (::)
  |= options.item
  |= loop [] (sepStep options)

sepStep :
  { separator : String
  , spaces : Parser ()
  , item : Parser a
  , trailing : Trailing
  }
  -> List a
  -> Parser (Parser.Step (List a) (List a) )
sepStep options l = oneOf
  [ succeed (Loop << (\x -> x :: l))
    |. backtrackable (symbol options.separator)
    |= options.item
  , succeed (Done l)
  ]

{-| Parse a single locator symbol

    > run singleLocator "$"
    Ok 0 : Result (List Parser.DeadEnd) Int

    > run singleLocator "^"
    Ok 1 : Result (List Parser.DeadEnd) Int
-}
singleLocator : Parser Int
singleLocator = oneOf
  [ succeed 1 |. symbol "^"
  , succeed 0 |. symbol "$"
  ]
