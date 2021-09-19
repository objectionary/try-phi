module Phi.Minimal.LocatorParser exposing (..)

{-| A term of Minimal 𝜑-calculus typed in online editor is in
one of the following forms:

1.  Object term: [ a₁ -> ?, …, aₖ -> ?, b₁ -> t₁, …, bₙ -> tₙ ]
2.  Attribute access: t.a
3.  Application: t₁(a -> t₂)
4.  Parent object locator: (^.)ⁿ, $ for n = 0

-}

import List exposing ((::))
import Parser exposing (..)
import Phi.Minimal.Syntax exposing (..)
import Set
import Debug exposing(..)

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
  |= sep
    { separator = "."
    , spaces = succeed ()
    , item = singleLocator
    , trailing = Forbidden
    }

{-| Parse a sequence of items separated by a separator.

    > opts = {separator = ";", spaces = succeed (), item = int, trailing = Forbidden}
    > run (sep opts) "1;2;3"
    Ok [1,2,3]
-}
sep :
  { separator : String
  , spaces : Parser ()
  , item : Parser a
  , trailing : Trailing
  }
  -> Parser (List a)
sep options =
  succeed (::)
  |= options.item
  |= oneOf
    [ succeed identity
      |. symbol options.separator
      |= sequence
        { start = ""
        , separator = options.separator
        , end = ""
        , spaces = options.spaces
        , item = options.item
        , trailing = options.trailing
        }
    , succeed []
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
