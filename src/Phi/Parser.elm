module Phi.Parser exposing (..)

import Dict
import Parser exposing (..)
import Set
import Phi.Syntax exposing (..)

parse : String -> Result (List DeadEnd) (Term Int)
parse = Parser.run (term |. spaces |. end)

term : Parser (Term Int)
term =
  lazy (\_ -> termNoDotApp)
    |> andThen (\t -> loop t termHelper)

termHelper : Term Int -> Parser (Step (Term Int) (Term Int))
termHelper t = oneOf
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

mkDot : Term Int -> List Attr -> Term Int
mkDot = List.foldl (\a t -> Dot t a)

termNoDotApp : Parser (Term Int)
termNoDotApp = oneOf
  [ succeed identity
      |. symbol "("
      |= lazy (\_ -> term)
      |. symbol ")"
  , succeed FreeAttr
      |. oneOf [keyword "?", keyword "ø", keyword "∅"]
  , succeed Var |= locator

  , succeed (Object << Dict.fromList)
      |= sequence 
        { start     = "["
        , separator = ","
        , end       = "]"
        , spaces    = spaces
        , trailing  = Forbidden
        , item      = attrAssignment
        }
  ]

attrAssignment : Parser (Attr, Term Int)
attrAssignment
  = succeed (\x y -> (x, y))
  |= attr
  |. spaces
  |. symbol "->"
  |. spaces
  |= lazy (\_ -> term)

attr : Parser Attr
attr = oneOf
  [ succeed "𝜑" |. symbol "@"
  , succeed "δ" |. keyword "__data__"
  , variable
      { start = Char.isAlpha
      , inner = \c -> Char.isAlphaNum c || c == '_'
      , reserved = Set.fromList [ "ρ", "ξ" ]
      }
  ]

locator : Parser Locator
locator = oneOf
  [ succeed "ρ" |. symbol "^"
  , succeed "ξ" |. symbol "$"
  , succeed "𝜑" |. symbol "@"
  , succeed "δ" |. keyword "__data__"
  , variable
      { start = Char.isAlpha
      , inner = \c -> Char.isAlphaNum c || c == '_'
      , reserved = Set.fromList [ ]
      }
  ]
