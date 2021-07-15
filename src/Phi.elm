module Phi exposing (..)

import Phi.Syntax exposing (Term(..))
import Phi.Pretty
import Phi.Examples

compile : String -> String
compile input = Phi.Pretty.ppTerm String.fromInt (parse input)

parse : String -> Term Int
parse input =
  case input of
    "ex1" -> Phi.Examples.ex1
    _ -> Dot (Var "x") "y"
