module Phi exposing (..)

import Parser
import Phi.Syntax exposing (Term(..))
import Phi.Pretty
import Phi.Parser
import Phi.Eval
import Phi.Examples

interpret : String -> String
interpret input =
  case Phi.Parser.parse input of
    Err err -> "ERROR: failed to parse input:\n" ++ Parser.deadEndsToString err
    Ok t ->
      case Phi.Eval.dataize t of
        Err t2 ->
          "ERROR: failed to dataize term" ++
          "\n  " ++
          "\n  " ++ Phi.Pretty.ppTerm String.fromInt t ++
          "\n" ++
          "\ncould not reduce further than its weak head normal form:" ++
          "\n  " ++
          "\n  " ++ Phi.Pretty.ppTerm String.fromInt t2
        Ok result ->
          "SUCCESS: dataization of the following term" ++
          "\n  " ++
          "\n  " ++ Phi.Pretty.ppTerm String.fromInt t ++
          "\n  " ++
          "\nhas been successfully dataized to value" ++
          "\n  " ++
          "\n  " ++ String.fromInt result

parse : String -> Result String (Term Int)
parse input =
  case input of
    "ex1" -> Ok (Phi.Examples.ex1)
    _ -> Err "parser is not implemented yet"
