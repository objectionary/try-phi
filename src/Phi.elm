module Phi exposing (..)

import Parser
import Phi.Syntax exposing (DefaultTerm)
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
          "\n  " ++ Phi.Pretty.ppTerm pp t ++
          "\n" ++
          "\ncould not reduce further than its weak head normal form:" ++
          "\n  " ++
          "\n  " ++ Phi.Pretty.ppTerm pp t2
        Ok result ->
          "SUCCESS: dataization of the following term" ++
          "\n  " ++
          "\n  " ++ Phi.Pretty.ppTerm pp t ++
          "\n  " ++
          "\nhas been successfully dataized to value" ++
          "\n  " ++
          "\n  " ++ pp result

pp : Result String Int -> String
pp r =
  case r of
    Err err -> "ERROR: " ++ err
    Ok n    -> String.fromInt n
