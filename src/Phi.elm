module Phi exposing (..)

import Parser
import Phi.Syntax exposing (DefaultTerm)
import Phi.Pretty
import Phi.Parser
import Phi.Eval
import Phi.Examples

type Mode
  = FullPhi
  | MinimalPhi

interpretStepsN : Mode -> Int -> String -> String
interpretStepsN mode n input =
  case mode of
    FullPhi ->
      case Phi.Parser.parse input of
        Err err -> "ERROR: failed to parse input:\n" ++ Parser.deadEndsToString err
        Ok t -> Phi.Pretty.ppStepsN pp (Phi.Eval.dataizeStepsN n t)

    MinimalPhi -> "ERROR: minimal phi calculus is not implemented yet"
      {-
      case Phi.Minimal.Parser.parse input of
        Err err -> "ERROR: failed to parse input:\n" ++ Parser.deadEndsToString err
        Ok t -> Phi.Minimal.Pretty.ppTerm (Phi.Minimal.Syntax.whnf t)
      -}

interpretSteps : String -> String
interpretSteps input =
  case Phi.Parser.parse input of
    Err err -> "ERROR: failed to parse input:\n" ++ Parser.deadEndsToString err
    Ok t -> Phi.Pretty.ppSteps pp (Phi.Eval.dataizeSteps t)

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
