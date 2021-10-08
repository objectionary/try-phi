module Phi exposing (..)

import Parser
import Full.Syntax exposing (DefaultTerm)
import Full.Pretty
import Full.Parser
import Full.Eval
import Full.Examples

import Minimal.Parser
import Minimal.Pretty
import Minimal.Syntax

type Mode
  = FullPhi
  | MinimalPhi

interpretStepsN : Mode -> Int -> String -> String
interpretStepsN mode n input =
  case mode of
    FullPhi ->
      case Full.Parser.parse input of
        Err err -> "ERROR: failed to parse input:\n" ++ Parser.deadEndsToString err
        Ok t -> Full.Pretty.ppStepsN pp (Full.Eval.dataizeStepsN n t)

    MinimalPhi ->
      case Minimal.Parser.parse input of
        Err err -> "ERROR: failed to parse input:\n" ++ Parser.deadEndsToString err
        Ok t -> Minimal.Pretty.ppTerm (Minimal.Syntax.whnf t)

interpretSteps : String -> String
interpretSteps input =
  case Full.Parser.parse input of
    Err err -> "ERROR: failed to parse input:\n" ++ Parser.deadEndsToString err
    Ok t -> Full.Pretty.ppSteps pp (Full.Eval.dataizeSteps t)

interpret : String -> String
interpret input =
  case Full.Parser.parse input of
    Err err -> "ERROR: failed to parse input:\n" ++ Parser.deadEndsToString err
    Ok t ->
      case Full.Eval.dataize t of
        Err t2 ->
          "ERROR: failed to dataize term" ++
          "\n  " ++
          "\n  " ++ Full.Pretty.ppTerm pp t ++
          "\n" ++
          "\ncould not reduce further than its weak head normal form:" ++
          "\n  " ++
          "\n  " ++ Full.Pretty.ppTerm pp t2
        Ok result ->
          "SUCCESS: dataization of the following term" ++
          "\n  " ++
          "\n  " ++ Full.Pretty.ppTerm pp t ++
          "\n  " ++
          "\nhas been successfully dataized to value" ++
          "\n  " ++
          "\n  " ++ pp result

pp : Result String Int -> String
pp r =
  case r of
    Err err -> "ERROR: " ++ err
    Ok n    -> String.fromInt n
