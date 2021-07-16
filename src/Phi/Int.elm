module Phi.Int exposing (..)

import Phi.Syntax exposing (..)
import Dict
import Phi.Eval exposing (dataizeWith)

mkInt : Int -> DefaultTerm
mkInt n = Object (Dict.fromList
  [ ("δ", Data (Ok n))
  , ("inc", (Object (Dict.fromList [("𝜑", Atom "num_inc" Nothing [] atom_num_inc)])))
  ])

mkAtom0 : (Int -> Int) -> List DefaultObject -> DefaultObject
mkAtom0 f parents =
  case parents of
    _ :: rho :: _ ->
      case dataizeWith parents (Object rho) of
        Ok (Ok n) -> Dict.insert "δ" (Data (Ok (f n))) rho
        _ -> mkErrObject "atom used outside of a proper parent object"
    _ -> mkErrObject "atom used outside of a proper parent object"

atom_num_inc : List DefaultObject -> DefaultObject
atom_num_inc = mkAtom0 (\x -> x + 1)

mkErrObject : String -> DefaultObject
mkErrObject err = Dict.fromList [("δ", Data (Err err))]
