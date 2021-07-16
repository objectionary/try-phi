module Phi.Int exposing (..)

import Phi.Syntax exposing (..)
import Dict
import Phi.Eval exposing (dataizeWith, whnfWith)

mkInt : Int -> DefaultTerm
mkInt = Object << mkIntObject

mkIntObject : Int -> DefaultObject
mkIntObject n = Dict.fromList
  [ ("δ", Data (Ok n))
  , ("isZero", Object (Dict.fromList [("𝜑", Atom "num_isZero" Nothing [] atom_num_isZero)]))
  , ("less", Object (Dict.fromList [("𝜑", Atom "num_less" Nothing [] atom_num_less), ("_1", FreeAttr)]))
  , ("inc", Object (Dict.fromList [("𝜑", Atom "num_inc" Nothing [] atom_num_inc)]))
  , ("add", Object (Dict.fromList [("𝜑", Atom "num_add" Nothing [] atom_num_add), ("_1", FreeAttr)]))
  , ("sub", Object (Dict.fromList [("𝜑", Atom "num_sub" Nothing [] atom_num_sub), ("_1", FreeAttr)]))
  , ("mul", Object (Dict.fromList [("𝜑", Atom "num_mul" Nothing [] atom_num_mul), ("_1", FreeAttr)]))
  , ("div", Object (Dict.fromList [("𝜑", Atom "num_div" Nothing [] atom_num_div), ("_1", FreeAttr)]))
  ]

mkAtom0 : (Int -> Int) -> List DefaultObject -> DefaultObject
mkAtom0 f parents =
  case parents of
    _ :: rho :: _ ->
      case dataizeWith parents (Object rho) of
        Ok (Ok n) -> mkIntObject (f n)
        Ok (Err err) -> mkErrObject err
        _ -> mkErrObject "atom used outside of a proper parent object"
    _ -> mkErrObject "atom used outside of a proper parent object"

mkAtom1 : (Int -> Int -> Int) -> List DefaultObject -> DefaultObject
mkAtom1 f parents =
  case parents of
    xi :: rho :: _ ->
      case dataizeWith parents (Object rho) of
        Ok (Ok n) ->
          case dataizeWith parents (Dot (Object xi) "_1") of
            Ok (Ok m) -> mkIntObject (f n m)
            Ok (Err err) -> mkErrObject err
            _ -> mkErrObject "atom used outside of a proper parent object"
        Ok (Err err) -> mkErrObject err
        _ -> mkErrObject "atom used outside of a proper parent object"
    _ -> mkErrObject "atom used outside of a proper parent object"

mkPredicate0 : (Int -> Bool) -> List DefaultObject -> DefaultObject
mkPredicate0 f parents =
  case parents of
    _ :: rho :: _ ->
      case dataizeWith parents (Object rho) of
        Ok (Ok n) -> mkBoolObject (f n)
        Ok (Err err) -> mkErrObject err
        _ -> mkErrObject "atom used outside of a proper parent object"
    _ -> mkErrObject "atom used outside of a proper parent object"

mkPredicate1 : (Int -> Int -> Bool) -> List DefaultObject -> DefaultObject
mkPredicate1 f parents =
  case parents of
    xi :: rho :: _ ->
      case dataizeWith parents (Object rho) of
        Ok (Ok n) ->
          case dataizeWith parents (Dot (Object xi) "_1") of
            Ok (Ok m) -> mkBoolObject (f n m)
            Ok (Err err) -> mkErrObject err
            _ -> mkErrObject "atom used outside of a proper parent object"
        Ok (Err err) -> mkErrObject err
        _ -> mkErrObject "atom used outside of a proper parent object"
    _ -> mkErrObject "atom used outside of a proper parent object"

atom_num_inc : List DefaultObject -> DefaultObject
atom_num_inc = mkAtom0 (\x -> x + 1)

atom_num_add : List DefaultObject -> DefaultObject
atom_num_add = mkAtom1 (\x y -> x + y)

atom_num_mul : List DefaultObject -> DefaultObject
atom_num_mul = mkAtom1 (\x y -> x * y)

atom_num_sub : List DefaultObject -> DefaultObject
atom_num_sub = mkAtom1 (\x y -> x - y)

atom_num_div : List DefaultObject -> DefaultObject
atom_num_div = mkAtom1 (\x y -> x // y)

atom_num_isZero : List DefaultObject -> DefaultObject
atom_num_isZero = mkPredicate0 (\x -> x == 0)

atom_num_less : List DefaultObject -> DefaultObject
atom_num_less = mkPredicate1 (\x y -> x < y)

mkBool : Bool -> DefaultTerm
mkBool = Object << mkBoolObject

mkBoolObject : Bool -> DefaultObject
mkBoolObject b = Dict.fromList
  [ ("δ", Data (Ok (if b then 1 else 0)))
  , ("not", Object (Dict.fromList [("𝜑", Atom "bool_not" Nothing [] atom_bool_not)]))
  , ("and", Object (Dict.fromList [("𝜑", Atom "bool_and" Nothing [] atom_bool_and), ("_1", FreeAttr)]))
  , ("or", Object (Dict.fromList [("𝜑", Atom "bool_or" Nothing [] atom_bool_or), ("_1", FreeAttr)]))
  , ("xor", Object (Dict.fromList [("𝜑", Atom "bool_xor" Nothing [] atom_bool_xor), ("_1", FreeAttr)]))
  , ("if", Object (Dict.fromList [("𝜑", Atom "bool_if" Nothing [] atom_bool_if), ("_1", FreeAttr), ("_2", FreeAttr)]))
  ]

atom_bool_not : List DefaultObject -> DefaultObject
atom_bool_not = mkPredicate0 (\x -> x == 0)

atom_bool_and : List DefaultObject -> DefaultObject
atom_bool_and = mkPredicate1 (\x y -> intToBool x && intToBool y)

atom_bool_or : List DefaultObject -> DefaultObject
atom_bool_or = mkPredicate1 (\x y -> intToBool x || intToBool y)

atom_bool_xor : List DefaultObject -> DefaultObject
atom_bool_xor = mkPredicate1 (\x y -> intToBool x /= intToBool y)

atom_bool_if : List DefaultObject -> DefaultObject
atom_bool_if parents =
  case parents of
    xi :: rho :: _ ->
      case dataizeWith parents (Object rho) of
        Ok (Ok n) ->
          case Dict.get "_1" xi of
            Nothing -> mkErrObject "atom_bool_if expects arguments _1 and _2, but _1 is missing"
            Just ifTrue ->
              case Dict.get "_2" xi of
                Nothing -> mkErrObject "atom_bool_if expects arguments _1 and _2, but _2 is missing"
                Just ifFalse ->
                  if intToBool n
                    then Dict.fromList [("𝜑", ifTrue)]
                    else Dict.fromList [("𝜑", ifFalse)]
        Ok (Err err) -> mkErrObject err
        _ -> mkErrObject "atom used outside of a proper parent object"
    _ -> mkErrObject "atom used outside of a proper parent object"

intToBool : Int -> Bool
intToBool x = x /= 0

mkErrObject : String -> DefaultObject
mkErrObject err = Dict.fromList [("δ", Data (Err err))]
