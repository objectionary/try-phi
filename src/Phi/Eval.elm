module Phi.Eval exposing (..)

import Phi.Syntax exposing (..)
import Dict exposing (Dict)

type alias Substitution d = Dict Locator (Term d)

substitute : Substitution d -> Term d -> Term d
substitute substs t =
  case t of
    Var x ->
      case Dict.get x substs of
        Just s  -> s
        Nothing -> Var x
    Dot f a ->
      Dot (substitute substs f) a
    App f (a, s) ->
      App (substitute substs f) (a, substitute substs s)
    Object o ->
      let shadowed = "ρ" :: Dict.keys o
          xiToRho =
            case Dict.get "ξ" substs of
              Just parent -> Dict.insert "ρ" parent << Dict.remove "ξ"
              Nothing     -> identity
          update = xiToRho << Dict.filter (\k _ -> not (List.member k shadowed))
       in Object (Dict.map (\_ -> substitute (update substs)) o)
    FreeAttr -> t
    Data _ -> t
    Atom _ _ _ _ -> t

-- | Reduce a term to WHNF (weak head normal form).
whnf : Term d -> Term d
whnf = whnfWith []

objectToSubstitution : Object d -> Substitution d
objectToSubstitution t = Dict.fromList (("ξ", Object t) ::
  List.map (\a -> (a, Dot (Object t) a)) (Dict.keys t))

-- | Reduce a term to WHNF (weak head normal form) given current stack of ancestor objects.
whnfWith : List (Object d) -> Term d -> Term d
whnfWith parents t =
  case t of
    App u (a, v) ->
      case whnfWith parents u of
        Object u2 ->
          case Dict.get a u2 of
            Just FreeAttr -> Object (Dict.insert a v u2)
            Nothing       -> App (Object u2) (a, v) -- error ("attribute " ++ a ++ " is missing!")
            Just _        -> App (Object u2) (a, v) -- error ("attribute " ++ a ++ " is not free!")
        u2 -> App u2 (a, v)

    Object o -> Object o

    Dot u a ->
      case whnfWith parents u of
        Object o ->
          case Dict.get a o of
            Just v -> whnfWith (o::parents) (substitute (objectToSubstitution o) (addParentToAtoms o v))
            Nothing ->
              case Dict.get "𝜑" o of
                Just v -> whnfWith (o::parents) (Dot (substitute (objectToSubstitution o) (addParentToAtoms o v)) a)
                Nothing -> Dot (Object o) a -- error ("attribute " ++ a ++ " not found in an object")
        u2 -> Dot u2 a

    FreeAttr -> t
    Data _ -> t
    Var _ -> t
    Atom _ mlvl outers atom ->
      -- FIXME: make atoms less hacky
      -- Here we take only lvl parents since later parents have been fixed for this atom as outers.
      case mlvl of
        Nothing  -> Object (atom parents)
        Just lvl -> Object (atom (List.take lvl parents ++ outers))

-- | Instantiate one of the ancestors of each atom in the given term.
addParentToAtoms : Object d -> Term d -> Term d
addParentToAtoms o =
  let go i tt =
        case tt of
          App t (a, s) -> App (go i t) (a, go i s)
          Object obj -> Object (Dict.map (\_ -> go (i + 1)) obj)
          Dot t a -> Dot (go i t) a
          FreeAttr -> tt
          Data _ -> tt
          Var _ -> tt
          Atom name mlvl outers atom ->
            case mlvl of
              Nothing -> Atom name (Just i) (o::outers) atom
              Just lvl ->
                if (i == lvl - 1)
                  then Atom name (Just i) (o::outers) atom
                  else if (i >= lvl)
                    then Atom name mlvl outers atom
                    else tt -- error ("impossible: i=" ++ String.fromInt i ++ " lvl=" ++ String.fromInt lvl)
  in go 0


dataize : Term d -> Result (Term d) d
dataize = dataizeWith []

dataizeWith : List (Object d) -> Term d -> Result (Term d) d
dataizeWith parents t =
  case whnfWith parents (Dot t "δ") of
    Data d -> Ok d
    s      -> Err s
