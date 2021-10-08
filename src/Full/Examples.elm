module Full.Examples exposing (..)

import Full.Syntax exposing (..)
import Full.Int exposing (mkInt)
import Dict exposing (Dict)

type alias Example =
  { raw : String
  , term : DefaultTerm
  , description : String
  }

ex1 : DefaultTerm
ex1 = Object (Dict.fromList [("x", FreeAttr), ("y", Var "x")])

ex2 : DefaultTerm
ex2 = Dot ex1 "y"

ex5 : DefaultTerm
ex5 = Dot (App ex1 ("x", mkInt 1)) "y"

ex3 : DefaultTerm
ex3 = Dot (Object (Dict.fromList [("𝜑", Var "x"), ("x", Var "y"), ("y", Var "z")])) "k"

ex4 : DefaultTerm
ex4 = Dot (Dot (Object (Dict.fromList [("x", Object (Dict.fromList [("y", Dot (Var "ρ") "z")])), ("z", Dot (Var "ξ") "w"), ("w", Var "u")])) "x") "y"

examples : List Example
examples =
  [ { description = "Simple object"
    , raw = "[x -> ?, y -> x]"
    , term = ex1
    }
  , { description = "Simple attribute access"
    , raw = "[x -> ?, y -> x].y"
    , term = ex2
    }
  , { description = "Simple application"
    , raw = "[x -> ?, y -> x](x -> 1).y"
    , term = ex5
    }
  , { description = "Using decoration (𝜑) with dot"
    , raw = "[@ -> x, x -> y, y -> z].k"
    , term = ex3
    }
  , { description = "Using parent and current object locators"
    , raw = "[x -> [y -> ^.z], z -> $.w, w -> u].x.y"
    , term = ex4
    }
  ]
