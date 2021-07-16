module Phi.Examples exposing (..)

import Phi.Syntax exposing (..)
import Dict exposing (Dict)

ex1 : Term Int
ex1 = Dot (Object (Dict.fromList [("x", FreeAttr), ("y", Var "x")])) "y"
