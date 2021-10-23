module Minimal.Examples exposing (..)

import Minimal.Syntax exposing (Term(..))
import Minimal.Parser exposing (parse)
import Html exposing (div, p, pre, text, b)
import Full.Examples exposing (ex3)
-- Examples from paper

getTerm : String -> Term 
getTerm s =
    case parse s of
        Ok term -> term
        _ -> Locator 0

-- lines 6-10

-- [
--     book3 ->[
--         isbn-> ?,
--         title->^.ObjectThinking,
--         price->^.memory,
--         setprice->[
--             p->?,
--             @->^.price.write(t->$.p)
--         ]
--     ]
-- ]

ex1Raw : String
ex1Raw = "[\n    book3 ->[\n        isbn-> ?,\n        title->^.ObjectThinking,\n        price->^.memory,\n        setprice->[\n            p->?,\n            @->^.price.write(t->$.p)\n        ]\n    ]\n]"
ex1 : Term
ex1 = getTerm ex1Raw


-- lines 67-73

-- [
--     point -> [
--         x -> ?,
--         y -> ?,
--         distance -> [
--             to -> ?,
--             len -> ^.^.^.vector
--                 (arg1 -> $.to.x.sub(arg->^.x))
--                 (arg2 -> $.to.x.sub(arg->^.y))
--                 .length
--         ]
--     ]
-- ]
ex2Raw : String
ex2Raw = "[\n    point -> [\n        x -> ?,\n        y -> ?,\n        distance -> [\n            to -> ?,\n            len -> ^.^.^.vector\n                (arg1 -> $.to.x.sub(arg->^.x))\n                (arg2 -> $.to.x.sub(arg->^.y))\n                .length\n        ]\n    ]\n]"
ex2 : Term
ex2 = getTerm ex2Raw


-- example 14

-- [
--     c -> [
--         center -> ^.point
--             (arg1 -> ^.negative3)
--             (arg1 -> ^.positive9),
--         radius -> ^.positive40,
--         @ -> $.center,
--         is_inside -> [
--             p -> ?,
--             @ -> ^.distance(arg -> $.p).leq(arg -> $.p.radius)
--         ]
--     ]
-- ]
ex3Raw : String
ex3Raw = "[\n    c -> [\n        center -> ^.point\n            (arg1 -> ^.negative3)\n            (arg1 -> ^.positive9),\n        radius -> ^.positive40,\n        @ -> $.center,\n        is_inside -> [\n            p -> ?,\n            @ -> ^.distance(arg -> $.p).leq(arg -> $.p.radius)\n        ]\n    ]\n]"

ex3 : Term
ex3 = getTerm ex3Raw

type alias Example =
  { raw : String
  , term : Term
  , description : String
  }

examples : List Example
examples =
  [ { description = "book: page 3, lines 6-10"
    , raw = ex1Raw
    , term = ex1
    }
  , { description = "point: page 5, lines 67-73"
    , raw = ex2Raw
    , term = ex2
    }
  , { description = "circle: page 9, example 15"
    , raw = ex3Raw
    , term = ex3
    }
  ]